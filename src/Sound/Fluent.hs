
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}

module Sound.Fluent where

import qualified Sound.OSC           as OSC
import qualified Sound.OSC.Transport.FD
import Sound.OSC (OSC, Datum)
import qualified Sound.PortAudio                  as PA
import qualified Sound.PortAudio.Base             as PAB
import qualified System.Random                    as R
import qualified Data.Vector.Storable             as V
import qualified Sound.File.Sndfile               as SF
import qualified Sound.File.Sndfile.Buffer        as BSF
import qualified Sound.File.Sndfile.Buffer.Vector as VSF

-- import Control.Lens


import Data.List (isPrefixOf)
import           Data.Monoid
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Monad                    (foldM, foldM_, forM_)
import           Text.Printf
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Data.Text (Text)
import qualified Data.Text as T

import           System.Environment               (getArgs)

-- Only use mono 44100 files for a start!
{-
  Before audio starts, we have a list of clips
  During DSP we might get an instruction to stat playing a clip
    This becomes a generator which can be stopped (and possibly changed)

-}
data Span = Span !Time !Time
  deriving (Eq, Ord, Show)
inside t (Span a b) = a <= t && t < b
onset (Span a b) = a
offset (Span a b) = b
duration s = offset s - onset s
delay t (Span a b) = Span (t + a) (t + b)
startAt t s = delay (t - onset s) s

data Clip =
  Clip
    !Text      -- unique clip id
    !Text      -- source file
    !Span      -- part of file
clipName (Clip n sf sp) = n
data Gen =
  Gen
    !Text      -- unique gen id
    !Clip      -- clip to play
    !Time      -- time generator was created

-- Span in which generator is alive
genSpan :: Gen -> Span
genSpan (Gen _ (Clip _ _ s) t) = startAt t s

genIsAlive :: Time -> Gen -> Bool
genIsAlive t s = t `inside` genSpan s

-- Processed samples
type Time = Int

-- All global state
data Fluent = Fluent {
  -- These don't need to be vars...
  -- preloadBuffers only access clips and buffers
  -- DSP thread access buffers, gens and time
  _fluentClips   :: (TVar (Map Text Clip)),
  _fluentBuffers :: (TVar (Map Text (V.Vector Float))),
  _fluentGens    :: (TVar (Map Text Gen)),
  _fluentTime    :: (TVar Time)
  }


removeFinishedGens :: Time -> Fluent -> IO ()
removeFinishedGens t fluent =
  atomically $ modifyTVar (_fluentGens fluent) (Map.filter (genIsAlive t))

removeGenNamed :: Text -> Fluent -> IO ()
removeGenNamed n fluent =
  atomically $ modifyTVar (_fluentGens fluent) (Map.delete n)

haveClip :: Clip -> Fluent -> IO Bool
haveClip clip fluent =
  atomically $ fmap (Map.member $ clipName clip) $ readTVar (_fluentClips fluent)

-- Set up buffer arrays
preloadBuffers :: [Clip] -> Fluent -> IO ()
preloadBuffers c fluent = do
  -- TODO just an example
  -- Really, load all the clips and add to table
  let inFile = "test.wav"
  (info, Just (x :: VSF.Buffer Float)) <- SF.readFile inFile

  -- putStrLn $ "sample rate: " ++ (show $ SF.samplerate info)
  -- putStrLn $ "channels: "    ++ (show $ SF.channels info)
  -- putStrLn $ "frames: "      ++ (show $ SF.frames info)
  let vecData = VSF.fromBuffer x
  atomically $ writeTVar (_fluentBuffers fluent) (Map.singleton "test" vecData)
  return ()

-- Add generator
startPlayingClip
  :: Clip   -- ^ clip to play
  -> Text -- ^ id (for stop)
  -> Fluent
  -> IO ()
startPlayingClip clip genId fluent = do
  ok <- haveClip clip fluent
  if not ok then
    putStrLn $ "Unknown clip: " ++ T.unpack (clipName clip)
    else do
      time <- atomically $ readTVar (_fluentTime fluent)
      let gen = Gen genId clip time
      return ()
  -- TODO add to table
  return ()

-- Remove generator
stopPlayingClip
  :: Text   -- ^ id (for stop)
  -> Fluent
  -> IO ()
stopPlayingClip = removeGenNamed


initAudio :: Fluent -> IO (PA.Stream CFloat CFloat)
initAudio fluent = do
  paRes <- PA.initialize
  case paRes of
    Just e  -> fail $ show e
    Nothing -> return ()

  str <- PA.openDefaultStream
    0 -- inputs
    2 -- outputs
    44100
    (Just (1024)) -- TODO 'Nothing' is more efficient
    (Just $ dspCallback fluent)
    (Just $ putStrLn "DSP done") -- when done
  str2 <- case str of
    Left _ -> fail "Could not open stream"
    Right s -> return s
  threadDelay (1000000*1) -- TODO
  PA.startStream str2
  return str2

  where

    dspCallback :: Fluent -> PA.StreamCallback CFloat CFloat
    dspCallback fluent __timing__ __flags__ frames __inpPtr__ outPtr = do
      -- TODO assume 2 channels
      let channels = 2
      t       <- atomically $ readTVar (_fluentTime fluent)
      buffers <- atomically $ readTVar (_fluentBuffers fluent)
      removeFinishedGens t fluent

      forM_ ["test"] $ \bufferName -> do
        let b = Map.lookup bufferName buffers
        case b of
          Nothing -> return () -- TODO missing buffer, report somewhere
          Just b -> do
            forM_ [0..channels-1] $ \c ->
              forM_ [0..frames-1] $ \f -> do
                let v = 0
                -- let v = (realToFrac f / realToFrac frames *0.1)
                let v = (V.!) b (t + fromIntegral f)
                pokeElemOff outPtr (fromIntegral $ f*channels+c) (realToFrac v)

      atomically $ modifyTVar (_fluentTime fluent) (\t -> t + fromIntegral frames)
      return PA.Continue

killAudio :: Fluent -> PA.Stream CFloat CFloat -> IO ()
killAudio fluent str = do
  PA.stopStream str
  threadDelay (1000000*1) -- TODO
  PA.closeStream str
  threadDelay (1000000*1) -- TODO
  paRes <- PA.terminate
  case paRes of
    Just e  -> fail $ show e
    Nothing -> return ()
  return ()

waitForOsc :: (OSC.Message -> IO ()) -> IO ()
waitForOsc handler = do
  let port = 54321
  putStrLn $ "Listening on port " ++ show 54321
  let t = OSC.udpServer "127.0.0.1" port
  Sound.OSC.Transport.FD.withTransport t $ \t -> void $ OSC.untilPredicate id $ do
    msgs <- Sound.OSC.Transport.FD.recvMessages t
    mapM_ handler msgs
    return $ any isQuitMessage msgs
  return ()
  where
    isQuitMessage :: OSC.Message -> Bool
    isQuitMessage m = not $ "/fluent/quit" `isPrefixOf` OSC.messageAddress m

runFluent = do
  putStrLn "Welcome to fluent!"
  c <- atomically $ newTVar mempty
  b <- atomically $ newTVar mempty
  g <- atomically $ newTVar mempty
  t <- atomically $ newTVar 0
  let fluent = Fluent c b g t
  preloadBuffers [] fluent
  str2 <- initAudio fluent
  
  -- do
  --   threadDelay (1000*1000*2) -- TODO
  --   threadDelay (1000*1000*8) -- TODO
  -- DEBUG
  startPlayingClip (Clip "test" "test.wav" (Span 0 44100)) "gen1" fluent
  
  waitForOsc print
  
  killAudio fluent str2
  putStrLn "Goodbye from fluent!"
{-
Setup file:
  [
    ("vln1", "/audio/violins/i.wav", (0, 100)),
    ...
  ]

OSC protocol (currently ignores time):
  /fluent/play "note1" "vln1"
  /fluent/play "note2" "vln1"
  /fluent/play "note1 vln1" -- equivalent to the above
  /fluent/play "note2 vln1"
  /fluent/stop "note1"
  /fluent/stop "note2"
  /fluent/status
  /fluent/quit
-}
