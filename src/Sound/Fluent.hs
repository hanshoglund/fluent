
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}

module Sound.Fluent where

import qualified Data.ByteString.Char8 as BS
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
  deriving (Show)

clipName (Clip n sf sp) = n
clipSourceFile (Clip n sf sp) = sf
clipSpan (Clip n sf sp) = sp

data Gen =
  Gen
    !Text      -- unique gen id
    !Clip      -- clip to play
    !Time      -- time generator was created
  deriving (Show)

genName (Gen n cl started) = n
genClip (Gen n cl started) = cl
genStarted (Gen n cl started) = started

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
preloadBuffers clips fluent =
  forM_ clips $ \clip -> do
    let inFile = T.unpack $ clipSourceFile clip
    putStrLn $ "Loading clip " ++ T.unpack (clipName clip) ++ "\t\t " ++ inFile ++ "..."
    (info, Just (x :: VSF.Buffer Float)) <- SF.readFile inFile
    -- putStrLn $ "sample rate: " ++ (show $ SF.samplerate info)
    -- putStrLn $ "channels: "    ++ (show $ SF.channels info)
    -- putStrLn $ "frames: "      ++ (show $ SF.frames info)
    let vecData = VSF.fromBuffer x
    atomically $ modifyTVar (_fluentClips fluent) (Map.insert (clipName clip) clip)
    atomically $ modifyTVar (_fluentBuffers fluent) (Map.insert (clipName clip) vecData)

-- Add generator
startPlayingClip
  :: Clip   -- ^ clip to play
  -> Text -- ^ id (for stop)
  -> Fluent
  -> IO ()
startPlayingClip clip genId fluent = do
  ok <- haveClip clip fluent
  if not ok then
    putStrLn $ "Error: Unknown clip: " ++ T.unpack (clipName clip)
    else do
      time <- atomically $ readTVar (_fluentTime fluent)
      let gen = Gen genId clip time
      atomically $ modifyTVar (_fluentGens fluent) (Map.insert genId gen)
      return ()
  return ()
  
startPlayingClipNamed
  :: Text -- ^ clip to play
  -> Text -- ^ id (for stop)
  -> Fluent
  -> IO ()
startPlayingClipNamed clipId genId fluent = do
  clips <- atomically $ readTVar (_fluentClips fluent)
  case Map.lookup clipId clips of
    Nothing -> putStrLn $ "Error: Unknown clip: " ++ T.unpack clipId
    Just c  -> startPlayingClip c genId fluent -- TODO unnecessary double check
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
    kSAMPLE_RATE
    (Just (kVECSIZE)) -- TODO 'Nothing' is more efficient
    (Just $ dspCallback fluent)
    (Just $ putStrLn "Audio thread finished alright") -- when done
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
      gens    <- atomically $ readTVar (_fluentGens fluent)
      -- print gens

      -- Zero all channels
      forM_ [0..channels-1] $ \c ->
        forM_ [0..frames-1] $ \f -> do
          pokeElemOff outPtr (fromIntegral $ f*channels+c) 0

      -- Run all generators one by one
      forM_ (fmap snd $ Map.toList gens) $ \gen -> do
        let clip = genClip gen
        let bufferName = clipName clip
        let b = Map.lookup bufferName buffers
        case b of
          Nothing -> return () -- TODO missing buffer, report somewhere
          Just b -> do
            forM_ [0..channels-1] $ \c ->
              forM_ [0..frames-1] $ \f -> do
                -- Index to read: current time offset + current frame + offset in buffer
                let preciseGlobalTime = t + fromIntegral f
                -- If we started later, read an earlier position in the buffer
                let preciseLocalTime = preciseGlobalTime - genStarted gen
                let v = (V.!) b (preciseLocalTime + onset (clipSpan clip))
                
                -- Add v to index (so simultanous generators are summed)
                oldV <- peekElemOff outPtr (fromIntegral $ f*channels+c)
                pokeElemOff outPtr (fromIntegral $ f*channels+c) (realToFrac v + oldV)

      atomically $ modifyTVar (_fluentTime fluent) (\t -> t + fromIntegral frames)
      removeFinishedGens t fluent
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
  putStrLn $ "Listening for OSC messages on port " ++ show 54321
  let t = OSC.udpServer "127.0.0.1" port
  Sound.OSC.Transport.FD.withTransport t $ \t -> void $ OSC.untilPredicate not {-should be not-} $ do
    msgs <- Sound.OSC.Transport.FD.recvMessages t
    mapM_ handler msgs
    return $ any isQuitMessage msgs
  return ()
  where

isQuitMessage :: OSC.Message -> Bool
isQuitMessage m = "/fluent/quit" `isPrefixOf` OSC.messageAddress m
               
composeHandlers = foldr composeHandlers2 (\x -> return ())

composeHandlers2 :: (OSC.Message -> IO ()) -> (OSC.Message -> IO ()) -> OSC.Message -> IO ()
composeHandlers2 f g m = do
  f m
  g m

statusHandler fluent m
  | "/fluent/status" `isPrefixOf` OSC.messageAddress m = putStrLn "Fluent is alright"
  | otherwise = return ()
startHandler fluent m
  | "/fluent/play" `isPrefixOf` OSC.messageAddress m = startHandler' fluent m
  | otherwise = return ()
startHandler' fluent  (OSC.Message _ [OSC.ASCII_String genId, OSC.ASCII_String clipId])
  = startPlayingClipNamed (bs2t clipId) (bs2t genId) fluent  >> return ()
startHandler' fluent  (OSC.Message _ [OSC.ASCII_String genAndClipId])
  = 
    let (genId : clipId : _) = BS.words genAndClipId in
    -- TODO crashes on bad msg
    startPlayingClipNamed (bs2t clipId) (bs2t genId) fluent  >> return ()
startHandler' fluent _ = putStrLn "Error: Bad message"
stopHandler fluent m
  | "/fluent/stop" `isPrefixOf` OSC.messageAddress m = stopHandler' fluent m
  | otherwise = return ()
stopHandler' fluent  (OSC.Message _ [OSC.ASCII_String genId])
  = stopPlayingClip (bs2t genId) fluent  >> return ()
stopHandler' fluent _ = putStrLn "Error: Bad message"

bs2t = T.pack . BS.unpack

runFluent = do
  putStrLn "Fluent says hello!"
  c <- atomically $ newTVar mempty
  b <- atomically $ newTVar mempty
  g <- atomically $ newTVar mempty
  t <- atomically $ newTVar 0
  let fluent = Fluent c b g t

  setup <- readFile "setupFluent.hs"
  
  -- TODO preloadClipNamed
  preloadBuffers (setupFileTextToClips setup) fluent

  str2 <- initAudio fluent
  
  -- do
  --   stopPlayingClip "nonexistant" fluent
  --   -- threadDelay (1000*500) -- TODO
  --   startPlayingClipNamed "test" "gen1" fluent
  --   threadDelay (1000*2000) -- TODO
  --   startPlayingClipNamed "test" "gen2" fluent
  --   threadDelay (1000*2000) -- TODO
  --   startPlayingClipNamed "foo" "gen3" fluent
  --   threadDelay (1000*2000) -- TODO
  --   startPlayingClipNamed "bar" "gen3" fluent
  --   -- threadDelay (1000*6000) -- TODO
  --   stopPlayingClip "gen1" fluent
  --   -- threadDelay (1000*500) -- TODO
  --   stopPlayingClip "gen2" fluent
  -- -- TODO DEBUG
  
  waitForOsc $ composeHandlers [statusHandler fluent, startHandler fluent, stopHandler fluent]
  
  killAudio fluent str2
  putStrLn "Fluent says goodbye!"

-- kTESTCLIPS = 
--   [ Clip "test" "test.wav" (Span (4410*95) (4410*100))
--   , Clip "foo" "test.wav" (Span (4410*20) (4410*100))
--   , Clip "bar" "test.wav" (Span (4410*30) (4410*100))
--   ]

setupFileTextToClips :: String -> [Clip]
setupFileTextToClips = setupFileDataToClips . read

setupFileDataToClips :: [(String, String, (Double, Double))] -> [Clip]
setupFileDataToClips = map toClip
  where
    toClip (n,f,(on,off)) = Clip (T.pack n) (T.pack f) (Span (floor $ on*kSAMPLE_RATE) (floor $ off*kSAMPLE_RATE))

kSAMPLE_RATE = 44100  
kVECSIZE = 128
-- TODO infer duration from sound files
  
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
