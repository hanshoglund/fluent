
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}

module Sound.Fluent where

import qualified Sound.PortAudio                  as PA
import qualified Sound.PortAudio.Base             as PAB
import qualified System.Random                    as R
import qualified Data.Vector.Storable             as V
import qualified Sound.File.Sndfile               as SF
import qualified Sound.File.Sndfile.Buffer        as BSF
import qualified Sound.File.Sndfile.Buffer.Vector as VSF

-- import Control.Lens


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
data AudioFile =
  AudioFile
    FilePath
data Clip =
  Clip
    AudioFile -- source
    Int       -- start index
    Int       -- num samples
    Bool      -- loop?
data Gen =
  Gen ()

-- Processed samples
type Time = Int

-- All global state
data Fluent = Fluent {
  _fluentBuffers :: (TVar (Map Text (V.Vector Float))),
  _fluentTime :: (TVar Time)
  }

-- Set up buffer arrays
preloadBuffers :: [Clip] -> Fluent -> IO ()
preloadBuffers c fluent = do
  -- TODO just an example
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
  -> String -- ^ id (for stop)
  -> Fluent
  -> IO ()
startPlayingClip = undefined

-- Remove generator
stopPlayingClip
  :: Clip   -- ^ clip to play
  -> String -- ^ id (for stop)
  -> Fluent
  -> IO ()
stopPlayingClip = undefined


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
    (Just (44100*5)) -- TODO 'Nothing' is more efficient
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
    dspCallback fluent _timing _flags frames inpPtr outPtr = do
      -- TODO assume 2 channels
      let channels = 2
      forM_ [0..channels-1] $ \c ->
        forM_ [0..frames-1] $ \f -> do
          let v = 0
          -- let v = (realToFrac f / realToFrac frames *0.1)
          b <- fmap (unsafeLookup "test") $ atomically $ readTVar (_fluentBuffers fluent)
          let v = (V.!) b (fromIntegral f)
          pokeElemOff outPtr (fromIntegral $ f*channels+c) (realToFrac v)
          atomically $ modifyTVar (_fluentTime fluent) (\t -> t + fromIntegral frames)
      return PA.Continue
      where
        unsafeLookup k m = case Map.lookup k m of
          Nothing -> error "No such key"
          Just x -> x    

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


runFluent = do
  putStrLn "Welcome to fluent!"
  b <- atomically $ newTVar undefined -- TODO must be replaced by preloadBuffers
  t <- atomically $ newTVar 0
  let fluent = Fluent b t
  preloadBuffers [] fluent
  str2 <- initAudio fluent
  threadDelay (1000000*10) -- TODO
  killAudio fluent str2
  putStrLn "Goodbye from fluent!"
{-
Operation:
  Start up
  Preload buffers according to some spec
  Begin waiting for OSC
    When OSC received
    Create generator
    Each generator is a (Time -> State -> (Ampl, State)) function where
      Time is relative to the creation of the generator
      State is the state of all buffers in the system
    In the audio thread, each bloc:
      Read MVar containing compiled generators (actual time, their creation time)
      Read MVar containing buffers
-}
