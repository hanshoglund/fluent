
{-# LANGUAGE TemplateHaskell #-}

module Sound.Fluent where

import Sound.PortAudio.Base
import qualified Sound.PortAudio as PA
import qualified System.Random as R

-- import Control.Lens

import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer as BSF
import qualified Sound.File.Sndfile.Buffer.Vector as VSF
import qualified Data.Vector.Storable as V

import Control.Monad (foldM, foldM_, forM_)
import Control.Concurrent.MVar
import Control.Concurrent
import Text.Printf

import Foreign.C.Types
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr

import System.Environment (getArgs)

-- Only use mono 44100 files for a start!
data AudioFile = 
  AudioFile
    FilePath
-- TODO lenses
data Clip =
  Clip
    AudioFile -- source
    Int       -- start index
    Int       -- num samples
    Bool      -- loop?
-- TODO lenses
data Gen =
  Gen ()

-- All global state
data Fluent = Fluent

-- Set up buffer arrays
preloadBuffers :: [Clip] -> Fluent -> IO ()    
preloadBuffers c f = do
  -- TODO
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


audioCallback :: PA.StreamCallback CFloat CFloat
audioCallback _timing _flags frames inpPtr outPtr = do
  -- putStrLn $ show frames
  
  let channels = 2 -- TODO

  forM_ [0..channels-1] $ \c ->
    forM_ [0..frames-1] $ \f ->
      -- Grab the right chunk of some buffer
      pokeElemOff outPtr (fromIntegral $ f*channels+c) (realToFrac f / realToFrac frames *0.1)
  
  -- TODO process audio
  return PA.Continue


initAudio :: Fluent -> IO (PA.Stream CFloat CFloat)
initAudio fluent = do
  PA.initialize
  -- TODO handle error
  str <- PA.openDefaultStream
    0 -- inputs
    2 -- outputs
    44100
    (Just 64) -- TODO Nothing is more efficient
    (Just audioCallback)
    (Just $ putStrLn "DSP done") -- when done
  str2 <- case str of
    Left _ -> fail "Could not open stream"
    Right s -> return s
  threadDelay (1000000*1) -- TODO
  PA.startStream str2
  return str2

killAudio :: Fluent -> PA.Stream CFloat CFloat -> IO ()
killAudio fluent str = do
  PA.stopStream str
  threadDelay (1000000*1) -- TODO
  -- TODO cleanup segfaults, why?
  -- PA.closeStream str
  -- threadDelay (1000000*1) -- TODO
  -- PA.terminate
  -- TODO handle error
  return ()


runFluent = do
  putStrLn "Welcome to fluent!"
  preloadBuffers [] undefined
  str2 <- initAudio undefined
  threadDelay (1000000*3) -- TODO
  killAudio undefined str2
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