
module Sound.Fluent where

import Sound.PortAudio.Base
import Sound.PortAudio

import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer as BSF
import qualified Sound.File.Sndfile.Buffer.Vector as VSF
import qualified Data.Vector.Storable as V

import Control.Monad (foldM, foldM_, forM_)
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Text.Printf

import Foreign.C.Types
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr

import System.Environment (getArgs)

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