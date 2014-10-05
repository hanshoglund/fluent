
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Sound.Fluent where

import           Data.ByteString.Char8            (ByteString (..))
import qualified Data.ByteString.Char8            as BS
import qualified Data.Vector.Storable             as V
import qualified Sound.File.Sndfile               as SF
import qualified Sound.File.Sndfile.Buffer.Vector as VSF
import qualified Sound.OSC                        as OSC
import qualified Sound.OSC.Transport.FD
import qualified Sound.PortAudio                  as PA
import qualified System.Random                    as R

-- import Control.Lens


import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.List                        (isPrefixOf)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Text.Printf

import           System.Environment               (getArgs)

{-
  A real-time DSP engine and sampler in Haskell based on portaudio, hsndfile and hosc.

  The API is imperative/OO: i.e. you need to create a Fluent instance
  inside IO and call methods to preload and start listening to OSC
  in a certain order: see runFluent for an example.

  You can have serveral Fluent instances, but initAudio/killAudio uses
  Portaudio global initialize and claims the entire audio hardware.

  Currently supports:
    - Playing mono 44100 wav or aiff files
    - Stereo (2 channel) output to the system default device
    - Preloads all clips into RAM plays or stops sound in response to OSC
    - All generators (instance of a playing sound) write to global output (mixing their result)

  Possibilities:
    - Typical sampler stuff, i.e. looping (over specific span) and speed/pitch
    - Allow generators to use several source files
    - Allow generators to compute a DSP graph (from source files or internal state, for i.e. filters)
    - Allow generators to write to buffers instead of just reading

  Implementing the above stuff would bring us closer to something like scsynth/CSound. I don't really
  want to go into the world of full DSP without thinking about a more functional way of organizing it.

  Something like: A fluent instance computes the sound in a *world*, in which *sounds* take place.
  Sounds may be *transformed*, i.e. delayed, stretched, positioned in space, or inside a filter or
  resonant environment. All this to get rid of the error-prone manual global bus/buffer/execution order
  stuff of scsynth et al.
-}

-- | Measure time in number of processed sample for speed and precision.
type Time = Int

timeToSeconds :: Time -> Double
timeToSeconds x = fromIntegral x / kSAMPLE_RATE

secondsToTime :: Double -> Time
secondsToTime x = floor (x * kSAMPLE_RATE)

-- |
data Span = Span !Time !Time
  deriving (Eq, Ord, Show)

inside :: Time -> Span -> Bool
inside t (Span a b) = a <= t && t < b

onset :: Span -> Time
onset (Span a b) = a

offset :: Span -> Time
offset (Span a b) = b

duration :: Span -> Time
duration s = offset s - onset s

delay :: Time -> Span -> Span
delay t (Span a b) = Span (t + a) (t + b)

startAt :: Time -> Span -> Span
startAt t s = delay (t - onset s) s

data Clip =
  Clip
    !Text      -- unique clip id
    !Text      -- source file path
    !Span      -- part of file
  deriving (Show)

clipName :: Clip -> Text
clipName (Clip n sf sp) = n

clipSourceFile :: Clip -> Text
clipSourceFile (Clip n sf sp) = sf

clipSpan :: Clip -> Span
clipSpan (Clip n sf sp) = sp

data Gen =
  Gen
    !Text      -- unique gen id
    !Clip      -- clip to play
    !Time      -- time generator was created
  deriving (Show)

genName :: Gen -> Text
genName (Gen n cl started) = n

genClip :: Gen -> Clip
genClip (Gen n cl started) = cl

genStarted :: Gen -> Time
genStarted (Gen n cl started) = started

-- Span in which generator is alive
genSpan :: Gen -> Span
genSpan (Gen _ (Clip _ _ s) t) = startAt t s

genIsAlive :: Time -> Gen -> Bool
genIsAlive t s = t `inside` genSpan s

-- All global state
data Fluent = Fluent {
  -- preloadBuffers only access clips and buffers
  -- DSP thread access buffers, gens and time
  _fluentClips   :: TVar (Map Text Clip),
  _fluentBuffers :: TVar (Map Text (V.Vector Float)),
  _fluentGens    :: TVar (Map Text Gen),
  _fluentTime    :: TVar Time
  }

newFluent :: IO Fluent
newFluent = do
  c <- atomically $ newTVar mempty
  b <- atomically $ newTVar mempty
  g <- atomically $ newTVar mempty
  t <- atomically $ newTVar 0
  return $ Fluent c b g t

removeAllGens :: Fluent -> IO ()
removeAllGens fluent =
  atomically $ writeTVar (_fluentGens fluent) mempty

removeFinishedGens :: Time -> Fluent -> IO ()
removeFinishedGens t fluent =
  atomically $ modifyTVar (_fluentGens fluent) (Map.filter (genIsAlive t))

removeGenNamed :: Text -> Fluent -> IO ()
removeGenNamed n fluent =
  atomically $ modifyTVar (_fluentGens fluent) (Map.delete n)

haveClip :: Clip -> Fluent -> IO Bool
haveClip clip fluent =
  atomically $ fmap (Map.member $ clipName clip) $ readTVar (_fluentClips fluent)


-- Set up buffer arrays
preloadBuffers :: [Clip] -> Fluent -> IO ()
preloadBuffers clips fluent =
  forM_ clips $ \clip -> do
    let inFile = T.unpack $ clipSourceFile clip
    putStrLn $ "Loading clip " ++ T.unpack (clipName clip) ++ "\t\t " ++ inFile ++ "..."
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
      time <- atomically $ readTVar (_fluentTime fluent)
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
  clips <- atomically $ readTVar (_fluentClips fluent)
  case Map.lookup clipId clips of
    Nothing -> putStrLn $ "Error: Unknown clip: " ++ T.unpack clipId
    Just c  -> startPlayingClip c genId fluent -- TODO unnecessary double check
  return ()

-- Remove generator
stopPlayingClipNamed
  :: Text   -- ^ id (for stop)
  -> Fluent
  -> IO ()
stopPlayingClipNamed = removeGenNamed


initAudio :: Fluent -> IO (PA.Stream CFloat CFloat)
initAudio fluent = do
  paRes <- PA.initialize
  case paRes of
    Just e  -> fail $ show e
    Nothing -> return ()

  str <- PA.openDefaultStream
    -- TODO assumes no input, stereo output
    (fromIntegral kINPUT_CHANNELS) -- inputs
    (fromIntegral kOUTPUT_CHANNELS) -- outputs
    kSAMPLE_RATE
    (Just kVECTOR_SIZE) -- TODO 'Nothing' is possibly more efficient
    (Just $ dspCallback fluent)
    (Just $ putStrLn "Audio thread finished alright") -- when done
  str2 <- case str of
    Left _ -> fail "Could not open stream"
    Right s -> return s
  threadDelay (1000000*1)
  PA.startStream str2
  return str2

  where

    dspCallback :: Fluent -> PA.StreamCallback CFloat CFloat
    dspCallback fluent __timing__ __flags__ frames __inpPtr__ outPtr = do
      -- TODO assumes 2 channels
      let channels = kOUTPUT_CHANNELS
      time    <- atomically $ readTVar (_fluentTime fluent)
      buffers <- atomically $ readTVar (_fluentBuffers fluent)
      gens    <- atomically $ readTVar (_fluentGens fluent)
      -- print gens

      -- Zero all channels
      forM_ [0..channels-1] $ \c ->
        forM_ [0..frames-1] $ \f -> do
          pokeElemOff outPtr (fromIntegral $ f*channels+c) 0

      -- Run all generators one by one
      forM_ (fmap snd $ Map.toList gens) $ \gen -> do
        let clip         = genClip gen
        let bufferName   = clipName clip
        let genStartTime = genStarted gen
        let clipOnset    = onset (clipSpan clip)
        case Map.lookup bufferName buffers of
          Nothing -> return () -- TODO missing buffer, report somewhere
          Just buffer -> do
            forM_ [0..channels-1] $ \channel ->
              forM_ [0..frames-1] $ \frame -> do
                -- Index to read: current time offset + current frame + offset in buffer
                let preciseGlobalTime = time + fromIntegral frame
                -- If we started later, read an earlier position in the buffer
                let preciseLocalTime = preciseGlobalTime - genStartTime
                let v = (V.!) buffer (preciseLocalTime + clipOnset)

                -- Add v to index (so simultanous generators are summed)
                addToElemOff outPtr (fromIntegral $ frame * channels + channel) (realToFrac v)

      atomically $ modifyTVar (_fluentTime fluent) (\t -> t + fromIntegral frames)
      removeFinishedGens time fluent
      return PA.Continue

    addToElemOff :: (Num a, Storable a) => Ptr a -> Int -> a -> IO ()
    addToElemOff p i x = do
      y <- peekElemOff p i
      pokeElemOff p i (x + y)
    {-# INLINE addToElemOff #-}


killAudio :: Fluent -> PA.Stream CFloat CFloat -> IO ()
killAudio fluent str = do
  PA.stopStream str
  threadDelay (1000000*1)
  PA.closeStream str
  threadDelay (1000000*1)
  paRes <- PA.terminate
  case paRes of
    Just e  -> fail $ show e
    Nothing -> return ()
  return ()

type Handler = OSC.Message -> IO ()

waitForOsc :: Handler -> IO ()
waitForOsc handler = do
  let port = 54321
  putStrLn $ "Listening for OSC messages on port " ++ show 54321
  let t = OSC.udpServer "127.0.0.1" port
  Sound.OSC.Transport.FD.withTransport t $ \t -> void $ OSC.untilPredicate not {-should be not-} $ do
    msgs <- Sound.OSC.Transport.FD.recvMessages t
    mapM_ handler msgs
    return $ any isQuitMessage msgs
  return ()
  where

isQuitMessage :: OSC.Message -> Bool
isQuitMessage m = "/fluent/quit" `isPrefixOf` OSC.messageAddress m

composeHandlers :: [Handler] -> Handler
composeHandlers = foldr composeHandlers2 (\x -> return ())

composeHandlers2 :: Handler -> Handler -> Handler
composeHandlers2 f g m = do
  f m
  g m

statusHandler :: Fluent -> Handler
statusHandler fluent m = when ("/fluent/status" `isPrefixOf` OSC.messageAddress m) $
  -- TODO print more info
  putStrLn "Fluent is alright"

startHandler :: Fluent -> Handler
startHandler fluent m  = when ("/fluent/play" `isPrefixOf` OSC.messageAddress m) $ go fluent m
  where
    go fluent  (OSC.Message _ [OSC.ASCII_String genId, OSC.ASCII_String clipId])
      = startPlayingClipNamed (bs2t clipId) (bs2t genId) fluent  >> return ()
    go fluent  (OSC.Message _ [OSC.ASCII_String genAndClipId])
      = let (genId : clipId : _) = BS.words genAndClipId in
        -- TODO crashes on bad msg
        startPlayingClipNamed (bs2t clipId) (bs2t genId) fluent  >> return ()
    go fluent _ = putStrLn "Error: Bad message"

stopHandler :: Fluent -> Handler
stopHandler fluent m = when ("/fluent/stop" `isPrefixOf` OSC.messageAddress m) $ go fluent m
  where
    go fluent  (OSC.Message _ [OSC.ASCII_String genId])
      = stopPlayingClipNamed (bs2t genId) fluent  >> return ()
    go fluent _ = putStrLn "Error: Bad message"

runFluent :: IO ()
runFluent = do
  putStrLn "Fluent says hello!"

  fluent <- newFluent
  setup  <- readFile "setupFluent.hs"
  preloadBuffers (setupFileTextToClips setup) fluent

  str2 <- initAudio fluent
  waitForOsc $ composeHandlers [statusHandler fluent, startHandler fluent, stopHandler fluent]
  killAudio fluent str2

  putStrLn "Fluent says goodbye!"


setupFileTextToClips :: String -> [Clip]
setupFileTextToClips = setupFileDataToClips . read

setupFileDataToClips :: [(String, String, (Double, Double))] -> [Clip]
setupFileDataToClips = map toClip
  where
    toClip (n,f,(on,off)) = Clip (T.pack n) (T.pack f) (Span (floor $ on*kSAMPLE_RATE) (floor $ off*kSAMPLE_RATE))

bs2t :: ByteString -> Text
bs2t = T.pack . BS.unpack

kSAMPLE_RATE    = 44100
kVECTOR_SIZE     = 128
kINPUT_CHANNELS  = 0
kOUTPUT_CHANNELS = (2 :: CULong)
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
