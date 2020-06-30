{-# LANGUAGE ExistentialQuantification, FlexibleContexts, ScopedTypeVariables #-}

module Euterpea.IO.Audio.IO
  ( outFile
  , outFileNorm
  , maxSample
  ) where

import Codec.Wav (exportFile)
import Data.Array.Unboxed (listArray)
import Data.Audio (Audio (Audio, sampleData), channelNumber, fromSample, sampleRate)
import Data.Int (Int32)

import Control.Arrow.ArrowP (ArrowP, strip)
import Control.SF.SF (SF, unfold)
import Euterpea.IO.Audio.Types hiding (Signal)

type Signal clk a b = ArrowP SF clk a b

-- | Writes sound to a wave file (.wav)
outFile :: forall a p. (AudioSample a, Clock p)
  => String         -- ^ Filename to write to.
  -> Double         -- ^ Duration of the wav in seconds.
  -> Signal p () a  -- ^ Signal representing the sound.
  -> IO ()
outFile = outFileHelp' id

normList :: [Double] -> [Double]
normList xs = (\x -> x / max 1.0 (maximum $ abs <$> xs)) <$> xs

-- | Like outFile, but normalizes the output if the amplitude of
-- the signal goes above 1.  If the maximum sample is less than
-- or equal to 1, the output is not normalized.
-- Currently this requires storing the entire output stream in memory
-- before writing to the file.
outFileNorm :: forall a p. (AudioSample a, Clock p)
  => String         -- ^ Filename to write to.
  -> Double         -- ^ Duration of the wav in seconds.
  -> Signal p () a  -- ^ Signal representing the sound.
  -> IO ()
outFileNorm = outFileHelp' normList

outFileHelp :: forall a p. (AudioSample a, Clock p)
  => ([Double] -> [Double])  -- ^ Post-processing function.
  -> String                  -- ^ Filename to write to.
  -> Double                  -- ^ Duration of the wav in seconds.
  -> Signal p () a           -- ^ Signal representing the sound.
  -> IO ()
outFileHelp f filepath dur sf = exportFile filepath aud where
  sr          = rate (undefined :: p)
  numChannels = numChans (undefined :: a)
  numSamples  = truncate (dur * sr) * numChannels
                     -- multiply by 0.999 to avoid wraparound at 1.0
  dat         = map (fromSample . (*0.999)) (f (toSamples dur sf)) :: [Int32]
  array       = listArray (0, numSamples-1) dat
  aud = Audio
    { sampleRate    = truncate sr
    , channelNumber = numChannels
    , sampleData    = array
    }

-- Alternative definition of the above that enforces a clipping behavior when
-- the value exceeds the [-1.0, 1.0] range. The overflow behavior makes it
-- very hard to debug sound modeling problems that involve certain waveforms,
-- such as saw waves. Clipping is also a more common behavior in other audio
-- software rather than overflowing or wrap-around.

outFileHelp' :: forall a p. (AudioSample a, Clock p)
  => ([Double] -> [Double])  -- ^ Post-processing function.
  -> String                  -- ^ Filename to write to.
  -> Double                  -- ^ Duration of the wav in seconds.
  -> Signal p () a           -- ^ Signal representing the sound.
  -> IO ()
outFileHelp' f filepath dur sf = exportFile filepath aud where
  sr          = rate (undefined :: p)
  numChannels = numChans (undefined :: a)
  numSamples  = truncate (dur * sr) * numChannels
  dat         = map (fromSample . (*0.999) . clipFix) (f (toSamples dur sf)) :: [Int32]
  array       = listArray (0, numSamples-1) dat
  aud = Audio
    { sampleRate    = truncate sr
    , channelNumber = numChannels
    , sampleData    = array
    }
  clipFix x
    | x > 1.0    = 1.0
    | x < (-1.0) = -1.0
    | otherwise  = x

toSamples :: forall a p. (AudioSample a, Clock p) => Double -> Signal p () a -> [Double]
toSamples dur = take numSamples . concatMap collapse . unfold . strip where
  sr          = rate     (undefined :: p)
  numChannels = numChans (undefined :: a)
  numSamples  = truncate (dur * sr) * numChannels

-- | Compute the maximum sample of an SF in the first 'dur' seconds.
maxSample :: forall a p. (AudioSample a, Clock p) => Double -> Signal p () a -> Double
maxSample dur sf = maximum $ abs <$> toSamples dur sf
