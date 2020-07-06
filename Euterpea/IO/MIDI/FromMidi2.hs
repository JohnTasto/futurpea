-- FromMidi2: an alternative Midi-to-Music conversion algorithm.
-- Author: Donya Quick
-- Last modified: 28-Dec-2016

-- The goal of this module is to provide a more intelligent
-- parse from MIDI files to Music structures. The fromMidi
-- function will convert Midi into Music, but the resulting
-- structure is one big parallel composition with no other
-- relationships between the notes. The fromMidi2 function
-- here is an attempt to provide a parse of musical features
-- that is more in line with how a human might write them
-- in Euterpea or perceive them by ear. It works best on
-- MIDI files that are very close to paper score in terms
-- of how the events are structured. The functions here are
-- not intended for use with "messy" MIDI files that have
-- been recorded from a live performance without quantization.

-- You can use fromMidi2 as an alternative to fromMidi to
-- parse a Midi value into a Music value with a better
-- method of grouping events together. The same algorithm
-- can be applied directly to a Music value with the
-- restructure function.

-- Examples of how to use fromMidi2 and restructure:

-- testMidi file = do
--     x <- importFile file
--     case x of Left err -> error err
--               Right m -> do
--                   let v = fromMidi2 m
--                   putStrLn $ show v
--                   play v

-- myMusic :: Music (Pitch, Volume)
-- myMusic = ...
-- newMusic :: Music (Pitch, Volume)
-- newMusic = restructure myMusic

-- Restructuring is done from the MEvent level. Importantly,
-- this means that there are no tempo changes or other Modify
-- nodes in the resulting Music value! A global tempo of
-- 120BPM is assumed. If your MIDI file has a different BPM,
-- you can use fromMidi in combination with restructure and
-- then apply a tempo modifier afterwards.

-- The method for organizing events is:
-- (1) Identify and group chords where every note
--     has the same start time and duration.
-- (2) Identify and group sequential patterns where items
--     are back-to-back. Note that this may include a mix of
--     single notes and chords from step 1.
-- (3) Greedily group any patterns with gaps between
--     them into a sequence with rests.

module Euterpea.IO.MIDI.FromMidi2
  ( fromMidi2
  , restructure
  , Chunk
  , chunkEvents
  , chunkToMusic
  ) where

import Euterpea.IO.MIDI.FromMidi (fromMidi)
import Euterpea.IO.MIDI.MEvent (MEvent (MEvent), eDur, ePitch, eTime, eVol, perform)
import Euterpea.IO.MIDI.ToMidi (splitByInst)
import Euterpea.Music
  ( AbsPitch
  , Dur
  , Music ((:+:))
  , Pitch
  , ToMusic1
  , Volume
  , chord
  , instrument
  , line
  , note
  , pitch
  , removeZeros
  , rest
  )

import Codec.Midi (Midi (Midi))
import Data.List (intercalate, sortBy)

-- | The primary exported functions for this module are:

fromMidi2 :: Midi -> Music (Pitch, Volume)
fromMidi2 = restructure . fromMidi

restructure :: (ToMusic1 a) => Music a -> Music (Pitch, Volume)
restructure = parseFeaturesI

-- | Other exported features are related to the Chunk datatype.

type Onset = Dur  -- to clarify some type signatures

-- | A Chunk is the data structure used to group events by the algorithm
-- described at the top of this file. Par and Chord correspond to features
-- that will be composed in parallel (:=:) at different levels, and Seq
-- corresponds to features that will be composed in sequence (:+:). E is
-- a wrapper for single events and R is a rest place-holder.
data Chunk = Par [Chunk] | Seq [Chunk] | Chord [Chunk] | E MEvent | R Onset Dur
  deriving Eq

-- | Initially, each MEvent is placed in its own chunk.
initChunk :: [MEvent] -> [Chunk]
initChunk mevs = map E $ sortBy sortFun mevs

-- | The chunkChord function looks for chunks that share the same
-- onset and duration and places them together in Chord chunks.
chunkChord :: [Chunk] -> [Chunk]
chunkChord []     = []
chunkChord (c:cs) = if null cChord
  then c                : chunkChord cs
  else Chord (c:cChord) : chunkChord (filter (`notElem` cChord) cs)
  where cChord = filter (chordWith c) cs

chordWith :: Chunk -> Chunk -> Bool
chordWith c0 c = chunkOnset c == chunkOnset c0 && chunkDur c == chunkDur c0

-- | The chunkMel function looks for sequences of chunks (which need
-- not be adjacent in the input list) where the end time of one chunk
-- is equal to the start time of the next chunk. There are no gaps
-- permitted, so notes separated by rests will not be grouped here.
chunkMel :: [Chunk] -> [Chunk]
chunkMel []       = []
chunkMel x@(c:cs) = if null cMel
  then c        : chunkMel cs
  else Seq cMel : chunkMel (filter (`notElem` cMel) x)
  where cMel = buildMelFrom (chunkOnset c) x  -- get ALL possible melody elements

buildMelFrom :: Onset -> [Chunk] -> [Chunk]
buildMelFrom t []     = []
buildMelFrom t (c:cs) = if chunkOnset c == t
  then c : buildMelFrom (t + chunkDur c) cs
  else buildMelFrom t cs

-- | The chunkSeqs function is more general and will look for anything
-- that can be grouped together linearly in time, even if it requires
-- inserting a rest. This will group together all non-overlapping
-- chunks in a greedy fashion.
chunkSeqs :: [Chunk] -> [Chunk]
chunkSeqs []       = []
chunkSeqs x@(c:cs) = if s == [c]
  then c : chunkSeqs cs
  else Seq s : chunkSeqs (filter (`notElem` s) x)
  where s = seqWithRests (chunkOnset c) x

seqWithRests :: Onset -> [Chunk] -> [Chunk]
seqWithRests t [] = []
seqWithRests t (c:cs)
  | dt == 0   = c : seqWithRests (tc + chunkDur c) cs
  | dt > 0    = R t dt : c : seqWithRests (tc + chunkDur c) cs
  | otherwise = seqWithRests t cs
  where
    tc = chunkOnset c
    dt = tc - t

-- | Finally, chunkEvents combines all of these methods in a particular
-- order that establishes preference for chords first, then melodies
-- (which may include chords), and then sequences including rests.
-- Anything left over will be handled by an outer Par.
chunkEvents :: [MEvent] -> Chunk
chunkEvents = Par . chunkSeqs . chunkMel . chunkChord . initChunk

-- |Chunks can be converted directly to Music. Durations have to be
-- divided in half because MEvents deal with seconds, while Music
-- deals with duration as whole notes (1 whole note = 2 seconds).
chunkToMusic :: Chunk -> Music (Pitch, Volume)
chunkToMusic (E     e) = note (eDur e / 2) (pitch $ ePitch e, eVol e)
chunkToMusic (R   _ d) = rest (d/2)
chunkToMusic (Seq   x) = line(map chunkToMusic x)
chunkToMusic (Chord x) = chord(map chunkToMusic x)
chunkToMusic (Par   x) = chord $ map (\v -> rest (chunkOnset v / 2) :+: chunkToMusic v) x

-- | The parseFeatures function will take an existing Music value, such
-- as one returned by fromMidi, and use the algorithms above to identify
-- musical features (chords and melodies) and construct a new Music
-- tree that is performance-equivalent to the original.
parseFeatures :: (ToMusic1 a) => Music a -> Music (Pitch, Volume)
parseFeatures = removeZeros . chunkToMusic . chunkEvents . perform

parseFeaturesI :: (ToMusic1 a) => Music a -> Music (Pitch, Volume)
parseFeaturesI m = chord $ zipWith instrument iList parsesI where
  parsesI        = removeZeros . chunkToMusic . chunkEvents <$> mevsI
  (iList, mevsI) = unzip $ splitByInst $ perform m

-- Utility Functions and Type Class Instances

-- | First, some functions to pretty-up printing of things for debugging purposes

doubleShow :: Rational -> String
doubleShow x = show (fromRational x :: Double)

pcShow :: AbsPitch -> String
pcShow = show . fst . pitch

listShow :: (Show a) => [a] -> String
listShow x = "[" ++ intercalate ", " (show <$> x) ++ "]"

listShowN :: (Show a) => [a] -> String
listShowN x = "[\n    " ++ intercalate ",\n    " (show <$> x) ++ "\n]"

listShowX :: (Show a) => Int -> [a] -> String
listShowX i x = "[\n" ++ v ++ intercalate (",\n" ++ v) (show <$> x) ++ "\n" ++ v ++ "]"
  where v = concat $ replicate i " "

instance Show Chunk where
  show   (E     e) = "E " ++ doubleShow (eTime e) ++ " " ++ pcShow (ePitch e) ++ " " ++ doubleShow (eDur e)
  show s@(Seq   x) = "S " ++ doubleShow (chunkOnset s) ++ " " ++ listShowX 4 x
  show c@(Chord x) = "C " ++ doubleShow (chunkOnset c) ++ " " ++ listShowX 6 x
  show p@(Par   x) = "P " ++ doubleShow (chunkOnset p) ++ " " ++ listShowX 2 x
  show   (R   o d) = "R " ++ doubleShow o ++ " " ++ doubleShow d

-- | An Ord instance for Chunk that enforces sorting based on onset time. No
-- other features are considered.
instance Ord Chunk where
  compare x1 x2 = compare (chunkOnset x1) (chunkOnset x2)

-- Functions to determine the start time (onset) and duration of a Chunk.

chunkOnset :: Chunk -> Onset
chunkOnset (Seq   x) = if null x then error "Empty Seq!"   else chunkOnset $ head x
chunkOnset (Chord x) = if null x then error "Empty Chord!" else chunkOnset $ head x
chunkOnset (Par   x) = if null x then 0                    else minimum $ chunkOnset <$> x
chunkOnset (E     e) = eTime e
chunkOnset (R   o _) = o

chunkEnd :: Chunk -> Onset
chunkEnd (Seq   x) = if null x then error "Empty Seq!"   else chunkEnd $ last x
chunkEnd (Chord x) = if null x then error "Empty Chord!" else chunkEnd $ head x
chunkEnd (Par   x) = if null x then 0                    else maximum $ chunkEnd <$> x
chunkEnd (E     e) = eTime e + eDur e
chunkEnd (R   o d) = o + d

chunkDur :: Chunk -> Dur
chunkDur (Seq   x) = if null x then error "Empty Seq!"   else sum $ chunkDur <$> x
chunkDur (Chord x) = if null x then error "Empty Chord!" else chunkDur (head x)
chunkDur c@(Par x) = if null x then 0                    else chunkEnd c - chunkOnset c
chunkDur (E     e) = eDur e
chunkDur (R   _ d) = d

-- Special sorting function for MEvents.

sortFun :: MEvent -> MEvent -> Ordering
sortFun e1 e2 = if eTime e1 == eTime e2
  then compare (ePitch e1) (ePitch e2)
  else compare (eTime e1) (eTime e2)
