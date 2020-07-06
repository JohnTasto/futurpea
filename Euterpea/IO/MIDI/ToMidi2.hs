-- ToMidi2: a module for allowing multiple tracks with the same GM instrument.
-- Author: Donya Quick
--
-- The writeMidi2 function allows use of the CustomInstrument constructor
-- in a very specific way to permit two tracks that have the same instrument.
-- The expected format is:
--
-- CustomInstrument "GMInstrumentName UniqueIdentifier"
--
-- For example:
--
--  import Euterpea
--  import Euterpea.IO.MIDI.ToMidi2
--  m = instrument (CustomInstrument "Flute A") (c 4 qn :+: d 4 qn) :=:
--      instrument (CustomInstrument "Flute B") (c 5 qn) :=:
--      instrument HonkyTonkPiano (rest hn :+: c 4 hn)
--  main = writeMidi2 "test.mid" m
--
-- This will create a MIDI file with three tracks, two of which are assigned
-- the Flute instrument and the third with the HonkyTonkPiano instrument.
--
-- Note: this module does NOT allow specification of particular track numbers.
-- The order in which the tracks appear in the MIDI file is determined by the
-- structure of the particular Music value.

module Euterpea.IO.MIDI.ToMidi2
  ( writeMidi2
  , resolveInstrumentName
  ) where

import Codec.Midi
  (FileType (MultiTrack, SingleTrack), Midi (Midi), TimeDiv (TicksPerBeat), fromAbsTime)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

import Euterpea.IO.MIDI.ExportMidiFile (exportMidiFile)
import Euterpea.IO.MIDI.MEvent (MEvent (MEvent), eInst, perform)
import Euterpea.IO.MIDI.ToMidi
  (UserPatchMap, allValid, defUpm, division, makeGMMap, mevsToMessages, splitByInst)
import Euterpea.Music (InstrumentName (AcousticGrandPiano, CustomInstrument), Music, ToMusic1)

instNameOnly :: String -> String
instNameOnly []     = []
instNameOnly (x:xs) = if x == ' ' then [] else x : instNameOnly xs

resolveInstrumentName :: InstrumentName -> InstrumentName
resolveInstrumentName x@(CustomInstrument s) = if i >= 0 then allInsts !! i else x where
  i        = fromMaybe (-1) . elemIndex iName $ show <$> allInsts
  allInsts = take 128 $ enumFrom AcousticGrandPiano
  iName    = instNameOnly s
resolveInstrumentName x                      = x

resolveMEventInsts :: [(InstrumentName, [MEvent])] -> [(InstrumentName, [MEvent])]
resolveMEventInsts = map f1 where
  f1 (iname, mevs) = (resolveInstrumentName iname, map f2 mevs)
  f2 mev           = mev{eInst=resolveInstrumentName (eInst mev)}

writeMidi2 :: ToMusic1 a => FilePath -> Music a -> IO ()
writeMidi2 fn m = exportMidiFile fn $ toMidiUPM2 defUpm $ perform m

toMidiUPM2 :: UserPatchMap -> [MEvent] -> Midi
toMidiUPM2 upm pf = Midi (if length split == 1 then SingleTrack else MultiTrack)
  (TicksPerBeat division) $ fromAbsTime . mevsToMessages rightMap <$> split
  where
    rightMap = if allValid upm insts then upm else makeGMMap insts
    insts    = map fst split
    split    = resolveMEventInsts $ splitByInst pf
