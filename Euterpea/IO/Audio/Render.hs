{-# LANGUAGE Arrows, FlexibleContexts, NamedFieldPuns, ScopedTypeVariables #-}

-- | Render a Music object to a audio signal function that can be further
-- manipulated or saved to a file.  It is channel-agnostic in that it is
-- able to deal with instruments of arbitrary number of channels.
module Euterpea.IO.Audio.Render
  ( Instr
  , InstrMap
  , renderSF
  ) where

import Control.Arrow (arr, (>>>))
import Control.Arrow.Operations (delay)
import qualified Data.IntMap as IM
import Data.List (foldl', sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

import Control.Arrow.ArrowP (ArrowP (ArrowP), strip)
import Control.SF.SF (SF, runSF)
import Euterpea.IO.Audio.Basics (integral, outA)
import Euterpea.IO.Audio.Types (AudioSample, Clock, Signal, mix, zero)
import Euterpea.IO.MIDI.MEvent
  (MEvent (MEvent), eDur, eInst, eParams, ePitch, eTime, eVol, perform1Dur)
import Euterpea.Music (AbsPitch, Dur, InstrumentName, Music, ToMusic1, Volume, toMusic1)


-- | Every instrument is a function that takes a duration, absolute
-- pitch, volume, and a list of parameters (Doubles).  What the function
-- actually returns is implementation independent.
type Instr a = Dur -> AbsPitch -> Volume -> [Double] -> a

type InstrMap a = [(InstrumentName, Instr a)]

lookupInstr :: InstrumentName -> InstrMap a -> Instr a
lookupInstr ins im = fromMaybe noInstErr $ lookup ins im where
  noInstErr = error $ "Instrument " ++ show ins ++ " does not have a matching Instr in the supplied InstrMap."

-- | Each note in a Performance is tagged with a unique NoteId, which
-- helps us keep track of the signal function associated with a note.
type NoteId = Int

-- | In this particular implementation, 'a' is the signal function that
-- plays the given note.
data NoteEvt a
  = NoteOn  NoteId a
  | NoteOff NoteId

type Evt a = (Double, NoteEvt a)  -- Timestamp in seconds, and the note event


-- | Turn an Event into a NoteOn and a matching NoteOff with the same NodeId.
eventToEvtPair :: InstrMap a -> MEvent -> Int -> [Evt a]
eventToEvtPair imap MEvent {eTime, eInst, ePitch, eDur, eVol, eParams} nid =
  [(tOn, NoteOn nid sf), (tOn + tDur, NoteOff nid)] where
    sf    = instr eDur ePitch eVol eParams
    tOn   = fromRational eTime
    tDur  = fromRational eDur :: Double
    instr = lookupInstr eInst imap

-- | Turn a Performance into an SF of NoteOn/NoteOffs.
-- For each note, generate a unique id to tag the NoteOn and NoteOffs.
-- The tag is used as the key to the collection of signal functions
-- for efficient insertion/removal.
toEvtSF :: Clock p => [MEvent] -> InstrMap a -> Signal p () [Evt a]
toEvtSF pf imap = proc _ -> do
  rec
    t <- integral -< 1
    es <- delay evts -< next
    -- Trim events that are due off the list and output them, retaining the rest
    let (evs, next) = span ((<= t) . fst) es
  outA -< evs
  where
    -- Sort all NoteOn/NoteOff events by timestamp.
    evts = sortOn fst . concat $ zipWith (eventToEvtPair imap) pf [0..]

-- | Modify the collection upon receiving NoteEvts.  The timestamps
-- are not used here, but they are expected to be the same.
modSF :: IM.IntMap a -> [Evt a] -> IM.IntMap a
modSF = foldl' mod where
  mod :: IM.IntMap a -> (b, NoteEvt a) -> IM.IntMap a  -- derived
  mod m (_, NoteOn nid sf) = IM.insert nid sf m
  mod m (_, NoteOff nid)   = IM.delete nid m

-- | Simplified version of a parallel switcher.
-- Note that this is tied to the particular implementation of SF, as it
-- needs to use runSF to run all the signal functions in the collection.
pSwitch :: forall p col a. (Clock p, Functor col)
  => col (Signal p () a)                -- Initial SF collection.
  -> Signal p () [Evt (Signal p () a)]  -- Input event stream.
  -> (col (Signal p () a) -> [Evt (Signal p () a)] -> col (Signal p () a))
     -- A Modifying function that modifies the collection of SF
     -- based on the event that is occuring.
  -> Signal p () (col a)
     -- The resulting collection of output values obtained from
     -- running all SFs in the collection.
pSwitch col esig mod = proc _ -> do
  evts <- esig -< ()
  rec
    -- perhaps this can be run at a lower rate using upsample
    sfcol <- delay col -< mod sfcol' evts
    let rs = fmap (\s -> runSF (strip s) ()) sfcol :: col (a, SF () a)
        (as, sfcol' :: col (Signal p () a)) = (fmap fst rs, fmap (ArrowP . snd) rs)
  outA -< as

-- | Returns tuple with duration of the music in seconds, and a signal function that plays the music.
renderSF :: (Clock p, ToMusic1 a, AudioSample b)
  => Music a
  -> InstrMap (Signal p () b)
  -> (Double, Signal p () b)
renderSF m im = (fromRational d, sf) where
  sf      = allsf >>> arr (foldl' mix zero . IM.elems)  -- add up all samples
  allsf   = pSwitch IM.empty evtsf modSF
  evtsf   = toEvtSF pf im
  (pf, d) = perform1Dur $ toMusic1 m  -- Updated 16-Dec-2015
