{-# LANGUAGE FlexibleInstances #-}

module Euterpea.Music where

import Control.Lens (both, over)

type AbsPitch = Int
type Octave   = Int
type Pitch    = (PitchClass, Octave)
type Dur      = Rational
data PitchClass
  = Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds
  | Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
  | Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As
  | Bf | Ass | B | Bs | Bss
  deriving (Show, Eq, Ord, Read, Enum, Bounded)

data Primitive a
  = Note Dur a
  | Rest Dur
  deriving (Show, Eq, Ord)

instance Functor Primitive where
--fmap :: (a -> b) -> Primitive a -> Primitive b
  fmap f (Note d x) = Note d (f x)
  fmap _ (Rest d)   = Rest d

data Music a
  = Prim (Primitive a)        -- ^ primitive value
  | Music a :+: Music a       -- ^ sequential composition
  | Music a :=: Music a       -- ^ parallel composition
  | Modify Control (Music a)  -- ^ modifier
  deriving (Show, Eq, Ord)

infixr 5 :+:, :=:

instance Functor Music where
--mMap :: (a -> b) -> Music a -> Music b
  fmap f (Prim p)     = Prim (fmap f p)
  fmap f (m1 :+: m2)  = fmap f m1 :+: fmap f m2
  fmap f (m1 :=: m2)  = fmap f m1 :=: fmap f m2
  fmap f (Modify c m) = Modify c (fmap f m)

mFold ::
     (Primitive a -> b)
  -> (b -> b -> b)
  -> (b -> b -> b)
  -> (Control -> b -> b)
  -> Music a -> b
mFold f (+:) (=:) g m = case m of
  Prim p     -> f p
  m1 :+: m2  -> recr m1 +: recr m2
  m1 :=: m2  -> recr m1 =: recr m2
  Modify c m -> g c (recr m)
  where recr = mFold f (+:) (=:) g

data Control
  = Tempo      Rational           -- ^ scale the tempo
  | Transpose  AbsPitch           -- ^ transposition
  | Instrument InstrumentName     -- ^ instrument label
  | Phrase     [PhraseAttribute]  -- ^ phrase attributes
  | KeySig     PitchClass Mode    -- ^ key signature and mode
  | Custom     String             -- ^ for user-specified controls
  deriving (Show, Eq, Ord)

data Mode
  = Major | Minor
  | Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian
  | CustomMode String
  deriving (Show, Eq, Ord)

data InstrumentName
  -- |   0. Piano
  = AcousticGrandPiano   | BrightAcousticPiano  | ElectricGrandPiano   | HonkyTonkPiano
  | RhodesPiano          | ChorusedPiano        | Harpsichord          | Clavinet
  -- |   8. Chromatic Percussion
  | Celesta              | Glockenspiel         | MusicBox             | Vibraphone
  | Marimba              | Xylophone            | TubularBells         | Dulcimer
  -- |  16. Organ
  | HammondOrgan         | PercussiveOrgan      | RockOrgan            | ChurchOrgan
  | ReedOrgan            | Accordion            | Harmonica            | TangoAccordion
  -- |  24. Guitar
  | AcousticGuitarNylon  | AcousticGuitarSteel  | ElectricGuitarJazz   | ElectricGuitarClean
  | ElectricGuitarMuted  | OverdrivenGuitar     | DistortionGuitar     | GuitarHarmonics
  -- |  32. Bass
  | AcousticBass         | ElectricBassFingered | ElectricBassPicked   | FretlessBass
  | SlapBass1            | SlapBass2            | SynthBass1           | SynthBass2
  -- |  40. Strings
  | Violin               | Viola                | Cello                | Contrabass
  | TremoloStrings       | PizzicatoStrings     | OrchestralHarp       | Timpani
  -- |  48. Ensemble
  | StringEnsemble1      | StringEnsemble2      | SynthStrings1        | SynthStrings2
  | ChoirAahs            | VoiceOohs            | SynthVoice           | OrchestraHit
  -- |  56. Brass
  | Trumpet              | Trombone             | Tuba                 | MutedTrumpet
  | FrenchHorn           | BrassSection         | SynthBrass1          | SynthBrass2
  -- |  64. Reed
  | SopranoSax           | AltoSax              | TenorSax             | BaritoneSax
  | Oboe                 | Bassoon              | EnglishHorn          | Clarinet
  -- |  72. Pipe
  | Piccolo              | Flute                | Recorder             | PanFlute
  | BlownBottle          | Shakuhachi           | Whistle              | Ocarina
  -- |  80. Synth Lead
  | Lead1Square          | Lead2Sawtooth        | Lead3Calliope        | Lead4Chiff
  | Lead5Charang         | Lead6Voice           | Lead7Fifths          | Lead8BassLead
  -- |  88. Synth Pad
  | Pad1NewAge           | Pad2Warm             | Pad3Polysynth        | Pad4Choir
  | Pad5Bowed            | Pad6Metallic         | Pad7Halo             | Pad8Sweep
  -- |  96. Synth Effects
  | FX1Train             | FX2Soundtrack        | FX3Crystal           | FX4Atmosphere
  | FX5Brightness        | FX6Goblins           | FX7Echoes            | FX8SciFi
  -- | 104. Ethnic
  | Sitar                | Banjo                | Shamisen             | Koto
  | Kalimba              | Bagpipe              | Fiddle               | Shanai
  -- | 112. Percussive
  | TinkleBell           | Agogo                | SteelDrums           | Woodblock
  | TaikoDrum            | MelodicDrum          | SynthDrum            | ReverseCymbal
  -- | 120. Sound Effects
  | GuitarFretNoise      | BreathNoise          | Seashore             | BirdTweet
  | TelephoneRing        | Helicopter           | Applause             | Gunshot
  -- | Etc
  | Percussion           | CustomInstrument String
  deriving (Show, Eq, Ord)

data PhraseAttribute
  = Dyn Dynamic
  | Tmp Tempo
  | Art Articulation
  | Orn Ornament
  deriving (Show, Eq, Ord)

data Dynamic
  = Accent Rational | Crescendo Rational | Diminuendo Rational
  | StdLoudness StdLoudness | Loudness Rational
  deriving (Show, Eq, Ord)

data StdLoudness = PPP | PP | P | MP | SF | MF | NF | FF | FFF
  deriving (Show, Eq, Ord, Enum)

data Tempo = Ritardando Rational | Accelerando Rational
  deriving (Show, Eq, Ord)

data Articulation
  = Staccato Rational | Legato Rational | Slurred Rational
  | Tenuto | Marcato | Pedal | Fermata | FermataDown | Breath
  | DownBow | UpBow | Harmonic | Pizzicato | LeftPizz
  | BartokPizz | Swell | Wedge | Thumb | Stopped
  deriving (Show, Eq, Ord)

data Ornament
  = Trill | Mordent | InvMordent | DoubleMordent
  | Turn | TrilledTurn | ShortTrill
  | Arpeggio | ArpeggioUp | ArpeggioDown
  | Instruction String | Head NoteHead
  | DiatonicTrans Int
  deriving (Show, Eq, Ord)

data NoteHead
  = DiamondHead | SquareHead | XHead | TriangleHead
  | TremoloHead | SlashHead | ArtHarmonic | NoHead
  deriving (Show, Eq, Ord)

type Volume = Int

addVolume :: Volume -> Music Pitch -> Music (Pitch, Volume)
addVolume v = fmap (\p -> (p, v))

data NoteAttribute
  = Volume Int        -- ^ MIDI convention: 0=min, 127=max
  | Fingering Integer
  | Dynamics String
  | Params [Double]
  deriving (Eq, Show)

type Note1  = (Pitch, [NoteAttribute])
type Music1 = Music Note1

-- | A new type class to allow for musical polymorphism that ultimately
-- must be converted to Music1 to be converted to MIDI format through
-- the MEvent framework.
class ToMusic1 a where
  toMusic1 :: Music a -> Music1

instance ToMusic1 Pitch where
  toMusic1 = fmap (\p -> (p, []))

instance ToMusic1 (Pitch, Volume) where
  toMusic1 = fmap (\(p, v) -> (p, [Volume v]))

instance ToMusic1 Note1 where
  toMusic1 = id

instance ToMusic1 AbsPitch where
  toMusic1 = fmap (\a -> (pitch a, []))

instance ToMusic1 (AbsPitch, Volume) where
  toMusic1 = fmap (\(p, v) -> (pitch p, [Volume v]))

note :: Dur -> a -> Music a
note d p = Prim $ Note d p

rest :: Dur -> Music a
rest = Prim . Rest

tempo :: Dur -> Music a -> Music a
tempo = Modify . Tempo

transpose :: AbsPitch -> Music a -> Music a
transpose = Modify . Transpose

instrument :: InstrumentName -> Music a -> Music a
instrument = Modify . Instrument

phrase :: [PhraseAttribute] -> Music a -> Music a
phrase = Modify . Phrase

keysig :: PitchClass -> Mode -> Music a -> Music a
keysig pc mo = Modify $ KeySig pc mo

cff,cf,c,cs,css,dff,df,d,ds,dss,eff,ef,e,es,ess,fff,ff,f,
  fs,fss,gff,gf,g,gs,gss,aff,af,a,as,ass,bff,bf,b,bs,bss ::
    Octave -> Dur -> Music Pitch
cff o d = note d (Cff, o);  cf  o d = note d (Cf,  o)
c   o d = note d (C,   o);  cs  o d = note d (Cs,  o)
css o d = note d (Css, o);  dff o d = note d (Dff, o)
df  o d = note d (Df,  o);  d   o d = note d (D,   o)
ds  o d = note d (Ds,  o);  dss o d = note d (Dss, o)
eff o d = note d (Eff, o);  ef  o d = note d (Ef,  o)
e   o d = note d (E,   o);  es  o d = note d (Es,  o)
ess o d = note d (Ess, o);  fff o d = note d (Fff, o)
ff  o d = note d (Ff,  o);  f   o d = note d (F,   o)
fs  o d = note d (Fs,  o);  fss o d = note d (Fss, o)
gff o d = note d (Gff, o);  gf  o d = note d (Gf,  o)
g   o d = note d (G,   o);  gs  o d = note d (Gs,  o)
gss o d = note d (Gss, o);  aff o d = note d (Aff, o)
af  o d = note d (Af,  o);  a   o d = note d (A,   o)
as  o d = note d (As,  o);  ass o d = note d (Ass, o)
bff o d = note d (Bff, o);  bf  o d = note d (Bf,  o)
b   o d = note d (B,   o);  bs  o d = note d (Bs,  o)
bss o d = note d (Bss, o)


bn, wn, hn, qn, en, sn, tn, sfn, dwn, dhn,
  dqn, den, dsn, dtn, ddhn, ddqn, dden :: Dur
bnr, wnr, hnr, qnr, enr, snr, tnr, sfnr, dwnr, dhnr,
  dqnr, denr, dsnr, dtnr, ddhnr, ddqnr, ddenr :: Music Pitch
bn   = 2;     bnr   = rest bn   -- brevis rest
wn   = 1;     wnr   = rest wn   -- whole note rest
hn   = 1/2;   hnr   = rest hn   -- half note rest
qn   = 1/4;   qnr   = rest qn   -- quarter note rest
en   = 1/8;   enr   = rest en   -- eighth note rest
sn   = 1/16;  snr   = rest sn   -- sixteenth note rest
tn   = 1/32;  tnr   = rest tn   -- thirty-second note rest
sfn  = 1/64;  sfnr  = rest sfn  -- sixty-fourth note rest
-- Dotted
dwn  = 3/2;   dwnr  = rest dwn  -- dotted whole note rest
dhn  = 3/4;   dhnr  = rest dhn  -- dotted half note rest
dqn  = 3/8;   dqnr  = rest dqn  -- dotted quarter note rest
den  = 3/16;  denr  = rest den  -- dotted eighth note rest
dsn  = 3/32;  dsnr  = rest dsn  -- dotted sixteenth note rest
dtn  = 3/64;  dtnr  = rest dtn  -- dotted thirty-second note rest
-- Double Dotted
ddhn = 7/8;   ddhnr = rest ddhn -- double-dotted half note rest
ddqn = 7/16;  ddqnr = rest ddqn -- double-dotted quarter note rest
dden = 7/32;  ddenr = rest dden -- double-dotted eighth note rest

-- | The conversion for Pitch and AbsPitch differs from previous versions
-- of Euterpea. In Euterpea 1.x, (C, 5) was pitch number 60, which is not
-- the most common interpretation. While there is no universal standard
-- for which octave should be octave 0, it is far more common to have the
-- pitch number relationship that (C, 4) = 60. Since this change has been
-- requested many times in previous versions of Euterpea, the following
-- standard is now in place as of version 2.0.0:
-- >>> pitch 0 = (C, -1)
-- >>> pitch 60 = (C, 4)
-- >>> pitch 127 = (G, 9)
absPitch :: Pitch -> AbsPitch
absPitch (pc, oct) = 12*(oct + 1) + pcToInt pc

pcToInt :: PitchClass -> Int
pcToInt pc = case pc of
  Cff -> -2;  Cf -> -1;  C ->  0;  Cs ->  1;  Css ->  2;
  Dff ->  0;  Df ->  1;  D ->  2;  Ds ->  3;  Dss ->  4;
  Eff ->  2;  Ef ->  3;  E ->  4;  Es ->  5;  Ess ->  6;
  Fff ->  3;  Ff ->  4;  F ->  5;  Fs ->  6;  Fss ->  7;
  Gff ->  5;  Gf ->  6;  G ->  7;  Gs ->  8;  Gss ->  9;
  Aff ->  7;  Af ->  8;  A ->  9;  As -> 10;  Ass -> 11;
  Bff ->  9;  Bf -> 10;  B -> 11;  Bs -> 12;  Bss -> 13;

pitch :: AbsPitch -> Pitch
pitch ap = ([C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B] !! n, oct - 1)
  where (oct, n) = divMod ap 12

trans :: Int -> Pitch -> Pitch
trans i p = pitch (absPitch p + i)

line, chord :: [Music a] -> Music a
line  = foldr (:+:) (rest 0)
chord = foldr (:=:) (rest 0)

line1, chord1 :: [Music a] -> Music a
line1  = foldr1 (:+:)
chord1 = foldr1 (:=:)

offset :: Dur -> Music a -> Music a
offset d m = rest d :+: m

times :: Int -> Music a -> Music a
times 0 _ = rest 0
times n m = m :+: times (n - 1) m

forever :: Music a -> Music a
forever m = m :+: forever m

lineToList :: Music a -> [Music a]
lineToList (Prim (Rest 0)) = []
lineToList (n :+: ns)      = n : lineToList ns
lineToList _               =
  error "lineToList: argument not created by function line"

invertAt :: Pitch -> Music Pitch -> Music Pitch
invertAt pRef = fmap (\p -> pitch (2 * absPitch pRef - absPitch p))

invertAt1 :: Pitch -> Music (Pitch, a) -> Music (Pitch, a)
invertAt1 pRef = fmap (\(p, x) -> (pitch (2 * absPitch pRef - absPitch p), x))

invert :: Music Pitch -> Music Pitch
invert m = if null pRef
  then m  -- no pitches in the structure!
  else invertAt (head pRef) m
  where
    pRef = mFold pFun (++) (++) (flip const) m

    pFun :: Primitive a -> [a]  -- derived
    pFun (Note d p) = [p]
    pFun _          = []

invert1 :: Music (Pitch, a) -> Music (Pitch, a)
invert1 m = if null pRef
  then m  -- no pitches!
  else invertAt1 (head pRef) m
  where
    pRef = mFold pFun (++) (++) (flip const) m

    pFun :: Primitive (a, b) -> [a] -- derived
    pFun (Note d (p, x)) = [p]
    pFun _               = []

retro :: Music a -> Music a
retro n@(Prim _)   = n
retro (Modify c m) = Modify c (retro m)
retro (m1 :+: m2)  = retro m2 :+: retro m1
retro (m1 :=: m2)  = if d1 > d2
  then retro m1 :=: (rest (d1-d2) :+: retro m2)
  else (rest (d2-d1) :+: retro m1) :=: retro m2
  where
    d1 = dur m1
    d2 = dur m2

retroInvert, invertRetro :: Music Pitch -> Music Pitch
retroInvert = retro . invert
invertRetro = invert . retro

dur :: Music a -> Dur
dur (Prim (Note d _))    = d
dur (Prim (Rest d))      = d
dur (m1 :+: m2)          = dur m1   +   dur m2
dur (m1 :=: m2)          = dur m1 `max` dur m2
dur (Modify (Tempo r) m) = dur m / r
dur (Modify _ m)         = dur m

cut :: Dur -> Music a -> Music a
cut d _ | d <= 0           = rest 0
cut d (Prim (Note nd p))   = note (min nd d) p
cut d (Prim (Rest rd))     = rest (min rd d)
cut d (m1 :+: m2)          = m1' :+: cut (d - dur m1') m2 where m1' = cut d m1
cut d (m1 :=: m2)          = cut d m1 :=: cut d m2
cut d (Modify (Tempo r) m) = tempo r (cut (d*r) m)
cut d (Modify c m)         = Modify c (cut d m)

remove :: Dur -> Music a -> Music a
remove d m | d <= 0           = m
remove d (Prim (Note nd p))   = note (max (nd - d) 0) p
remove d (Prim (Rest rd))     = rest (max (rd - d) 0)
remove d (m1 :=: m2)          = remove d m1 :=: remove d m2
remove d (m1 :+: m2)          = remove d m1 :+: remove (d - dur m1) m2
remove d (Modify (Tempo r) m) = tempo r (remove (d*r) m)
remove d (Modify c m)         = Modify c (remove d m)

removeZeros :: Music a -> Music a
removeZeros (Prim p)     = Prim p
removeZeros (Modify c m) = Modify c (removeZeros m)
removeZeros (m1 :+: m2)  = case over both removeZeros (m1, m2) of
  (Prim (Note 0 p), m) -> m
  (Prim (Rest 0  ), m) -> m
  (m, Prim (Note 0 p)) -> m
  (m, Prim (Rest 0  )) -> m
  (m1, m2)             -> m1 :+: m2
removeZeros (m1 :=: m2)  = case over both removeZeros (m1, m2) of
  (Prim (Note 0 p), m) -> m
  (Prim (Rest 0  ), m) -> m
  (m, Prim (Note 0 p)) -> m
  (m, Prim (Rest 0  )) -> m
  (m1, m2)             -> m1 :=: m2

type LazyDur = [Dur]

durL :: Music a -> LazyDur
durL m@(Prim _)           = [dur m]
durL (m1 :+: m2)          = d1 ++ map (+ last d1) (durL m2) where d1 = durL m1
durL (m1 :=: m2)          = mergeLD (durL m1) (durL m2)
durL (Modify (Tempo r) m) = map (/ r) (durL m)
durL (Modify _         m) = durL m

mergeLD :: LazyDur -> LazyDur -> LazyDur
mergeLD [] ld = ld
mergeLD ld [] = ld
mergeLD ld1@(d1:ds1) ld2@(d2:ds2) = if d1 < d2
  then d1 : mergeLD ds1 ld2
  else d2 : mergeLD ld1 ds2

minL :: LazyDur -> Dur -> Dur
minL []       d2 = d2
minL [d1]     d2 = min d1 d2
minL (d1:ds1) d2 = if d1 < d2 then minL ds1 d2 else d2

cutL :: LazyDur -> Music a -> Music a
cutL []     _                = rest 0
cutL (d:ds) m | d <= 0       = cutL ds m
cutL ld (Prim (Note d p))    = note (minL ld d) p
cutL ld (Prim (Rest d))      = rest (minL ld d)
cutL ld (m1 :+: m2)          = m1' :+: cutL (map (subtract $ dur m1') ld) m2 where m1' = cutL ld m1
cutL ld (m1 :=: m2)          = cutL ld m1 :=: cutL ld m2
cutL ld (Modify (Tempo r) m) = tempo r (cutL (map (* r) ld) m)
cutL ld (Modify c m)         = Modify c (cutL ld m)

(/=:) :: Music a -> Music a -> Music a
m1 /=: m2 = cutL (durL m2) m1 :=: cutL (durL m1) m2

data PercussionSound
  = AcousticBassDrum  -- ^ MIDI Key 35
  | BassDrum1         -- ^ MIDI Key 36
  | SideStick         -- ^ ...
  | AcousticSnare | HandClap     | ElectricSnare | LowFloorTom
  | ClosedHiHat   | HighFloorTom | PedalHiHat    | LowTom
  | OpenHiHat     | LowMidTom    | HiMidTom      | CrashCymbal1
  | HighTom       | RideCymbal1  | ChineseCymbal | RideBell
  | Tambourine    | SplashCymbal | Cowbell       | CrashCymbal2
  | Vibraslap     | RideCymbal2  | HiBongo       | LowBongo
  | MuteHiConga   | OpenHiConga  | LowConga      | HighTimbale
  | LowTimbale    | HighAgogo    | LowAgogo      | Cabasa
  | Maracas       | ShortWhistle | LongWhistle   | ShortGuiro
  | LongGuiro     | Claves       | HiWoodBlock   | LowWoodBlock
  | MuteCuica     | OpenCuica    | MuteTriangle
  | OpenTriangle      -- ^ MIDI Key 82
  deriving (Show, Eq, Ord, Enum)

perc :: PercussionSound -> Dur -> Music Pitch
perc ps dur = instrument Percussion $ note dur $ pitch $ fromEnum ps + 35

-- | Sometimes we may wish to alter the internal structure of a Music value
-- rather than wrapping it with Modify. The following functions allow this.

shiftPitches :: AbsPitch -> Music Pitch -> Music Pitch
shiftPitches k = fmap (trans k)

shiftPitches1 :: AbsPitch -> Music (Pitch, b) -> Music (Pitch, b)
shiftPitches1 k = fmap (\(p, xs) -> (trans k p, xs))

scaleDurations :: Rational -> Music a -> Music a
scaleDurations r (Prim (Note d p)) = note (d/r) p
scaleDurations r (Prim (Rest d))   = rest (d/r)
scaleDurations r (m1 :+: m2)       = scaleDurations r m1 :+: scaleDurations r m2
scaleDurations r (m1 :=: m2)       = scaleDurations r m1 :=: scaleDurations r m2
scaleDurations r (Modify c m)      = Modify c (scaleDurations r m)

changeInstrument :: InstrumentName -> Music a -> Music a
changeInstrument i m = Modify (Instrument i) $ removeInstruments m

removeInstruments :: Music a -> Music a
removeInstruments (Modify (Instrument _) m) = removeInstruments m
removeInstruments (Modify c              m) = Modify c $ removeInstruments m
removeInstruments (m1 :+: m2)               = removeInstruments m1 :+: removeInstruments m2
removeInstruments (m1 :=: m2)               = removeInstruments m1 :=: removeInstruments m2
removeInstruments m                         = m
