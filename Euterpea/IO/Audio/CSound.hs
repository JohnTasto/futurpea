{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |This module is strictly for backward compatibility with Euterpea 0.1.0,
-- which used many csound names for the basic signal functions.
module Euterpea.IO.Audio.CSound where

import Control.Arrow (ArrowChoice)
import Control.Arrow.Operations (ArrowCircuit)

import Control.Arrow.ArrowP (ArrowP)
import Control.SF.SF (SF)
import Euterpea.IO.Audio.Basics (outA)
import Euterpea.IO.Audio.BasicSigFuns
  ( Table
  , delayLine
  , delayLine1
  , delayLineT
  , envASR
  , envCSEnvlpx
  , envExpon
  , envExponSeg
  , envLine
  , envLineSeg
  , filterBandPass
  , filterBandPassBW
  , filterBandStop
  , filterBandStopBW
  , filterComb
  , filterHighPass
  , filterHighPassBW
  , filterLowPass
  , filterLowPassBW
  , noiseBLH
  , noiseBLI
  , noiseWhite
  , osc
  , oscDur
  , oscDurI
  , oscFixed
  , oscI
  , oscPartials
  , tableBessel
  , tableBesselN
  , tableExpon
  , tableExponN
  , tableLinear
  , tableLinearN
  , tableSines
  , tableSines3
  , tableSines3N
  , tableSinesN
  )
import Euterpea.IO.Audio.Types (Clock, Signal)

gen05    = tableExponN
gen05'   = tableExpon
gen07    = tableLinearN
gen07'   = tableLinear
gen09    = tableSines3N
gen09'   = tableSines3
gen10    = tableSinesN
gen10'   = tableSines
gen12    = tableBesselN
gen12'   = tableBessel

compSine1    = tableSinesN
compSine2    = tableSines3N
exponential1 = tableExponN
lineSeg1     = tableLinearN

tone, atone :: Clock p => Signal p (Double, Double) Double
tone     = filterLowPass
atone    = filterHighPass

reson, areson :: Clock p => Int -> Signal p (Double, Double, Double) Double
reson    = filterBandPass
areson   = filterBandStop

butterlp, butterhp:: Clock p => Signal p (Double, Double) Double
butterlp = filterLowPassBW
butterhp = filterHighPassBW

butterbp, butterbr :: Clock p => Signal p (Double, Double, Double) Double
butterbp = filterBandPassBW
butterbr = filterBandStopBW

comb :: Clock p => Double -> Signal p (Double, Double) Double
comb     = filterComb

oscil :: (Clock p, ArrowCircuit a) => Table -> Double -> ArrowP a p Double Double
oscil    = osc

oscili :: (Clock p, ArrowCircuit a) => Table -> Double -> ArrowP a p Double Double
oscili   = oscI

oscils :: (Clock p, ArrowCircuit a) => Double -> ArrowP a p Double Double
oscils f = proc a -> do
            o <- oscFixed f -< ()
            outA -< o * a

oscil1 :: (Clock p, ArrowChoice a, ArrowCircuit a)
  => Table
  -> Double
  -> Double
  -> ArrowP a p Double Double
oscil1 tab del dur =
          proc a -> do
            o <- oscDur  tab del dur -< ()
            outA -< o*a

oscil1i :: (Clock p, ArrowChoice a, ArrowCircuit a)
  => Table
  -> Double
  -> Double
  -> ArrowP a p Double Double
oscil1i tab del dur =
          proc a -> do
            o <- oscDurI tab del dur -< ()
            outA -< o*a

buzz :: Clock p
  => Table
  -> Double
  -> Signal p (Double, Int) Double
buzz     = oscPartials

-- pluck    = pluck
-- balance  = balance

line :: Clock p => Double -> Double -> Double -> ArrowP SF p Double Double
line a d b =
  proc s -> do
    o <- envLine a d b -< ()
    outA -< o*s

expon :: Clock p => Double -> Double -> Double -> ArrowP SF p Double Double
expon a d b =
  proc s -> do
    o <- envExpon a d b -< ()
    outA -< o*s

linseg, expseg :: Clock p => [Double] -> [Double] -> Signal p () Double
linseg   = envLineSeg
expseg   = envExponSeg

linen :: Clock p => Double -> Double -> Double -> ArrowP SF p Double Double
linen rise dur dec =
  proc s -> do
    o <- envASR rise dur dec -< ()
    outA -< o*s

envlpx :: Clock p =>
     Double
  -> Double
  -> Double
  -> Table
  -> Double
  -> Double
  -> ArrowP SF p Double Double
envlpx rise dur dec tab atss atdec =
  proc s -> do
    o <- envCSEnvlpx rise dur dec tab atss atdec -< ()
    outA -< o*s

rand :: Int -> ArrowP SF p Double Double
rand s =
  proc a -> do
    o <- noiseWhite s -< ()
    outA -< o*a

randi :: Clock p => Int -> ArrowP SF p (Double, Double) Double
randi s =
  proc (a,f) -> do
    o <- noiseBLI s -< f
    outA -< o*a

randh :: Clock p => Int -> ArrowP SF p (Double, Double) Double
randh s =
  proc (a,f) -> do
    o <- noiseBLH s -< f
    outA -< o*a

delay :: Clock p => Double -> Signal p Double Double
delay  = delayLine

vdelay :: Clock p => Double -> Signal p (Double, Double) Double
vdelay = delayLine1

delay1 :: Clock p => Double -> Signal p (Double, Double) Double
delay1 = delayLine1

delayT :: Clock p => Int -> Table -> Signal p Double Double
delayT = delayLineT
