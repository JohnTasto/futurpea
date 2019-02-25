module Euterpea.IO.Audio
  ( module Euterpea.IO.Audio.BasicSigFuns,
    module Euterpea.IO.Audio.Basics,
    module Euterpea.IO.Audio.Types,
    module Euterpea.IO.Audio.IO,
    module Euterpea.IO.Audio.Render,
    writeWav,
    writeWavNorm
  ) where

import Euterpea.IO.Audio.Basics (apToHz, countDown, countUp, integral, outA, pchToHz, upsample)
import Euterpea.IO.Audio.BasicSigFuns
  ( PluckDecayMethod (RecursiveFilter, SimpleAveraging, SimpleDrum, StretchedAveraging, StretchedDrum, WeightedAveraging)
  , Table
  , balance
  , countTime
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
  , milliseconds
  , noiseBLH
  , noiseBLI
  , noiseWhite
  , osc
  , oscDur
  , oscDurI
  , oscFixed
  , oscI
  , oscPartials
  , pluck
  , samples
  , seconds
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
import Euterpea.IO.Audio.IO (maxSample, outFile, outFileNorm)
import Euterpea.IO.Audio.Render (Instr, InstrMap, renderSF)
import Euterpea.IO.Audio.Types
  ( AudRate
  , AudioSample (collapse, mix, numChans, zero)
  , Clock (rate)
  , CtrRate
  , Mono
  , SigFun
  , Signal
  , Stereo
  )

writeWav fname iMap m =
    let (d,s) = renderSF m iMap
    in  outFile fname d s

writeWavNorm fname iMap m =
    let (d,s) = renderSF m iMap
    in  outFileNorm fname d s

