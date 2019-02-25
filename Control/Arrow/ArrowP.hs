module Control.Arrow.ArrowP where

import Prelude hiding (id, (.))

import Control.Arrow (Arrow (arr, first), ArrowChoice (left, (|||)), ArrowLoop (loop))
import Control.Arrow.Operations (ArrowCircuit (delay))
import Control.Category (Category (id, (.)))


newtype ArrowP a p b c = ArrowP { strip :: a b c }

instance Category a => Category (ArrowP a p) where
  id = ArrowP id
  ArrowP g . ArrowP f = ArrowP (g . f)

instance Arrow a => Arrow (ArrowP a p) where
  arr f = ArrowP (arr f)
  first (ArrowP f) = ArrowP (first f)

instance ArrowLoop a => ArrowLoop (ArrowP a p) where
  loop (ArrowP f) = ArrowP (loop f)

instance ArrowCircuit a => ArrowCircuit (ArrowP a p) where
  delay i = ArrowP (delay i)

instance ArrowChoice a => ArrowChoice (ArrowP a p) where
  left (ArrowP f) = ArrowP (left f)
  ArrowP f ||| ArrowP g = ArrowP (f ||| g)

