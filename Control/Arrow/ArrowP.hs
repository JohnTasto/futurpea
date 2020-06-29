{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Control.Arrow.ArrowP where

import Prelude hiding (id, (.))

import Control.Arrow (Arrow (arr, first), ArrowChoice (left, (|||)), ArrowLoop (loop))
import Control.Arrow.Operations (ArrowCircuit (delay))
import Control.Category (Category (id, (.)))


-- newtype ArrowP a p b c = ArrowP { strip :: a b c }

newtype ArrowP a p b c = ArrowP (a b c)

strip :: ArrowP a p b c -> a b c
strip (ArrowP x) = x


instance Category a => Category (ArrowP a p) where
--id :: ArrowP a p a1 a1
  id = ArrowP id

--(.) :: ArrowP a p b c -> ArrowP a p a1 b -> ArrowP a p a1 c
  ArrowP f . ArrowP g = ArrowP (f . g)


instance Arrow a => Arrow (ArrowP a p) where
--arr :: (b -> c) -> ArrowP a p b c
  arr f = ArrowP (arr f)

--first :: ArrowP a p b c -> ArrowP a p (b, d) (c, d)
  first (ArrowP f) = ArrowP (first f)


instance ArrowLoop a => ArrowLoop (ArrowP a p) where
--loop :: ArrowP a p (b, d) (c, d) -> ArrowP a p b c
  loop (ArrowP f) = ArrowP (loop f)


instance ArrowCircuit a => ArrowCircuit (ArrowP a p) where
--delay :: b -> ArrowP a p b b
  delay b = ArrowP (delay b)


instance ArrowChoice a => ArrowChoice (ArrowP a p) where
--left :: ArrowP a p b c -> ArrowP a p (Either b d) (Either c d)
  left (ArrowP f) = ArrowP (left f)

--(|||) :: ArrowP a p b d -> ArrowP a p c d -> ArrowP a p (Either b c) d
  ArrowP f ||| ArrowP g = ArrowP (f ||| g)
