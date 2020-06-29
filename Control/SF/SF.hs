{-# LANGUAGE BangPatterns, LambdaCase #-}

module Control.SF.SF where

import Prelude hiding (id, (.))

import Control.Arrow (Arrow (arr, first, (&&&), (***)), ArrowChoice (left), ArrowLoop (loop))
import Control.Arrow.Operations (ArrowCircuit (delay))
import Control.Category (Category (id, (.)))

import Control.Arrow.ArrowP ()


-- newtype SF a b = SF { runSF :: a -> (b, SF a b) }

newtype SF a b = SF (a -> (b, SF a b))

runSF :: SF a b -> a -> (b, SF a b)
runSF (SF x) = x


instance Category SF where
--id :: SF a a
  id = SF $ \ a -> (a, id)

--(.) :: SF b c -> SF a b -> SF a c
  SF f . SF g = SF $ \ a ->
    let (b, sfG') = g a
        (c, sfF') = f b
    in  seq sfF' $ seq sfG' (c, sfF' . sfG')


instance Arrow SF where
--arr :: (b -> c) -> SF b c
  arr f = SF (\ b -> (f b, arr f))

--first :: SF b c -> SF (b, d) (c, d)
  first (SF f) = SF $ \ (b, d) ->
    let (c, sf') = f b
    in  seq sf' ((c, d), first sf')

--(&&&) :: SF b c -> SF b c' -> SF b (c, c')
  SF f &&& SF g = SF $ \ b ->
    let (c,  sfF') = f b
        (c', sfG') = g b
    in ((c, c'), sfF' &&& sfG')

--(***) :: SF b c -> SF b' c' -> SF (b, b') (c, c')
  SF f *** SF g = SF $ \ (b, b') ->
    let (c,  sfF') = f b
        (c', sfG') = g b'
    in ((c, c'), sfF' *** sfG')


instance ArrowLoop SF where
--loop :: SF (b, d) (c, d) -> SF b c
  loop (SF f) = SF $ \ b ->
    let ((c, d), sf') = f (b, d)
    in  seq sf' (c, loop sf')


instance ArrowChoice SF where
--left :: SF b c -> SF (Either b d) (Either c d)
  left sf@(SF f) = SF $ \case
    Left  b -> seq sf' (Left  c, left sf') where (c, sf') = f b
    Right d ->         (Right d, left sf )


instance ArrowCircuit SF where
--delay :: b -> SF b b
  delay b = SF $ \ b' -> (b, delay b')


run :: SF a b -> [a] -> [b]
run _ [] = []
run (SF f) (a:as) =
  let (b, f') = f a
  in  seq b $ seq f' (b : run f' as)

unfold :: SF () a -> [a]
unfold = flip run inp where
  inp = () : inp


nth :: Int -> SF () a -> a
nth n (SF f) = seq a $ if n == 0 then a else nth (n - 1) sf' where
  (a, sf') = f ()

nth' :: Int -> (b, ((), b) -> (a, b)) -> a
nth' !n (b, f) = seq n $ seq b $ seq f $ aux n b where
  aux !n !b = seq a $ seq b' $ if n == 0 then a else aux (n-1) b' where
    (a, b') = f ((), b)
