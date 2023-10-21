{-# LANGUAGE TupleSections #-}
module Utils.Mealy where

import Data.Maybe

newtype Mealy i a = Mealy {runMealy :: i -> (a, Mealy i a)}

instance Functor (Mealy i) where
    fmap f (Mealy mm) = Mealy $ \i -> case mm i of
        (x, m) -> (f x, fmap f m)

instance Applicative (Mealy i) where
    pure x = let r = Mealy (const (x, r)) in r
    Mealy mff  <*> Mealy mxx = Mealy $ \i -> case mff i of
        (f, mf) -> case mxx i of
            (x, mx) -> (f x, mf <*> mx)

echoMealy :: Mealy i i
echoMealy = Mealy (,echoMealy)


scanMealy :: (a -> b -> a) -> a -> Mealy i b -> Mealy i a
scanMealy f z (Mealy mm) = Mealy $ \i -> case mm i of
    (x, m) -> let z2 = f z x in (z2, scanMealy f z2 m)


---------------------------------------------------------------------
-- MEALY UTILITIES

oldMealy :: a -> Mealy i a -> Mealy i (a,a)
oldMealy oldd = scanMealy (\(_,old) new -> (old,new)) (oldd,oldd)

latch :: Mealy i (Bool, a) -> Mealy i a
latch s = fromJust <$> scanMealy f Nothing s
    where f old (b,v) = Just $ if b then fromMaybe v old else v

iff :: Mealy i Bool -> Mealy i a -> Mealy i a -> Mealy i a
iff cc tt ff = (\c t f -> if c then t else f) <$> cc <*> tt <*> ff

-- decay'd division, compute a/b, with a decay of f
-- r' is the new result, r is the last result
-- r' ~= a' / b'
-- r' = r*b + f*(a'-a)
--      -------------
--      b + f*(b'-b)
-- when f == 1, r == r'
--
-- both streams must only ever increase
decay :: Double -> Mealy i Double -> Mealy i Double -> Mealy i Double
decay f aa bb = scanMealy step 0 $ (,) <$> oldMealy 0 aa <*> oldMealy 0 bb
    where step r ((a,a'),(b,b')) = if isNaN r then a' / b' else ((r*b) + f*(a'-a)) / (b + f*(b'-b))
