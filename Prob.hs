module Prob
( Prob (Prob)
, getProb
, integrate
, flatten
) where
  import Data.Ratio
  import Data.List

  newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving (Show, Eq)
  
  flatten :: Prob (Prob a) -> Prob a
  flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs, p) = map (\(x, p') -> (x, p * p')) innerxs

  integrate :: (Eq a) => Prob a -> Prob a
  integrate (Prob xs) = Prob $ foldr insertProb [] xs
    where
      insertProb :: (Eq a, Num b) => (a, b) -> [(a, b)] -> [(a, b)]
      insertProb (x, p) v =
        case (findIndex (\(y, q) -> x == y) v) of
          Nothing -> (x, p):v
          Just n  -> let (l, r) = splitAt n v
                     in l ++ [(x, p + (snd $ head r))] ++ (tail r)

  instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs
  
  instance Monad Prob where
    return x = Prob [(x, 1 % 1)]
    x >>= f = flatten $ fmap f x
    fail _ = Prob []
