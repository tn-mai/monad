import Data.Ratio

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving (Show, Eq)

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
  where multAll (Prob innerxs, p) = map (\(x, p') -> (x, p * p')) innerxs

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

instance Monad Prob where
  return x = Prob [(x, 1 % 1)]
  x >>= f = flatten $ fmap f x
  fail _ = Prob []
