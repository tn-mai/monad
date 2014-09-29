import Data.Ratio

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving (Show, Eq)

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs
