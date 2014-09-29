import Data.Ratio
import Data.List (all)
import qualified Prob as P

data Coin = Heads | Tails deriving (Show, Eq)

coin :: P.Prob Coin
coin = P.Prob [(Heads, 1 % 2), (Tails, 1 % 2)]

loadedCoin :: P.Prob Coin
loadedCoin = P.Prob [(Heads, 1 % 10), (Tails, 9 % 10)]

flipThree :: P.Prob Bool
flipThree = do
  a <- coin
  b <- coin
  c <- loadedCoin
  return $ all (==Tails) [a, b, c]
