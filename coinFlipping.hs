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

flipThreePrime:: P.Prob Bool
flipThreePrime =
  P.flatten $ case coin of
    (P.Prob axs) -> P.Prob $ map (\(ax, ap) ->
      ((\a -> P.flatten $ case coin of
        (P.Prob bxs) -> P.Prob $ map (\(bx, bp) ->
          ((\b -> P.flatten $ case loadedCoin of
            (P.Prob cxs) -> P.Prob $ map (\(cx, cp) ->
              ((\c -> P.Prob [(all (==Tails) [a, b, c], 1 % 1)]) cx, cp)
              ) cxs
          ) bx, bp)) bxs
      ) ax, ap)) axs
