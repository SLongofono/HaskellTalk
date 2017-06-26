import qualified Data.Map as Map
import Control.Monad
import Data.Monoid
import System.Random
import Data.List.Split

buildKeys :: (Monoid a, Ord a) => [a] -> [a] -> Int-> [([a], [a])]
buildKeys [] x n = buildKeys (replicate n mempty) x n
buildKeys _ [] _ = []
buildKeys carry l@(x:xs) n
    | (length l) > n = (carry, [x]) : (buildKeys ((tail carry) ++ [x]) xs n)
    | otherwise = []

buildMap :: (Monoid a, Ord a)=> [a] -> Int -> Map.Map [a] [a]
buildMap x n = Map.fromListWith mappend $ buildKeys [] x n

pickWord :: (Monoid a, Ord a) => [a] -> Map.Map [a] [a] -> IO a
pickWord p m = let as = g $ Map.lookup p m in randomRIO (0, (length as)-1) >>= (return . (as!!))
    where g Nothing = [mempty]
          g (Just as) = as

gen :: (Monoid a, Ord a) => Int -> [a] -> Map.Map [a] [a] -> IO [a]
gen 0 _ _ = return []
gen n p m = do
    nxt <- pickWord p m
    fol <- gen (n-1) ((tail p) ++ [nxt]) m
    return (nxt:fol)

main = do
    fullTxt <- getContents
    let n = 2
    let sentences = lines fullTxt :: [String]
    let mp = foldr (Map.unionWith (++)) Map.empty $ map (\x -> buildMap (words x) n) sentences
    gen 60 (replicate n "") mp >>= (putStrLn . unwords)
    --print $ mp
