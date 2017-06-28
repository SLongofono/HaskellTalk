import qualified Data.Map as Map
import Control.Monad
import Data.Monoid
import System.Random
import Data.List.Split
import qualified Twitter as T

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
          g (Just xs) = xs

gen :: (Monoid a, Ord a) => Int -> [a] -> Map.Map [a] [a] -> IO [a]
gen n p m = do
    nxt <- pickWord p m
    fol <- if nxt==mempty && n<=0 then return [mempty] else gen (n-1) ((tail p) ++ [nxt]) m
    return $ nxt:fol

ignoreBlanks a b
    | a=="" = b
    | b=="" = a
    | otherwise = a ++ " " ++ b

main = do
    fullTxt <- getContents
    let n = 2
    let sentences = lines fullTxt :: [String]
    let mp = foldr (Map.unionWith mappend) Map.empty $ map (\x -> buildMap (words x) n) sentences
    gen 20 (replicate n "") mp >>= (T.tweet . (foldr ignoreBlanks ""))
