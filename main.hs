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
gen 0 _ _ = return []
gen n p m = do
    nxt <- pickWord p m
    fol <- if nxt==mempty && n<=5 then return [] else gen (n-1) ((tail p) ++ [nxt]) m
    return $ nxt:fol

ignoreBlanks a b
    | a=="" = b
    | b=="" = a
    | otherwise = a ++ " " ++ b

getLastTweets :: String -> Int -> IO [String]
getLastTweets userId n = do
    tweets <- T.getTweets userId 0 n
    return $ map T.text tweets

genTweetFromUser :: String -> IO String
genTweetFromUser user_id = do
    let n = 2
    sentences <- getLastTweets user_id 1000
    let mp = foldr (Map.unionWith mappend) Map.empty $ map (\x -> buildMap (words x) n) sentences
    alt <- gen 15 (replicate n "") mp >>= (return . (foldr ignoreBlanks ""))
    putStrLn alt
    return alt

main = genTweetFromUser "25073877" >>= T.tweet
