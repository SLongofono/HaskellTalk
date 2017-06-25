import qualified Data.Map as Map
import Control.Monad
import Data.Monoid
import System.Random
import Data.List.Split

data Prefix = Prefix [String] deriving (Show, Eq, Ord)

buildKeys :: [String] -> [String] -> Int-> [(Prefix, [String])]
buildKeys [] x n = buildKeys (replicate n "") x n
buildKeys _ [] _ = []
buildKeys carry l@(x:xs) n
    | (length l) > n = (Prefix carry, [x]) : (buildKeys ((tail carry) ++ [x]) xs n)
    | otherwise = []

buildMap :: [String] -> Int -> Map.Map Prefix [String]
buildMap x n = Map.fromListWith mappend $ buildKeys [] x n

pickWord :: Prefix -> Map.Map Prefix [String] -> IO String
pickWord p m = let as = g $ Map.lookup p m in randomRIO (0, (length as)-1) >>= (return . (as!!))
    where g Nothing = [""]
          g (Just as) = as

gen :: Int -> Prefix -> Map.Map Prefix [String] -> IO [String]
gen 0 _ _ = return []
gen n p@(Prefix pa) m = do
    nxt <- pickWord p m :: IO String
    fol <- gen (n-1) (Prefix ((tail pa) ++ [nxt])) m
    return (nxt:fol)

main = do
    fullTxt <- getContents
    let n = 3
    let sentences = splitOn "." fullTxt :: [String]
    let mp = foldr (Map.unionWith (++)) Map.empty $ map (\x -> buildMap (words x) n) sentences
    gen 60 (Prefix (replicate n "")) mp >>= (putStrLn . unwords)
