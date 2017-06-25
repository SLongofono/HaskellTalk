import qualified Data.Map as Map
import Control.Monad
import Data.Monoid
import System.Random
import Data.List.Split

data Prefix = Prefix [String] deriving (Show, Eq, Ord)

buildKeys :: [String] -> Int-> [(Prefix, [String])]
buildKeys xs n
    | (length xs) > n = (Prefix (init pref), [last pref]) : (buildKeys (tail xs) n)
    | otherwise = []
        where pref = take (n+1) xs

buildMap :: [String] -> Int -> Map.Map Prefix [String]
buildMap x n = Map.fromListWith mappend $ buildKeys x n

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
    let n = 4
    let sentences = splitOn "." fullTxt :: [String]
    let mp = foldr (Map.unionWith (++)) Map.empty $ map (\x -> buildMap ((replicate n "") ++ (words x)) n) sentences
    gen 30 (Prefix (replicate n "")) mp >>= (putStrLn . unwords)
