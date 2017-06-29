import qualified Data.Map as Map
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import System.Random
import qualified Twitter as T

buildKeys :: [String] -> [String] -> Int-> [([String], [String])]
buildKeys [] x n = buildKeys (replicate n mempty) x n
buildKeys _ [] _ = []
buildKeys carry l@(x:xs) n
    | (length l) > n = (carry, [x]) : (buildKeys ((tail carry) ++ [x]) xs n)
    | otherwise = []

buildMap :: [String] -> Int -> Map.Map [String] [String]
buildMap x n = Map.fromListWith mappend $ buildKeys [] x n

pickWord :: [String] -> Map.Map [String] [String] -> IO String
pickWord p m = let as = g $ Map.lookup p m in randomRIO (0, (length as)-1) >>= (return . (as!!))
    where g Nothing = [mempty]
          g (Just xs) = xs

gen :: Int -> [String] -> Map.Map [String] [String] -> IO [String]
gen 0 _ _ = return []
gen n p m = do
    nxt <- pickWord p m :: IO String
    fol <- case nxt of
            [] -> gen n (tail p ++ [""]) m
            xs | (last xs)=='.' && n<=15 -> return []
               | (last xs)==',' && n<=10 -> return []
               | otherwise -> gen (n-1) ((tail p) ++ [xs]) m
    return $ nxt:fol

ignoreBlanks a b
    | a=="" = b
    | b=="" = a
    | otherwise = a ++ " " ++ b

getLastTweets :: String -> Int -> MaybeT IO [String]
getLastTweets userId n = do
    tweets <- T.getTweets userId n
    return $ map T.text tweets

genTweetFromUser :: String -> IO (Maybe String)
genTweetFromUser user_id = runMaybeT $ do
    let n = 2
    sentences <- getLastTweets user_id 1000
    let mp = foldr (Map.unionWith mappend) Map.empty $ map (\x -> buildMap (words x) n) sentences
    liftIO $ gen 20 (replicate n "") mp >>= (return . (foldr ignoreBlanks ""))

main = genTweetFromUser "25073877" >>= \x -> case x of Just a -> print a
                                                       Nothing -> print "Error"
