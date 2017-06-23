import qualified Data.Map as Map
import Control.Monad
import System.Random
import qualified Data.Maybe as Maybe

data Prefix = Prefix String String deriving (Show, Eq, Ord)

buildKeys :: [String] -> [(Prefix, [String])]
buildKeys (x:y:z:xs) = (Prefix x y, [z]) : (buildKeys (y:z:xs))
buildKeys _          = []

buildMap :: [String] -> Map.Map Prefix [String]
buildMap x = Map.fromListWith mappend $ buildKeys x

pickWord :: Prefix -> Map.Map Prefix [String] -> Maybe (IO String)
pickWord p m = do
    as <- Map.lookup p m :: Maybe [String]
    let a = randomRIO (0, (length as)-1) >>= (return . (as!!))
    return a

gen :: Int -> Prefix -> Map.Map Prefix [String] -> Maybe (IO [String])
gen 0 _ _ = Just (return [])
gen n p@(Prefix pa pb) m = do
    nxt <- pickWord p m :: Maybe (IO String)
    let fol = nxt >>= (\x -> (g (gen (n-1) (Prefix pb x) m)) >>= (return .(x:)))  :: IO [String]
            where g (Nothing) = return []
                  g (Just a) = a
    return fol

main = getContents >>= (\inp -> p $ gen 10 (Prefix "" "") $ buildMap (["",""] ++ (words inp)))
        where p (Nothing) = putStrLn "Nothing"
              p (Just a) = a >>= print
