module Utils where
import System.Random
import System
import Data.List.Key (sort)

type RandomInt = Integer
type RandomDouble = Double
type RandomIntList = [Integer]
type RandomDoubleList = [Double]
type RandomIntRange = (Integer, Integer)
type RandomDoubleRange = (Double, Double)

groups :: Int -> [a] -> [[a]]
groups n [] = []
groups n xs = let (g, rest) = splitAt n xs in g : groups n rest

penult :: [a] -> a
penult x = x !! (length x - 2)

neck :: [a] -> a
neck = (!! 1)

randomInt :: RandomIntRange -> IO RandomInt
randomInt = getStdRandom . randomR

randomDouble :: RandomDoubleRange -> IO RandomDouble
randomDouble = getStdRandom . randomR

randomIntList :: Int -> RandomIntRange -> IO RandomIntList
randomIntList size range = do
            stdGen <- getStdGen
            let randInfList = randomRs range stdGen
            return (take size randInfList)

randomDoubleList :: Int -> RandomDoubleRange -> IO RandomDoubleList
randomDoubleList size range = do
            stdGen <- getStdGen
            let randInfList = randomRs range stdGen
            return (take size randInfList)

avg :: (Num a, Fractional a) => a -> a -> a
avg x y = (x + y)/2

average a = sum a / fromIntegral (length a)

ksort :: Ord b => (a -> b) -> [a] -> [a]
ksort = Data.List.Key.sort

comp :: [a] -> Int -> ([a], a, [a])
comp x i
    | i < 0             = error "Index less than zero!"
    | i >= length x     = error "Index greater than list length!"
    | otherwise         = (a, head b, tail b)
    where
    (a, b) = splitAt i x

decomp :: ([a], a, [a]) -> [a]
decomp (a, b, c) = a ++ [b] ++ c

replace :: [a] -> Int -> a -> [a]
replace x i new = decomp (a, new, b)
        where
        (a, _, b) = comp x i

nan = read "NaN"::Double

lastfew :: (a, a, a, a, a, a) -> (a, a, a, a, a)
lastfew (_, a, b, c, d, e) = (a, b, c, d, e)

command :: String -> IO ()
command x = do
        _ <- system x
        return ()
