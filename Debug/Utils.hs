module Utils where
import System.Random
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

repApp f x = f (repApp f (x - 1))

neck :: [a] -> a
neck = (!! 1)

set :: a -> Integer -> [a] -> [a]
set x 1 (y:ys) = x:ys
set x n (y:ys) = y : set x (n-1) ys

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

sort :: Ord b => (a -> b) -> [a] -> [a]
sort = Data.List.Key.sort

componentize :: [a] -> Int -> ([a], a, [a])
componentize x i
    | i < 0             = error "Index less than zero!"
    | i >= length x     = error "Index greater than list length!"
    | otherwise         = (a, head b, tail b)
    where
    (a, b) = splitAt i x

decomponentize :: ([a], a, [a]) -> [a]
decomponentize (a, b, c) = a ++ [b] ++ c

replace :: [a] -> Int -> a -> [a]
replace x i new = decomponentize (a, new, b)
        where
        (a, _, b) = componentize x i

nan = (read "NaN"::Double)

lastfew :: (a, a, a, a, a, a) -> (a, a, a, a, a)
lastfew (_, a, b, c, d, e) = (a, b, c, d, e)
