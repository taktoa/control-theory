----------------------------------------------------------------------------------------------------
-- Genetic.hs
----------------------------------------------------------------------------------------------------

module Genetic where
import Prelude
import Utils

type Gene = Double
type Chromosome = [Gene]
type Fitness = Double
type Organism = (Chromosome, Fitness)
type Population = [Organism]

data GConfig = GConfig  {    fitnessFunc :: Chromosome -> Fitness,
                             selectionWeight :: Double,
                             mutationWeight :: Double,
                             crossoverWeight :: Double,
                             populationSize :: Int,
                             genesPerChromosome :: Int,
                             geneRange :: (Double, Double),
                             mutationRange :: Double
                        }

arithCrossover :: Chromosome -> Chromosome -> Chromosome
arithCrossover = zipWith avg

mutation' :: (RandomInt, RandomDouble) -> Chromosome -> Chromosome
mutation' (x, y) z = replace' z x y

mutation :: [Chromosome] -> GConfig -> IO [Chromosome]
mutation chroms cfg = do
        geneLoc <- randomInt (0, gpc)
        popLoc <- randomInt (0, length chroms - 1)
        mutagen <- randomDouble (0, 1/mweight)
        dRand <- randomDouble (-mrange, mrange)
        let rbool = (mutagen * mweight) > 0.5
        let mutated = mutation' (geneLoc, dRand) (chroms !! popLoc)
        let output = replace' chroms popLoc mutated
        randomFlush
        return (if rbool then output else chroms)
        where
        GConfig _ _ mweight _ _ gpc _ mrange = cfg

selection :: [Chromosome] -> GConfig -> Population
selection chroms cfg = take (round (k * fromIntegral (length pop))) (ksort snd pop)
    where
    k = selectionWeight cfg
    pop = chromsToPop chroms cfg

crossover' :: Int -> [Chromosome] -> [Chromosome]
crossover' 0 _ = []
crossover' n (a:b:c) = arithCrossover a b : crossover' (n - 1) c

crossover :: Population -> GConfig -> IO Population
crossover pop cfg = do
            filledIn <- fillInPop pop cfg
            rand <- randomInt (0, 1)
            let n = length filledIn `div` 2
            return (if rand == 1 then chromsToPop (crossover' n (popToChroms filledIn)) cfg else pop)

fillInPop :: Population -> GConfig -> IO Population
fillInPop [] cfg = do
                    initchroms <- makeInitChroms cfg
                    return (chromsToPop initchroms cfg)
fillInPop pop cfg
        | popSize == neededSize         = return pop
        | popSize < neededSize          = return lessThan
        | popSize > neededSize          = return greaterThan
        where
        greaterThan = take neededSize sortedPop
        lessThan = take neededSize (replicate (neededSize - popSize) (head sortedPop) ++ sortedPop)
        sortedPop = ksort snd pop
        neededSize = populationSize cfg
        popSize = length pop

popToChroms :: Population -> [Chromosome]
popToChroms = map fst

chromsToPop :: [Chromosome] -> GConfig -> Population
chromsToPop chroms cfg = zip chroms (map (fitnessFunc cfg) chroms)

makeInitChroms' :: GConfig -> [Chromosome] -> Int -> IO [Chromosome]
makeInitChroms' _ xs 0 = return xs
makeInitChroms' cfg chroms x = do
            rlist <- randomDoubleList pop range
            randomFlush
            makeInitChroms' cfg (zipWith (:) rlist chroms) (x - 1)
            where
            GConfig _ _ _ _ pop _ range _ = cfg

makeInitChroms :: GConfig -> IO [Chromosome]
makeInitChroms cfg = makeInitChroms' cfg (replicate pop []) gpc
            where
            GConfig _ _ _ _ pop gpc _ _ = cfg

generation :: GConfig -> [Chromosome] -> IO [Chromosome]
generation cfg chroms = do
            mutated <- mutation chroms cfg
            let selected = selection mutated cfg
            crossedover <- crossover selected cfg
            fillIn <- (fillInPop crossedover cfg)
            return (popToChroms fillIn)

runGen' :: GConfig -> Int -> [Chromosome] -> IO [Chromosome]
runGen' cfg 0 chroms = return chroms
runGen' cfg iters chrom = generation cfg chrom >>= runGen' cfg (iters - 1)

runGen :: Int -> GConfig -> IO Chromosome
runGen iters cfg = do
            initchroms <- (makeInitChroms cfg)
            chroms <- runGen' cfg iters initchroms
            let spop = (ksort snd (chromsToPop chroms cfg))
            return (fst (head spop))
