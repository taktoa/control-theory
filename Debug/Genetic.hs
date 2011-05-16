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
                             populationSize :: Integer,
                             genesPerChromosome :: Integer,
                             geneRange :: (Double, Double),
                             mutationRange :: Double
                        }

arithCrossover :: Chromosome -> Chromosome -> Chromosome
arithCrossover = zipWith avg

mutation' :: (RandomInt, RandomDouble) -> Chromosome -> IO Chromosome
mutation' (x, y) z = return (replace a y z)
    where
    a = fromIntegral x

mutation :: [Chromosome] -> GConfig -> IO [Chromosome]
mutation chrom cfg = do
        let mRange = mutationRange cfg
        let mweight = mutationWeight cfg
        loc1' <- randomInt (0, fromIntegral (length chrom))
        loc2' <- randomInt (0, fromIntegral (length chrom) - 1)
        mutagen <- randomDouble (0, 1/mweight)
        dRand <- randomDouble (-mRange, mRange)
        let rbool = (mutagen * mweight) > 0.5
        let (loc1, loc2) = (fromIntegral loc1', fromIntegral loc2')
        let rand = (loc2, dRand)
        mutated <- mutation' rand (chrom !! loc1)
        let output = replace loc2 mutated chrom
        return (if rbool then output else chrom)

selection :: [Chromosome] -> GConfig -> Population
selection chroms cfg = take (round (k * fromIntegral (length pop))) (ksort snd pop)
    where
    k = selectionWeight cfg
    pop = chromsToPop chroms cfg

crossover' :: Integer -> [Chromosome] -> [Chromosome]
crossover' 0 _ = []
crossover' n (a:b:c) = arithCrossover a b : crossover' (n - 1) c

crossover :: Population -> GConfig -> IO Population
crossover pop cfg = do
            filledIn <- fillInPop pop cfg
            rand <- randomInt (0, 1)
            let n = length filledIn `div` 2
            return (if rand == 1 then chromsToPop (crossover' (fromIntegral n) (popToChroms filledIn)) cfg else pop)

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
        neededSize = fromIntegral (populationSize cfg)
        popSize = fromIntegral (length pop)

popToChroms :: Population -> [Chromosome]
popToChroms = map fst

chromsToPop :: [Chromosome] -> GConfig -> Population
chromsToPop chroms cfg = zip chroms (map (fitnessFunc cfg) chroms)

makeInitChroms' :: GConfig -> [Chromosome] -> Integer -> IO [Chromosome]
makeInitChroms' _ xs 0 = return xs
makeInitChroms' cfg chroms x = do
            rlist <- randomDoubleList popSize range
            --randomFlush
            makeInitChroms' cfg (zipWith (:) rlist chroms) (x - 1)
            where
            GConfig _ _ _ _ pop _ range _ = cfg
            popSize = fromIntegral pop

makeInitChroms :: GConfig -> IO [Chromosome]
makeInitChroms cfg = makeInitChroms' cfg (replicate popsize []) gpc
            where
            gpc = genesPerChromosome cfg
            popsize = fromIntegral (populationSize cfg)

generation :: GConfig -> [Chromosome] -> IO [Chromosome]
generation cfg chroms = do
            mutated <- mutation chroms cfg
            let selected = selection mutated cfg
            crossedover <- crossover selected cfg
            fillIn <- (fillInPop crossedover cfg)
            return (popToChroms fillIn)

runGen' :: GConfig -> Integer -> [Chromosome] -> IO [Chromosome]
runGen' cfg 0 chroms = return chroms
runGen' cfg iters chrom = generation cfg chrom >>= runGen' cfg (iters - 1)

runGen :: Integer -> GConfig -> IO Chromosome
runGen iters cfg = do
            initchroms <- (makeInitChroms cfg)
            chroms <- runGen' cfg iters initchroms
            let spop = (ksort snd (chromsToPop chroms cfg))
            print initchroms
            print chroms
            print spop
            return (fst (head spop))
