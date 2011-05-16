module BangBangImp where
import Simulation
import Genetic
import BangBang
import Utils
import System
import System.IO

evalSim :: DBConfig -> System -> System
evalSim bcfg (state, scfg) = (updateState, scfg)
        where
        SState t x theta v w f = state
        f' = bEvaluate bcfg theta
        evalState = SState t x theta v w f'
        updateState = update scfg evalState

bangSim :: SConfig -> DBConfig -> Int -> [SState]
bangSim scfg bcfg iters = map fst (take iters (iterate (evalSim bcfg) initSystem))
        where
        initSystem = (initialState scfg, scfg)

bangFitness :: Int -> SConfig -> DBConfig -> Fitness
bangFitness iters scfg bcfg = average (map theta (bangSim scfg bcfg iters))

bangGenetic :: SConfig -> IO DBConfig
bangGenetic scfg = do
        let (sWeight, mWeight, cWeight) = (0.05, 0.01, 0.7)
            (bIters, gIters) = (200, 300)
            chromToBConfig (a:b:c:_) = BConfig a b c
            fitness = bangFitness bIters scfg . chromToBConfig
            (popSize, gpc) = (20, 12)
            (gr, mr) = ((0, 2), 2)
            gcfg = GConfig fitness sWeight mWeight cWeight popSize gpc gr mr
        outchrom <- runGen gIters gcfg
        return (chromToBConfig outchrom)

runBangSim :: IO ()
runBangSim = do
        let initState = SState 0 0 0.1 0 0 0
            scfg = protoConfig (0.125, 1, 1, 2, 10, initState)
            bIters = 300
        bcfg <- bangGenetic scfg
        putStrLn "a"
        let bang = bangSim scfg bcfg bIters
            fitness = bangFitness bIters scfg bcfg
            disp = (average (map theta bang))
        args <- getArgs
        putStrLn "b"
        let fitnessFile:trackFile:_ = args
        fitnessHandle <- openFile fitnessFile WriteMode
        hPutStr fitnessHandle (show disp ++ ", " ++ show fitness ++ ", \"" ++ show bcfg ++ "\", ")
        hClose fitnessHandle
        putStrLn "c"
        trackHandle <- openFile trackFile WriteMode
        hPrint trackHandle bang
        hClose trackHandle
        putStrLn "d"
