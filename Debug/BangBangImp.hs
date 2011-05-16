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
        let (bIters, gIters) = (200, 500)
        let chromToBConfig (a:b:c:_) = BConfig a b c
        let fitness = bangFitness bIters scfg . chromToBConfig
        let (popSize, gpc) = (20, 12)
        let (gr, mr) = ((0, 2), 2)
        let gcfg = GConfig fitness sWeight mWeight cWeight popSize gpc gr mr
        outchrom <- runGen gIters gcfg
        return (chromToBConfig outchrom)

runBangSim :: IO ()
runBangSim = do
        let initState = SState 0 0 0.1 0 0 0
        let scfg = protoConfig (0.125, 1, 1, 2, 10, initState)
        let bIters = 300
        bcfg <- bangGenetic scfg
        let bang = bangSim scfg bcfg bIters
        let fitness = bangFitness bIters scfg bcfg
        let disp = (average (map theta bang))
        args <- getArgs
        let fitnessFile:trackFile:_ = args
        fitnessHandle <- openFile fitnessFile WriteMode
        hPutStr fitnessHandle (show disp ++ ", " ++ show fitness ++ ", \"" ++ show bcfg ++ "\", ")
        hClose fitnessHandle
        trackHandle <- openFile trackFile WriteMode
        hPrint fitnessHandle bang
        hClose trackHandle
