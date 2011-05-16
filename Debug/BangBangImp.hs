module BangBangImp where
import Simulation
import Genetic
import BangBang
import Utils

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
        let (bIters, gIters) = (2000, 1000)
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
        bcfg <- bangGenetic scfg
        print (map stateTuple (filter (\x -> abs (w x) < 40) (bangSim scfg bcfg 300)))
