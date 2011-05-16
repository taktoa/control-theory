module NeuralImp where
import Genetic
import Simulation
import Neural
import Utils

evalSim :: NConfig -> System -> System
evalSim ncfg (state, scfg) = (updateState, scfg)
        where
        (t_, x_, theta_, v_, w_, f_) = stateTuple state
        stateList = [t_, x_, theta_, v_, w_, f_]
        f' = last (evaluate ncfg stateList)
        evalState = tupleState (t_, x_, theta_, v_, w_, f')
        updateState = update scfg evalState

neuralSim :: SConfig -> NConfig -> Int -> [SState]
neuralSim scfg ncfg iters = map fst (take iters (iterate (evalSim ncfg) initSystem))
        where
        initSystem = (initialState scfg, scfg)

neuralFitness :: Int -> SConfig -> NConfig -> Fitness
neuralFitness iters scfg ncfg = average (map theta (neuralSim scfg ncfg iters))

threshold, sigma, linear :: Double -> Transfer

threshold x y = if y > x then 1 else 0

sigma x y = 1 / (1 + exp (x - y))

linear x y
    | y + 0.5 < x   = 0
    | y - 0.5 > x   = 1
    | otherwise     = y - x + 0.5

makeTransfer :: Double -> Double -> Transfer
makeTransfer x
    | kind == 0     = threshold
    | kind == 1     = sigma
    | kind == 2     = linear
    | otherwise     = sigma
    where
    kind = round x

neuralGenetic :: SConfig -> IO NConfig
neuralGenetic scfg = do
        let gcfg = GConfig
        let (sWeight, mWeight, cWeight) = (0.05, 0.01, 0.7)
        let (nIters, gIters) = (1000, 800)
        let chromToNConfig (x:y:z) = NConfig weights transfer
                where
                transfer = makeTransfer x y
                weights = groups 5 z
        let fitness = neuralFitness nIters scfg . chromToNConfig
        let (popSize, gpc) = (20, 10)
        let (gr, mr) = ((0, 2), 2)
        let gcfg = GConfig fitness sWeight mWeight cWeight popSize gpc gr mr
        outchrom <- runGen gIters gcfg
        --putStr "Fitness: "
        --print (fitness outchrom)
        return (chromToNConfig outchrom)

runNeuralSim :: IO ()
runNeuralSim = do
        let initState = SState 0 0 0.1 0 0 0
        let scfg = protoConfig (0.125, 1, 1, 2, 10, initState)
        ncfg <- neuralGenetic scfg
        putStr "Neural configuration: "
        print ncfg
        print (map stateTuple (filter (\x -> abs (w x) < 40) (neuralSim scfg ncfg 300)))
