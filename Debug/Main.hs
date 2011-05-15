import Simulation
import Neural
import PID
--import Fuzzy
import Genetic
import Utils
import Tests (simulTest, geneticTest, neuralTest)

evalSim :: NConfig -> System -> System
evalSim ncfg (state, scfg) = (updateState, scfg)
        where
        (t_, x_, theta_, v_, w_, f_) = stateTuple state
        stateList = [t_, x_, theta_, v_, w_, f_]
        f' = last (evaluate ncfg stateList)
        evalState = tupleState (t_, x_, theta_, v_, w_, f')
        updateState = update scfg evalState
--      b = theta state == nan
--      err = error "Simulation went out of bounds!"

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
        --putStr "Neural configuration: "
        --print ncfg
        print (map stateTuple (filter (\x -> abs (w x) < 40) (neuralSim scfg ncfg 300)))

pidFitness :: Integer -> SConfig -> (Double, Double, Double) -> Fitness
pidFitness iters scfg (i, d, p) = average (map theta (runPID iters pcfg))
        where
        func a b = update scfg b'
            where
            b' = SState (t b) (x b) (theta b) (v b) (w b) a
        input = theta
        set = initialState scfg
        pcfg = PConfig i d p (step scfg) func input set

pidGenetic :: SConfig -> IO (PConfig SState)
pidGenetic scfg = do
        let func a b = update scfg (SState (t b) (x b) (theta b) (v b) (w b) a)
        let (sWeight, mWeight, cWeight) = (0.05, 0.01, 0.7)
        let (gIters, pIters) = (500, 200)
        let iW [x, _, _] = x
        let dW [_, x, _] = x
        let pW [_, _, x] = x
        let fitness x = pidFitness pIters scfg (iW x, dW x, pW x)
        let (popSize, gpc) = (20, 3)
        let (gr, mr) = ((-10, 10), 2)
        let gcfg = GConfig fitness sWeight mWeight cWeight popSize gpc gr mr
        let set = initialState scfg
        out <- runGen gIters gcfg
        return (PConfig (iW out) (dW out) (pW out) (step scfg) func theta set)

runPIDSim :: IO ()
runPIDSim = do
        let initState = SState 0 0 0.1 0 0 0
        let scfg = protoConfig (0.125, 1, 1, 2, 10, initState)
        pcfg <- pidGenetic scfg
        let init = (map (lastfew . stateTuple) (filter (\x -> abs (w x) < 40) (runPID 300 pcfg)))
        print init
        --print pcfg

main = runPIDSim
