import Simulation
import PID
import NeuralImp
import Genetic
import Utils

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

main = runNeuralSim
