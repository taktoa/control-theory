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
--      b = theta state == nan
--      err = error "Simulation went out of bounds!"

neuralSim :: SConfig -> NConfig -> Int -> [SState]
neuralSim scfg ncfg iters = map fst (take iters (iterate (evalSim ncfg) initSystem))
        where
        initSystem = (initialState scfg, scfg)

neuralFitness :: Int -> SConfig -> NConfig -> Fitness
neuralFitness iters scfg ncfg = average (map theta (neuralSim scfg ncfg iters))
