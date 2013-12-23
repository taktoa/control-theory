module PID where
import Utils

data PConfig = PConfig   {      propWeight :: Double,
                                integralWeight :: Double,
                                derivWeight :: Double,
                                timeStep :: Double
                         }

instance (Show a) => Show (PConfig a) where
    show (PConfig i d p t _ _ s) = show ((i, d, p), (t, s))

pid :: [a] -> Int -> PConfig a -> [a]
pid _ 0 _ = []
pid err n cfg = f (derivative + integral + proportional) (head err) : pid err (n - 1) cfg
    where
    PConfig k_i k_d k_p step = cfg
    f = function cfg
    inp = map (toInput cfg) err
    derivative = (head inp - neck inp) * k_d / step
    integral = sum inp * k_i
    proportional = k_p * head inp

runPID :: Int -> PConfig a -> [a]
runPID iters cfg = pid initErr iters cfg
    where
    initErr = [set, set]
    set = setPoint cfg

pEvaluate :: PConfig -> PState -> PState
pEvaluate pcfg state = PState current int deriv (p + i + d)
    where
    PConfig pW iW dW step = pcfg
    PState current int deriv _ = state
    (p, i, d) = (pW * current, iW * int, dW * deriv)
