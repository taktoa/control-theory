module PID where
import Utils

data PConfig a = PConfig {      integralWeight :: Double,
                                derivWeight :: Double,
                                propWeight :: Double,
                                timeStep :: Double,
                                function :: Double -> a -> a,
                                toInput :: a -> Double,
                                setPoint :: a
                         }

instance (Show a) => Show (PConfig a) where
    show (PConfig i d p t _ _ s) = show ((i, d, p), (t, s))

pid :: [a] -> Integer -> PConfig a -> [a]
pid _ 0 _ = []
pid err n cfg = f (derivative + integral + proportional) (head err) : pid err (n - 1) cfg
    where
    k_i = integralWeight cfg
    k_d = derivWeight cfg
    k_p = propWeight cfg
    step = timeStep cfg
    f = function cfg
    inp = map (toInput cfg) err
    derivative = (head inp - neck inp) * k_d / step
    integral = sum inp * k_i
    proportional = k_p * head inp

runPID :: Integer -> PConfig a -> [a]
runPID iters cfg = pid initErr iters cfg
    where
    initErr = [set, set]
    set = setPoint cfg
