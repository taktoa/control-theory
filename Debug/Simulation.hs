module Simulation where
import Utils

data SState = SState {  t :: Double,
                        x :: Double,
                        theta :: Double,
                        v :: Double,
                        w :: Double,
                        f :: Double
                        } deriving (Eq, Show, Read)

data SConfig = SConfig { step :: Double,
                        xfunc :: SState -> SConfig -> Double,
                        thetafunc :: SState -> SConfig -> Double,
                        vfunc :: SState -> SConfig -> Double,
                        wfunc :: SState -> SConfig -> Double,
                        m1 :: Double,
                        m2 :: Double,
                        r :: Double,
                        g :: Double,
                        initialState :: SState }

type System = (SState, SConfig)

protoConfig :: (Double, Double, Double, Double, Double, SState) -> SConfig
protoConfig (step, m1, m2, r, g, initState) = SConfig step protoFX protoFTheta protoFV protoFW m1 m2 r g initState

protoState :: SState
protoState = SState 0 0 0 0 0 0

stateTuple :: SState -> (Double, Double, Double, Double, Double, Double)
stateTuple state = (t state, x state, theta state, v state, w state, f state)

tupleState :: (Double, Double, Double, Double, Double, Double) -> SState
tupleState tuple = state
    where
    (t, x, theta, v, w, f) = tuple
    state = SState t x theta v w f

update :: SConfig -> SState -> SState
update cfg state = SState t_ x_ theta_ v_ w_ f
    where
    h = step cfg
    (t, x, theta, v, w, f) = stateTuple state
    (f_x, f_theta, f_v, f_w) = (xfunc cfg, thetafunc cfg, vfunc cfg, wfunc cfg)
    h_ = h/2
    h__ = h/6
    s1 = tupleState (t, x, theta, v, w, f)
    s2 = tupleState (t + h_, x + k1a/2, theta + k1b/2, v + k1c/2, w + k1d/2, f)
    s3 = tupleState (t + h_, x + k2a/2, theta + k2b/2, v + k2c/2, w + k2d/2, f)
    s4 = tupleState (t + h, x + k3a, theta + k3b, v + k3c, w + k3d, f)
    k1a = h * f_x s1 cfg
    k1b = h * f_theta s1 cfg
    k1c = h * f_v s1 cfg
    k1d = h * f_w s1 cfg
    k2a = h * f_x s2 cfg
    k2b = h * f_theta s2 cfg
    k2c = h * f_v s2 cfg
    k2d = h * f_w s2 cfg
    k3a = h * f_x s3 cfg
    k3b = h * f_theta s3 cfg
    k3c = h * f_v s3 cfg
    k3d = h * f_w s3 cfg
    k4a = f_x s4 cfg
    k4b = f_theta s4 cfg
    k4c = f_v s4 cfg
    k4d = f_w s4 cfg
    x_ = x + (k1a + 2 * k2a + 2 * k3a + k4a) * h__
    theta_ = theta + (k1b + 2 * k2b + 2 * k3b + k4b) * h__
    v_ = v + (k1c + 2 * k2c + 2 * k3c + k4c) * h__
    w_ = w + (k1d + 2 * k2d + 2 * k3d + k4d) * h__
    t_ = t + h

protoFX :: SState -> SConfig -> Double
protoFX state cfg = v state

protoFTheta :: SState -> SConfig -> Double
protoFTheta state cfg = w state

protoFV :: SState -> SConfig -> Double
protoFV (SState t x theta v w f) cfg = (w - ((grav * sin theta) / radius)) / cos theta
    where
    grav = g cfg
    radius = r cfg

protoFW :: SState -> SConfig -> Double
protoFW (SState t x theta v w f) cfg = (((v^2 * (m' + m)) + f*v) / cos theta) - (w^2 * tan theta)
    where
    m = m1 cfg
    m' = m2 cfg
