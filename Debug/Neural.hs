module Neural where
import Utils

type Weight = Double
type Input = Double
type Output = Double
type Transfer = Double -> Double

data NConfig = NConfig { weights :: [[Weight]], transfer :: Transfer }
instance Show NConfig where
    show (NConfig x y) = show x

nEvaluate :: NConfig -> [Input] -> [Output]
nEvaluate cfg inputs = map (\ws -> transfer cfg . sum $ zipWith (*) ws inputs) (weights cfg)
