module BangBang where

type DBConfig = BConfig Double Double

data (Ord a, Ord b) => BConfig a b = BConfig    {   threshold :: a,
                                                    lowValue :: b,
                                                    highValue :: b
                                                }

instance (Show a, Show b) => Show (BConfig a b) where
    show = show

bEvaluate :: (Ord a, Ord b) => BConfig a b -> a -> b
bEvaluate bcfg input
        | input >= limen        = high
        | otherwise             = low
        where
        limen = threshold bcfg
        low = lowValue bcfg
        high = highValue bcfg
