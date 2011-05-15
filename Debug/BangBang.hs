module BangBang where

data (Ord a, Ord b) => BConfig a b = BConfig    {   threshold :: a,
                                                    lowValue :: b,
                                                    highValue :: b
                                                }

update :: (Ord a, Ord b) => a -> BConfig a b -> b
update input bcfg
        | input >= limen        = high
        | otherwise             = low
        where
        limen = threshold bcfg
        low = lowValue bcfg
        high = highValue bcfg
