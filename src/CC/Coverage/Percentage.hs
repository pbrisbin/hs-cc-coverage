{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CC.Coverage.Percentage
    ( Percentage
    , toPercentage
    ) where

import Data.Aeson

newtype Percentage = Percentage Float
    deriving ToJSON

-- | Safe constructor
--
-- - Restricts values to @[0, 100]@
-- - Returns 100% for a 0 denominator (this is an arbitrary choice)
--
toPercentage :: (Integral a, Integral b) => a -> b -> Percentage
toPercentage n d
    | d == 0 = Percentage 100
    | f < 0 = Percentage 0
    | f > 100 = Percentage 100
    | otherwise = Percentage f
  where
    f = 100 * fromIntegral n / fromIntegral d
