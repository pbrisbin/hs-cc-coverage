{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CC.Coverage.Percentage
    ( Percentage
    , toPercentage
    ) where

import Data.Aeson

newtype Percentage = Percentage Float
    deriving ToJSON

toPercentage :: (Integral a, Integral b) => a -> b -> Percentage
toPercentage n d
    | f < 0 = Percentage 0
    | f > 100 = Percentage 100
    | otherwise = Percentage f
  where
    f = 100 * fromIntegral n / fromIntegral d
