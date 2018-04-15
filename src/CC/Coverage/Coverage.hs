module CC.Coverage.Coverage
    ( Coverage(..)
    , toCoveredPercent
    ) where

import CC.Coverage.Percentage
import Data.Aeson
import Numeric.Natural

data Coverage
    = Coverable Natural
    | Ignored

isCovered :: Coverage -> Bool
isCovered Ignored = False
isCovered (Coverable 0) = False
isCovered (Coverable _) = True

isCoverable :: Coverage -> Bool
isCoverable Ignored = False
isCoverable (Coverable _) = True

instance ToJSON Coverage where
    toJSON (Coverable n) = toJSON n
    toJSON Ignored = Null

toCoveredPercent :: [Coverage] -> Percentage
toCoveredPercent cs = toPercentage
    (length $ filter isCovered cs)
    (length $ filter isCoverable cs)
