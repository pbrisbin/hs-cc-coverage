module CC.Coverage.Coverage
    ( Coverage(..)
    ) where

import Data.Aeson
import Numeric.Natural

data Coverage
    = Covered Natural
    | Uncovered

instance ToJSON Coverage where
    toJSON (Covered n) = toJSON n
    toJSON Uncovered = Null
