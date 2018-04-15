module CC.Coverage.Timestamp
    ( Timestamp(..)
    ) where

import Data.Aeson
import Data.Time
import Data.Time.Clock.POSIX

newtype Timestamp = Timestamp UTCTime

instance ToJSON Timestamp where
    toJSON (Timestamp t) = toJSON $ utcTimeToPOSIXSeconds t
