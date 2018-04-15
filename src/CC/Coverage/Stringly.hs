-- |
--
-- <https://github.com/codeclimate/test-reporter/blob/master/schema.json#L66>
--
module CC.Coverage.Stringly
    ( Stringly(..)
    ) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C8

newtype Stringly a = Stringly a

instance ToJSON a => ToJSON (Stringly a) where
    toJSON (Stringly a) = toJSON . C8.unpack $ encode a
