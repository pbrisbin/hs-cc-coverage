{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CC.Coverage.Sha
    ( Sha
    , getSha
    , toSha
    , toShaThrow
    , toShaUnsafe
    ) where

import Control.Exception.Safe
import Data.Aeson
import Data.Char (isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T

-- | Type-safe @newtype@ for SHA values
newtype Sha = Sha { getSha :: Text }
    deriving ToJSON

-- | Safe constructor
--
-- Must be 40 hexidecimal characters.
--
toSha :: Text -> Either String Sha
toSha t
    | T.length t /= 40 = Left "not 40 characters"
    | T.any (not . isHexDigit) t = Left "non-hex characters"
    | otherwise = Right $ Sha t

-- | Throw the invalid case
toShaThrow :: MonadThrow m => Text -> m Sha
toShaThrow = either throwString pure . toSha

-- | Error in the invalid case
toShaUnsafe :: Text -> Sha
toShaUnsafe = either error id . toSha
