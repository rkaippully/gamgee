{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Gamgee.Effects.Error
  ( EffError(..)
  ) where

import qualified Crypto.Error as CE
import qualified Gamgee.Token as Token
import           Relude


-- | Errors returned by Gamgee effects
data EffError = AlreadyExists Token.TokenIdentifier
              | NoSuchToken Token.TokenIdentifier
              | CryptoError CE.CryptoError
              | CorruptIV ByteString
              | CorruptBase64Encoding Text
              | SecretDecryptError Text
              | InvalidTokenPeriod Token.TokenPeriod
              | UnsupportedConfigVersion Word32
              | JSONDecodeError Text
              deriving stock (Show, Eq)
