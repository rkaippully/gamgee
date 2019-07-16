{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Data structures to define and manipulate tokens
module Gamgee.Token
    ( TokenType (..)
    , TokenLabel (..)
    , TokenSecret (..)
    , TokenIssuer (..)
    , TokenAlgorithm (..)
    , TokenDigits (..)
    , TokenPeriod (..)
    , TokenSpec (..)
    , TokenIdentifier (..)
    , Tokens
    , getIdentifier
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.Text  as Text
import           Relude


-- | Type of token TOTP or HOTP (not supported yet)
data TokenType = TOTP
  deriving stock Show

instance Aeson.FromJSON TokenType where
  parseJSON (Aeson.String "totp") = return TOTP
  parseJSON invalid               = fail $ "Invalid token type: " ++ show invalid

instance Aeson.ToJSON TokenType where
  toJSON TOTP = Aeson.String "totp"

-- | Label of the token
newtype TokenLabel = TokenLabel {
  unTokenLabel :: Text
  }
  deriving newtype (Show, IsString, Aeson.FromJSON, Aeson.ToJSON)

-- | Secret used to generate OTPs
data TokenSecret = TokenSecretPlainText Text
                 | TokenSecretAES256 {
                     tokenSecretAES256IV     :: Text
                     , tokenSecretAES256Data :: Text
                     }
                 deriving stock    (Show, Generic)
                 deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

-- | Optional issuer of this token
newtype TokenIssuer = TokenIssuer {
  unTokenIssuer :: Text
  }
  deriving newtype (Show, IsString, Aeson.FromJSON, Aeson.ToJSON)

data TokenAlgorithm = AlgorithmSHA1
                    | AlgorithmSHA256
                    | AlgorithmSHA512
                    deriving stock    (Show, Generic)
                    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data TokenDigits = Digits6
                 | Digits8
                 deriving stock Show

instance Aeson.FromJSON TokenDigits where
  parseJSON (Aeson.Number 6) = return Digits6
  parseJSON (Aeson.Number 8) = return Digits8
  parseJSON invalid          = fail $ "Invalid number of digits: " ++ show invalid ++ ". Must be 6 or 8."

instance Aeson.ToJSON TokenDigits where
  toJSON Digits6 = Aeson.Number 6
  toJSON Digits8 = Aeson.Number 8

-- | Refresh interval of the token in seconds
newtype TokenPeriod = TokenPeriod {
  unTokenPeriod :: Word16
  }
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Show, Aeson.FromJSON, Aeson.ToJSON)

data TokenSpec = TokenSpec {
  -- ^ TOTP/HOTP token
  tokenType        :: TokenType
  -- ^ A short unique label for this token used to identify it
  , tokenLabel     :: TokenLabel
  -- ^ The secret provided by the issuer to generate tokens
  , tokenSecret    :: TokenSecret
  -- ^ The name of the issuer
  , tokenIssuer    :: TokenIssuer
  -- ^ SHA algorithm used to generate tokens
  , tokenAlgorithm :: TokenAlgorithm
  -- ^ Number of digits in the token - 6 or 8
  , tokenDigits    :: TokenDigits
  -- ^ Refresh interval of the token - typically 30 sec
  , tokenPeriod    :: TokenPeriod
  }
  deriving stock    (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

-- An identifier for a token. This is derived from the label and issuer
newtype TokenIdentifier = TokenIdentifier {
  unTokenIdentifier :: Text
  }
  deriving newtype (Eq, Show, Hashable, IsString, Semigroup, ToString
                   , Aeson.FromJSON, Aeson.ToJSON
                   , Aeson.FromJSONKey, Aeson.ToJSONKey)

getIdentifier :: TokenSpec -> TokenIdentifier
getIdentifier spec =
  let
    TokenLabel label = tokenLabel spec
    TokenIssuer issuer = tokenIssuer spec
  in
    TokenIdentifier $ if | Text.null issuer                      -> label
                         | Text.isPrefixOf (issuer <> ":") label -> label
                         | otherwise                             -> issuer <> ":" <> label

type Tokens = HashMap TokenIdentifier TokenSpec
