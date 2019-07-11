{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Gamgee.Effects.TOTP
    ( -- * Effects
      TOTP (..)

      -- * Actions
    , getTOTP

      -- * Interpretations
    , runTOTP
    ) where

import qualified Crypto.Hash.Algorithms     as HashAlgos
import qualified Crypto.OTP                 as OTP
import qualified Data.ByteArray.Encoding    as Encoding
import qualified Data.Time.Clock.POSIX      as Clock
import           Gamgee.Effects.Crypto      (Crypto)
import qualified Gamgee.Effects.Crypto      as Crypto
import           Gamgee.Effects.SecretInput (SecretInput)
import qualified Gamgee.Token               as Token
import           Polysemy                   (Member, Members, Sem)
import qualified Polysemy                   as P
import qualified Polysemy.Error             as P
import           Relude
import qualified Text.Printf                as Printf


----------------------------------------------------------------------------------------------------
-- Effects
----------------------------------------------------------------------------------------------------

data TOTP m a where
  GetTOTP :: Token.TokenSpec -> Clock.POSIXTime -> TOTP m Text

P.makeSem ''TOTP


----------------------------------------------------------------------------------------------------
-- Interpret TOTP
----------------------------------------------------------------------------------------------------

runTOTP :: Members [SecretInput Text, Crypto, P.Error Text] r => Sem (TOTP : r) a -> Sem r a
runTOTP = P.interpret $ \case
  GetTOTP spec time -> do
    secret <- Crypto.decryptSecret spec
    case Encoding.convertFromBase Encoding.Base32 (encodeUtf8 secret :: ByteString) of
      Left msg  -> P.throw $ "Internal Error: secret of this token is corrupt: " <> toText msg
      Right key -> computeTOTP spec key time

computeTOTP :: Member (P.Error Text) r => Token.TokenSpec -> ByteString -> Clock.POSIXTime -> Sem r Text
computeTOTP spec key time =
  case Token.tokenAlgorithm spec of
    Token.AlgorithmSHA1   -> makeOTP <$> makeParams HashAlgos.SHA1
    Token.AlgorithmSHA256 -> makeOTP <$> makeParams HashAlgos.SHA256
    Token.AlgorithmSHA512 -> makeOTP <$> makeParams HashAlgos.SHA512

  where
    period :: Word16
    period = Token.unTokenPeriod $ Token.tokenPeriod spec

    digits :: OTP.OTPDigits
    digits = case Token.tokenDigits spec of
               Token.Digits6 -> OTP.OTP6
               Token.Digits8 -> OTP.OTP8

    makeParams :: (Member (P.Error Text) r, HashAlgos.HashAlgorithm h) => h -> Sem r (OTP.TOTPParams h)
    makeParams alg = either (P.throw . fromString) return $ OTP.mkTOTPParams alg 0 period digits OTP.NoSkew

    makeOTP :: (HashAlgos.HashAlgorithm h) => OTP.TOTPParams h -> Text
    makeOTP p = format $ OTP.totp p key $ floor time

    format :: OTP.OTP -> Text
    format otp =
      let (base, size) = case Token.tokenDigits spec of
                           Token.Digits6 -> (1000000, "6")
                           Token.Digits8 -> (100000000, "8")
      in fromString $ Printf.printf ("%0" ++ size ++ "d") (otp `mod` base)
