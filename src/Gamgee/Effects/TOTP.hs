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
      Left _    -> P.throw "Internal Error: secret of this token is corrupt"
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







{-
-- | Generate the current OTP for a token
computeTOTP :: Monad m => Token.TokenSpec -> m OTPString
computeTOTP spec = do
  secret <- undefined -- encodeUtf8 <$> Crypto.getSecret spec
  case Encoding.convertFromBase Encoding.Base32 (secret :: ByteString) of
    Left _ -> error ("Secret of this token is corrupt" :: Text)
    Right key -> do
      hash <- getTOTP spec key
      return $ hashToOTP (Token.tokenDigits spec) hash

getTOTP :: Monad m => Token.TokenSpec -> ByteString -> m OTP.OTP
getTOTP spec key = do
  now <- undefined -- floor <$> Clock.getPOSIXTime
  let period = Token.unTokenPeriod $ Token.tokenPeriod spec
      digits = case Token.tokenDigits spec of
                Token.Digits6 -> OTP.OTP6
                Token.Digits8 -> OTP.OTP8

      mkParams :: (HashAlgos.HashAlgorithm h) => h -> Either String (OTP.TOTPParams h)
      mkParams alg = OTP.mkTOTPParams alg 0 period digits OTP.NoSkew

      mkOTP :: (HashAlgos.HashAlgorithm h) => OTP.TOTPParams h -> Either String OTP.OTP
      mkOTP p = return $ OTP.totp p key now

      result = case Token.tokenAlgorithm spec of
                Token.AlgorithmSHA1   -> mkParams HashAlgos.SHA1 >>= mkOTP
                Token.AlgorithmSHA256 -> mkParams HashAlgos.SHA256 >>= mkOTP
                Token.AlgorithmSHA512 -> mkParams HashAlgos.SHA512 >>= mkOTP
  either (error . fromString) return result

-- | Convert a 32-bit OTP to string format
hashToOTP :: Token.TokenDigits -> Word32 -> OTPString
hashToOTP digits hash =
  let (base, size) = case digits of
                      Token.Digits6 -> (1000000, "6")
                      Token.Digits8 -> (100000000, "8")
  in OTPString $ Printf.printf ("%0" ++ size ++ "d") (hash `mod` base)
-}
