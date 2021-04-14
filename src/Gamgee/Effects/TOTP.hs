module Gamgee.Effects.TOTP
    ( -- * Effects
      TOTP (..)

      -- * Actions
    , getSecret
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
import qualified Gamgee.Effects.Error       as Err
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
  GetSecret :: Token.TokenSpec -> TOTP m Text
  GetTOTP   :: Token.TokenSpec -> Clock.POSIXTime -> TOTP m Text

P.makeSem ''TOTP


----------------------------------------------------------------------------------------------------
-- Interpret TOTP
----------------------------------------------------------------------------------------------------

runTOTP :: Members [SecretInput Text, Crypto, P.Error Err.EffError] r => Sem (TOTP : r) a -> Sem r a
runTOTP = P.interpret $ \case
  GetSecret spec    -> snd <$> retrieveKeyAndSecret spec
  GetTOTP spec time -> retrieveKeyAndSecret spec >>= computeTOTP spec time . fst

retrieveKeyAndSecret :: Members [SecretInput Text, Crypto, P.Error Err.EffError] r
                     => Token.TokenSpec
                     -> Sem r (ByteString, Text)
retrieveKeyAndSecret spec = do
  secret <- Crypto.decryptSecret spec
  case Encoding.convertFromBase Encoding.Base32 (encodeUtf8 secret :: ByteString) of
    Left msg  -> P.throw $ Err.SecretDecryptError $ toText msg
    Right key -> return (key, secret)

computeTOTP :: Member (P.Error Err.EffError) r => Token.TokenSpec -> Clock.POSIXTime -> ByteString -> Sem r Text
computeTOTP spec time key =
  case Token.tokenAlgorithm spec of
    Token.AlgorithmSHA1   -> makeOTP <$> makeParams HashAlgos.SHA1
    Token.AlgorithmSHA256 -> makeOTP <$> makeParams HashAlgos.SHA256
    Token.AlgorithmSHA512 -> makeOTP <$> makeParams HashAlgos.SHA512

  where
    period :: Token.TokenPeriod
    period = Token.tokenPeriod spec

    digits :: OTP.OTPDigits
    digits = case Token.tokenDigits spec of
               Token.Digits6 -> OTP.OTP6
               Token.Digits8 -> OTP.OTP8

    makeParams :: (Member (P.Error Err.EffError) r, HashAlgos.HashAlgorithm h) => h -> Sem r (OTP.TOTPParams h)
    makeParams alg = either (const (P.throw $ Err.InvalidTokenPeriod period))
                            return $
                            OTP.mkTOTPParams alg 0 (Token.unTokenPeriod period) digits OTP.NoSkew

    makeOTP :: (HashAlgos.HashAlgorithm h) => OTP.TOTPParams h -> Text
    makeOTP p = format $ OTP.totp p key $ floor time

    format :: OTP.OTP -> Text
    format otp =
      let (base, size) = case Token.tokenDigits spec of
                           Token.Digits6 -> (1000000, "6")
                           Token.Digits8 -> (100000000, "8")
      in fromString $ Printf.printf ("%0" ++ size ++ "d") (otp `mod` base)
