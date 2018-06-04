module Gamgee.OTP
    ( OTPString (..)
    , computeTOTP
    ) where

import qualified Crypto.Hash.Algorithms  as HashAlgos
import qualified Crypto.OTP              as OTP
import qualified Data.ByteArray.Encoding as Encoding
import qualified Data.Time.Clock.POSIX   as Clock
import qualified Gamgee.Crypto           as Crypto
import qualified Gamgee.Token            as Token
import qualified Gamgee.Util             as Util
import qualified Text.Printf             as Printf
import           Universum


newtype OTPString = OTPString { unOTPString :: String }
  deriving newtype (ToString)

-- | Generate the current OTP for a token
computeTOTP :: Token.TokenSpec -> IO OTPString
computeTOTP spec = do
  secret <- encodeUtf8 <$> Crypto.getSecret spec
  case Encoding.convertFromBase Encoding.Base32 (secret :: ByteString) of
    Left _ -> Util.exitWithMessage ("Secret of this token is corrupt" :: Text)
    Right key -> do
      hash <- getTOTP spec key
      return $ hashToOTP (Token.tokenDigits spec) hash

getTOTP :: Token.TokenSpec -> ByteString -> IO OTP.OTP
getTOTP spec key = do
  now <- Clock.getPOSIXTime >>= return . floor
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
  either fail return result

-- | Convert a 32-bit OTP to string format
hashToOTP :: Token.TokenDigits -> Word32 -> OTPString
hashToOTP digits hash =
  let (base, size) = case digits of
                      Token.Digits6 -> (1000000, "6")
                      Token.Digits8 -> (100000000, "8")
  in OTPString $ Printf.printf ("%0" ++ size ++ "d") (hash `mod` base)
