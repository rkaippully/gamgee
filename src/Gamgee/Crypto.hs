-- | Cryptographic functions for securing tokens
module Gamgee.Crypto
    ( getSecret
    , encryptSecret
    ) where

import           Crypto.Cipher.AES      (AES256)
import qualified Crypto.Cipher.Types    as CT
import qualified Crypto.Error           as CE
import qualified Crypto.Random.Types    as CRT
import qualified Data.ByteArray         as ByteArray
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy   as LBS
import           Data.Maybe             (fromJust)
import qualified Gamgee.Token           as Token
import qualified Gamgee.Util            as Util
import           Universum


-- | A key for a given block cipher
data CipherKey c where
  CipherKey :: (CT.BlockCipher c) => c -> ByteString -> CipherKey c

-- | Generates a key for encryption based on a password
genSecretKey :: (CT.BlockCipher c) => Text -> c -> CipherKey c
genSecretKey password proxy = CipherKey proxy $ LBS.toStrict $ LBS.take 32 $ LBS.cycle $ encodeUtf8 password

-- | Generate a random initialization vector for a given block cipher
genRandomIV :: (CT.BlockCipher c) => c -> IO (CT.IV c)
genRandomIV proxy = do
  bytes <- CRT.getRandomBytes $ CT.blockSize proxy
  return $ fromJust $ CT.makeIV (bytes :: ByteString)

--- | Encrypt a Text message
encrypt :: (CT.BlockCipher c) => CipherKey c -> Text -> IO (ByteString, CT.IV c)
encrypt (CipherKey c key) msg = do
  iv <- genRandomIV c
  case CT.cipherInit key of
    CE.CryptoFailed e -> fail $ show e
    CE.CryptoPassed a -> return (CT.ctrCombine a iv (encodeUtf8 msg), iv)

--- | Decryption is symmetric
decrypt :: (CT.BlockCipher c) => CipherKey c -> CT.IV c -> ByteString -> IO Text
decrypt (CipherKey _ key) iv msg =
  case CT.cipherInit key of
    CE.CryptoFailed e -> fail $ show e
    CE.CryptoPassed a -> return $ decodeUtf8 $ CT.ctrCombine a iv msg

-- | Extract the secret from a token spec. This function will prompt the user for a password and use
-- that to decrypt the secret if needed.
getSecret :: Token.TokenSpec -> IO Text
getSecret spec =
  case Token.tokenSecret spec of
    Token.TokenSecretPlainText secret        -> return secret
    Token.TokenSecretAES256 ivText secret    ->
      case CT.makeIV <$> fromBase64 ivText of
        Right (Just iv) -> do
          password <- Util.getPassword "Password: "
          let key = genSecretKey password (undefined :: AES256)
          case fromBase64 secret of
            Right secret' -> decrypt key iv secret'
            Left _        -> fail "Invalid secret. Your config seems to be corrupt."
        _ -> fail "Invalid IV for secret. Your config seems to be corrupt."

-- | Encrypt the secret with a password given by the user
encryptSecret :: Token.TokenSpec -> IO Token.TokenSpec
encryptSecret spec =
  case Token.tokenSecret spec of
    -- Already encrypted
    Token.TokenSecretAES256 _ _ -> return spec
    -- Encrypt a plain text secret
    Token.TokenSecretPlainText secret -> do
      password <- Util.getPassword "Password to encrypt (leave blank to skip encryption): "
      if password == ""
      then return spec
      else do
        let key = genSecretKey password (undefined :: AES256)
        (encSecret, iv) <- encrypt key secret
        let secret' = Token.TokenSecretAES256 {
          Token.tokenSecretAES256IV = toBase64 $ ByteArray.convert iv
          , Token.tokenSecretAES256Data = toBase64 encSecret
          }
        return spec { Token.tokenSecret = secret' }

toBase64 :: ByteString -> Text
toBase64 = decodeUtf8 . Base64.encode

fromBase64 :: Text -> Either String ByteString
fromBase64 = Base64.decode . encodeUtf8
