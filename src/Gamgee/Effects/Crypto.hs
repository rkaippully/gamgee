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

-- | Cryptographic effect for securing tokens
module Gamgee.Effects.Crypto
    ( -- * Effect
      Crypto(..)

      -- * Programs
    , encryptSecret
    , decryptSecret

      -- * Interpretations
    , runCrypto
    ) where

import           Crypto.Cipher.AES           (AES256)
import qualified Crypto.Cipher.Types         as CT
import qualified Crypto.Error                as CE
import qualified Data.ByteArray              as BA
import qualified Data.ByteString.Base64      as B64
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Text                   as Text
import qualified Gamgee.Effects.CryptoRandom as CR
import qualified Gamgee.Effects.Error        as Err
import qualified Gamgee.Effects.SecretInput  as SI
import qualified Gamgee.Token                as Token
import           Polysemy                    (Member, Members, Sem)
import qualified Polysemy                    as P
import qualified Polysemy.Error              as P
import           Relude


----------------------------------------------------------------------------------------------------
-- Effect
----------------------------------------------------------------------------------------------------

-- | Effect for encrypting and decrypting secrets
data Crypto m a where
  -- | Encrypts a secret with an optional password
  Encrypt :: Text                        -- ^ The secret to encrypt
          -> Text                        -- ^ The password
          -> Crypto m Token.TokenSecret
  -- | Decrypt a secret with an optional password
  Decrypt :: Text              -- ^ Base64 encoded IV
          -> Text              -- ^ Base64 encoded encrypted secret
          -> Text              -- ^ The password for decryption
          -> Crypto m Text     -- ^ Decrypted secret

P.makeSem ''Crypto


----------------------------------------------------------------------------------------------------
-- Programs
----------------------------------------------------------------------------------------------------

encryptSecret :: Members [SI.SecretInput Text, Crypto] r => Token.TokenSpec -> Sem r Token.TokenSpec
encryptSecret spec =
  case Token.tokenSecret spec of
    -- Secret is already encrypted
    Token.TokenSecretAES256 _ _ -> return spec
    Token.TokenSecretPlainText plainSecret -> do
      -- Ask the user for a password
      password <- SI.secretInput "Password to encrypt (leave blank to skip encryption): "

      if Text.null password
      then return spec
      else do
        -- Sometimes the secret may contain extraneous chars - '=', '-', space etc. Clear those.
        let secret = (Text.toUpper . Text.dropWhileEnd (== '=') . Text.replace " " "" . Text.replace "-" "" . Text.strip) plainSecret

        secret' <- encrypt secret password
        return spec { Token.tokenSecret = secret' }

decryptSecret :: Members [SI.SecretInput Text, Crypto] r => Token.TokenSpec -> Sem r Text
decryptSecret spec =
  case Token.tokenSecret spec of
    Token.TokenSecretPlainText plainSecret  -> return plainSecret
    Token.TokenSecretAES256 encIV encSecret -> do
      password <- SI.secretInput "Password: "
      decrypt encIV encSecret password


----------------------------------------------------------------------------------------------------
-- Interpretations
----------------------------------------------------------------------------------------------------

runCrypto :: Members [CR.CryptoRandom, P.Error Err.EffError] r => Sem (Crypto : r) a -> Sem r a
runCrypto = P.interpret $ \case
  Encrypt secret password -> do
    iv <- genRandomIV
    toTokenSecret iv secret password

  Decrypt encIV encSecret password -> fromTokenSecret encIV encSecret password

-- | Generate a random initialization vector
genRandomIV :: Members [P.Error Err.EffError, CR.CryptoRandom] r => Sem r (CT.IV AES256)
genRandomIV = do
  bytes <- CR.randomBytes $ CT.blockSize (error "Internal Error: This shouldn't be evaluated" :: AES256)
  case CT.makeIV (bytes :: ByteString) of
    Just iv -> return iv
    Nothing -> error "Internal Error: Unable to generate random initial vector"

-- | Generate an encrypted TokenSecret from an iv, a secret text and password
toTokenSecret :: Member (P.Error Err.EffError) r
              => CT.IV AES256
              -> Text                    -- ^ Secret
              -> Text                    -- ^ Password
              -> Sem r Token.TokenSecret
toTokenSecret iv secret password = do
  cipher <- CE.onCryptoFailure (P.throw . Err.CryptoError) return $ CT.cipherInit (passwordToKey password)
  return Token.TokenSecretAES256 {
    Token.tokenSecretAES256IV = toBase64 $ BA.convert iv
    , Token.tokenSecretAES256Data = toBase64 $ CT.ctrCombine cipher iv (encodeUtf8 secret)
    }

-- | Extract the secret text from a TokenSecret given its password
fromTokenSecret :: Member (P.Error Err.EffError) r
                => Text       -- ^ Base64 encoded IV
                -> Text       -- ^ Base64 encoded encrypted secret
                -> Text       -- ^ The password
                -> Sem r Text -- ^ IV and secret
fromTokenSecret encIV encSecret password = do
  iv <- fromBase64 encIV
  case CT.makeIV iv of
    Nothing  -> P.throw $ Err.CorruptIV iv
    Just iv' -> do
      secret <- fromBase64 encSecret
      cipher <- CE.onCryptoFailure (P.throw . Err.CryptoError) return $ CT.cipherInit (passwordToKey password)
      return $ decodeUtf8 $ CT.ctrCombine (cipher :: AES256) iv' secret

passwordToKey :: Text -> ByteString
passwordToKey password = toStrict $ LBS.take 32 $ LBS.cycle $ encodeUtf8 password

toBase64 :: ByteString -> Text
toBase64 = decodeUtf8 . B64.encode

fromBase64 :: Member (P.Error Err.EffError) r
           => Text
           -> Sem r ByteString
fromBase64 = either (P.throw . Err.CorruptBase64Encoding . toText) return .  B64.decode . encodeUtf8
