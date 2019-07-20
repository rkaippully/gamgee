module Gamgee.Effects.JSONStore
  ( -- * Effect
    JSONStore (..)

    -- * Actions
  , jsonEncode
  , jsonDecode

    -- * Interpretations
  , runJSONStore
  , configStoreToByteStore
  ) where

import qualified Data.Aeson               as Aeson
import qualified Gamgee.Effects.ByteStore as BS
import qualified Gamgee.Effects.Error     as Err
import qualified Gamgee.Token             as Token
import           Polysemy                 (Member, Sem)
import qualified Polysemy                 as P
import qualified Polysemy.Error           as P
import           Relude



----------------------------------------------------------------------------------------------------
-- An abstract JSON store effect that encodes and decodes JSON objects
----------------------------------------------------------------------------------------------------

data JSONStore o m a where
  JsonEncode :: o -> JSONStore o m ()
  JsonDecode :: JSONStore o m o

P.makeSem ''JSONStore


----------------------------------------------------------------------------------------------------
-- Interpret JSONStore backed by a ByteStore
----------------------------------------------------------------------------------------------------

-- | Reinterprets a JSONStore as a ByteStore
runJSONStore :: Member (P.Error Err.EffError) r
             => Sem (JSONStore Token.Tokens : r) a
             -> Sem (BS.ByteStore : r) a
runJSONStore = configStoreToByteStore . tokenStoreToConfigStore

tokenStoreToConfigStore :: Member (P.Error Err.EffError) r
                        => Sem (JSONStore Token.Tokens : r) a
                        -> Sem (JSONStore Token.Config : r) a
tokenStoreToConfigStore =
  P.reinterpret $ \case
    JsonEncode o -> tokensToConfig o >>= jsonEncode
    JsonDecode   -> jsonDecode >>= configToTokens

  where
    tokensToConfig :: Token.Tokens -> Sem r Token.Config
    tokensToConfig ts = return $ Token.Config { Token.configVersion = Token.currentConfigVersion, Token.configTokens = ts }

    configToTokens :: Member (P.Error Err.EffError) r => Token.Config -> Sem r Token.Tokens
    configToTokens cfg = if Token.configVersion cfg == Token.currentConfigVersion
                         then return (Token.configTokens cfg)
                         else P.throw $ Err.UnsupportedConfigVersion $ Token.configVersion cfg

configStoreToByteStore :: Member (P.Error Err.EffError) r
                       => Sem (JSONStore Token.Config : r) a
                       -> Sem (BS.ByteStore : r) a
configStoreToByteStore =
  P.reinterpret $ \case
    JsonEncode cfg -> BS.writeByteStore $ Aeson.encode cfg
    JsonDecode     -> do
      bytes <- BS.readByteStore
      let
        cfg = maybe (Right Token.initialConfig) Aeson.eitherDecode' bytes
      either handleDecodeError return cfg

  where
    handleDecodeError :: Member (P.Error Err.EffError) r => String -> Sem r a
    handleDecodeError msg = P.throw $ Err.JSONDecodeError $ toText msg
