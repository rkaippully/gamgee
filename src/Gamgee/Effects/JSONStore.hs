module Gamgee.Effects.JSONStore
  ( -- * Effect
    JSONStore (..)

    -- * Actions
  , jsonEncode
  , jsonDecode

    -- * Interpretation
  , runGamgeeJSONStore
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
-- Gamgee Configuration
----------------------------------------------------------------------------------------------------

data Config = Config {
  configVersion  :: Word32
  , configTokens :: Token.Tokens
  }
  deriving stock    (Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

currentConfigVersion :: Word32
currentConfigVersion = 1

initialConfig :: Config
initialConfig = Config {
  configVersion = currentConfigVersion
  , configTokens = fromList []
  }


----------------------------------------------------------------------------------------------------
-- Interpret JSONStore backed by a ByteStore
----------------------------------------------------------------------------------------------------

runJSONStore :: (o -> Sem (BS.ByteStore : r) Config) -- ^ Function to map object to a JSON serializable value
             -> (Config -> Sem (BS.ByteStore : r) o) -- ^ Function to map a JSON value to the object
             -> (String -> Sem (BS.ByteStore : r) o) -- ^ Function to handle errors in decoding
             -> Sem (JSONStore o : r) a
             -> Sem (BS.ByteStore : r) a
runJSONStore convertE convertD handleError =
  P.reinterpret $ \case
    JsonEncode o -> do
      j <- convertE o
      BS.writeByteStore $ Aeson.encode j
    JsonDecode   -> do
      bytes <- BS.readByteStore
      let
        config = maybe (Right initialConfig) Aeson.eitherDecode' bytes
      either handleError convertD config

runGamgeeJSONStore :: Member (P.Error Err.EffError) r => Sem (JSONStore Token.Tokens : r) a -> Sem (BS.ByteStore : r) a
runGamgeeJSONStore = runJSONStore tokensToConfig jsonToTokens handleDecodeError
  where
    tokensToConfig :: Token.Tokens -> Sem r Config
    tokensToConfig ts = return $ Config { configVersion = currentConfigVersion, configTokens = ts }

    jsonToTokens :: Member (P.Error Err.EffError) r => Config -> Sem r Token.Tokens
    jsonToTokens cfg = if configVersion cfg == currentConfigVersion
                       then return (configTokens cfg)
                       else P.throw $ Err.UnsupportedConfigVersion $ configVersion cfg

    handleDecodeError :: Member (P.Error Err.EffError) r => String -> Sem r a
    handleDecodeError msg = P.throw $ Err.JSONDecodeError $ toText msg
