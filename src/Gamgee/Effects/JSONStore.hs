{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

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
import qualified Data.Aeson.Types         as Aeson
import qualified Gamgee.Effects.ByteStore as BS
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

runJSONStore :: (Aeson.ToJSON j)
             => (o -> Sem (BS.ByteStore : r) j)           -- ^ Function to map object to a JSON serializable value
             -> (Aeson.Value -> Sem (BS.ByteStore : r) o) -- ^ Function to map a JSON value to the object
             -> (String -> Sem (BS.ByteStore : r) o)      -- ^ Function to handle errors in decoding
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
        config :: Either String Aeson.Value
        config = maybe (Right $ Aeson.toJSON initialConfig) Aeson.eitherDecode' bytes
      either handleError convertD config

runGamgeeJSONStore :: Member (P.Error Text) r => Sem (JSONStore Token.Tokens : r) a -> Sem (BS.ByteStore : r) a
runGamgeeJSONStore = runJSONStore tokensToConfig jsonToTokens handleDecodeError
  where
    tokensToConfig :: Token.Tokens -> Sem r Config
    tokensToConfig ts = return $ Config { configVersion = currentConfigVersion, configTokens = ts }

    jsonToTokens :: Member (P.Error Text) r => Aeson.Value -> Sem r Token.Tokens
    jsonToTokens v =
      case Aeson.parseEither Aeson.parseJSON v of
        Left err  -> P.throw $ "Internal Error: Could not parse Gamgee config file: " <> fromString err
        Right cfg -> if configVersion cfg == currentConfigVersion
                     then return (configTokens cfg)
                     else P.throw $ "Internal Error: Unsupported config version: " <> show (configVersion cfg)

    handleDecodeError :: Member (P.Error Text) r => String -> Sem r a
    handleDecodeError msg = P.throw $ "Internal Error: Could not decode Gamgee config file: " <> fromString msg
