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
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Gamgee.Program.Effects
  ( runM_
  , runGamgeeFileStoreIO
  , runGamgeeJSONStore
  , runStateJSON
  , runOutputStdOut
  , runOutputClipboard
  , runErrorStdErr
  ) where

import           Control.Exception.Safe (catch)
import qualified Data.Aeson             as Aeson
import qualified Data.Aeson.Types       as Aeson
import qualified Data.Text.IO           as TIO
import qualified Gamgee.Effects         as Eff
import qualified Gamgee.Token           as Token
import           Polysemy               (Lift, Member, Members, Sem)
import qualified Polysemy               as P
import qualified Polysemy.Error         as P
import qualified Polysemy.Output        as P
import qualified Polysemy.State         as P
import           Relude
import qualified System.Directory       as Dir
import           System.FilePath        ((</>))
import qualified System.Hclip           as Clip
import qualified System.IO.Error        as IO
import qualified System.Posix.Files     as Files


-- | A version of runM that ignores its result
runM_ :: Monad m => Sem '[Lift m] a -> m ()
runM_ = void . P.runM

----------------------------------------------------------------------------------------------------
-- Interpret Output by writing it to stdout or clipboard
----------------------------------------------------------------------------------------------------

runOutputStdOut :: Member (Lift IO) r => Sem (P.Output Text : r) a -> Sem r a
runOutputStdOut = P.interpret $ \case
  P.Output s -> P.sendM $ putTextLn s

runOutputClipboard :: Member (Lift IO) r => Sem (P.Output Text : r) a -> Sem r a
runOutputClipboard = P.interpret $ \case
  P.Output s -> P.sendM $ Clip.setClipboard $ toString s


----------------------------------------------------------------------------------------------------
-- Interpret Error by writing it to stderr
----------------------------------------------------------------------------------------------------

runErrorStdErr :: Member (Lift IO) r => Sem (P.Error Text : r) a -> Sem r (Maybe a)
runErrorStdErr a = P.runError a >>= either printError (return . Just)
  where
    printError :: Member (Lift IO) r => Text -> Sem r (Maybe a)
    printError msg = P.sendM (TIO.hPutStrLn stderr msg) $> Nothing


----------------------------------------------------------------------------------------------------
-- A file store for saving and reading bytes
----------------------------------------------------------------------------------------------------

data FileStore m a where
  ReadFileStore :: FileStore m LByteString
  WriteFileStore :: LByteString -> FileStore m ()

P.makeSem ''FileStore


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
-- Interpret FileStore in IO
----------------------------------------------------------------------------------------------------

runFileStoreIO :: ( Members [Lift IO, P.Error e] r
                  , Exception e1
                  , Exception e2)
               => FilePath
               -> (e1 -> Either e LByteString)     -- ^ Function to handle read errors
               -> (e2 -> Maybe e)                  -- ^ Function to handle write errors
               -> Sem (FileStore : r) a
               -> Sem r a
runFileStoreIO file handleReadError handleWriteError = P.interpret $ \case
  ReadFileStore        -> do
    res <- P.sendM $ (Right <$> readFileLBS file) `catch` (return . handleReadError)
    either P.throw return res
  WriteFileStore bytes -> do
    res <- P.sendM $ (writeFileLBS file bytes $> Nothing) `catch` (return . handleWriteError)
    whenJust res P.throw
    P.sendM $ Files.setFileMode file $ Files.ownerReadMode `Files.unionFileModes` Files.ownerWriteMode

runGamgeeFileStoreIO :: Members [Lift IO, P.Error Text] r
                     => Sem (FileStore : r) a
                     -> Sem r a
runGamgeeFileStoreIO prog = do
  file <- P.sendM configFilePath
  runFileStoreIO file handleReadError handleWriteError prog

  where
    handleReadError :: IO.IOError -> Either Text LByteString
    handleReadError e = if IO.isDoesNotExistError e
                        then Right $ Aeson.encode initialConfig
                        else Left $ show e

    handleWriteError :: IO.IOError -> Maybe Text
    handleWriteError e = Just $ "Internal Error: Error saving configuration file: " <> show e

-- | Path under which tokens are stored - typically ~/.config/gamgee/tokens.json
configFilePath :: IO FilePath
configFilePath = do
  dir <- Dir.getXdgDirectory Dir.XdgConfig "gamgee"
  Dir.createDirectoryIfMissing True dir
  return $ dir </> "tokens.json"


----------------------------------------------------------------------------------------------------
-- Interpret JSONStore backed by a FileStore
----------------------------------------------------------------------------------------------------

runJSONStore :: (Aeson.ToJSON j)
             => (o -> Sem (FileStore : r) j)           -- ^ Function to map object to a JSON serializable value
             -> (Aeson.Value -> Sem (FileStore : r) o) -- ^ Function to map a JSON value to the object
             -> (String -> Sem (FileStore : r) o)      -- ^ Function to handle errors in decoding
             -> Sem (Eff.JSONStore o : r) a
             -> Sem (FileStore : r) a
runJSONStore convertE convertD handleError =
  P.reinterpret $ \case
    Eff.JsonEncode o -> do
      j <- convertE o
      writeFileStore $ Aeson.encode j
    Eff.JsonDecode   -> do
      bytes <- readFileStore
      either handleError convertD $ Aeson.eitherDecode' bytes

runGamgeeJSONStore :: Member (P.Error Text) r => Sem (Eff.JSONStore Token.Tokens : r) a -> Sem (FileStore : r) a
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


----------------------------------------------------------------------------------------------------
-- Interpret State backed by a JSON store
----------------------------------------------------------------------------------------------------

runStateJSON :: Sem (P.State o : r) a -> Sem (Eff.JSONStore o : r) a
runStateJSON = P.reinterpret $ \case
  P.Get   -> Eff.jsonDecode
  P.Put s -> Eff.jsonEncode s
