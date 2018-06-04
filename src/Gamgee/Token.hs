-- | Data structures to define and manipulate tokens
module Gamgee.Token
    ( TokenType (..)
    , TokenLabel (..)
    , TokenSecret (..)
    , TokenIssuer (..)
    , TokenAlgorithm (..)
    , TokenDigits (..)
    , TokenPeriod (..)
    , TokenSpec (..)
    , TokenIdentifier (..)
    , getIdentifier
    , readTokens
    , writeTokens
    ) where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Text            as Text
import qualified Gamgee.Util          as Util
import qualified System.Directory     as Directory
import           System.FilePath      ((</>))
import           System.IO.Error
import qualified System.Posix.Files   as Files
import           Universum


-- | Type of token TOTP or HOTP (not supported yet)
data TokenType = TOTP

instance Aeson.FromJSON TokenType where
  parseJSON (Aeson.String "totp") = return TOTP
  parseJSON invalid               = fail $ "Invalid token type: " ++ show invalid

instance Aeson.ToJSON TokenType where
  toJSON TOTP = Aeson.String "totp"

-- | Label of the token
newtype TokenLabel = TokenLabel {
  unTokenLabel :: Text
  } deriving newtype (IsString, Aeson.FromJSON, Aeson.ToJSON)

-- | Secret used to generate OTPs
data TokenSecret = TokenSecretPlainText Text
                 | TokenSecretAES256 {
                      tokenSecretAES256IV     :: Text
                      , tokenSecretAES256Data :: Text
                   }
                 deriving (Generic, Aeson.FromJSON, Aeson.ToJSON)

-- | Optional issuer of this token
newtype TokenIssuer = TokenIssuer {
  unTokenIssuer :: Text
  } deriving newtype (IsString, Aeson.FromJSON, Aeson.ToJSON)

data TokenAlgorithm = AlgorithmSHA1
                    | AlgorithmSHA256
                    | AlgorithmSHA512
                    deriving (Generic, Aeson.FromJSON, Aeson.ToJSON)

data TokenDigits = Digits6 | Digits8

instance Aeson.FromJSON TokenDigits where
  parseJSON (Aeson.Number 6) = return Digits6
  parseJSON (Aeson.Number 8) = return Digits8
  parseJSON invalid          = fail $ "Invalid number of digits: " ++ show invalid ++ ". Must be 6 or 8."

instance Aeson.ToJSON TokenDigits where
  toJSON Digits6 = Aeson.Number 6
  toJSON Digits8 = Aeson.Number 8

-- | Refresh interval of the token in seconds
newtype TokenPeriod = TokenPeriod {
  unTokenPeriod :: Word16
  } deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Aeson.FromJSON, Aeson.ToJSON)

data TokenSpec = TokenSpec {
  tokenType        :: TokenType
  , tokenLabel     :: TokenLabel
  , tokenSecret    :: TokenSecret
  , tokenIssuer    :: TokenIssuer
  , tokenAlgorithm :: TokenAlgorithm
  , tokenDigits    :: TokenDigits
  , tokenPeriod    :: TokenPeriod
  } deriving (Generic, Aeson.FromJSON, Aeson.ToJSON)

-- An identifier for a token. This is derived from the label and issuer
newtype TokenIdentifier = TokenIdentifier {
  unTokenIdentifier :: Text
  } deriving newtype (Eq, Hashable, IsString, Semigroup, ToString
                     , Aeson.FromJSON, Aeson.ToJSON
                     , Aeson.FromJSONKey, Aeson.ToJSONKey)

getIdentifier :: TokenSpec -> TokenIdentifier
getIdentifier spec =
  let
    TokenLabel label = tokenLabel spec
    TokenIssuer issuer = tokenIssuer spec
  in
    TokenIdentifier $ if | Text.null issuer                      -> label
                         | Text.isPrefixOf (issuer <> ":") label -> label
                         | otherwise                             -> issuer <> ":" <> label

type Tokens = HashMap TokenIdentifier TokenSpec

data Config = Config {
  configVersion  :: Word32
  , configTokens :: Tokens
  } deriving (Generic, Aeson.FromJSON, Aeson.ToJSON)

currentConfigVersion :: Word32
currentConfigVersion = 1

initialConfig :: Config
initialConfig = Config {
  configVersion = currentConfigVersion
  , configTokens = HashMap.empty
  }

-- | Path under which tokens are stored - typically ~/.config/gamgee/tokens.json
configFilePath :: IO FilePath
configFilePath = do
  dir <- Directory.getXdgDirectory Directory.XdgConfig "gamgee"
  Directory.createDirectoryIfMissing True dir
  return $ dir </> "tokens.json"

-- | Read tokens from a config file
readTokens :: IO Tokens
readTokens = do
  file <- configFilePath
  bs <- LBS.readFile file `catch` handleReadError
  case Aeson.eitherDecode bs of
    Left err -> Util.exitWithMessage $ "Error parsing Gamgee config file: " ++ err
    Right cfg ->
      if configVersion cfg == currentConfigVersion
      then return (configTokens cfg)
      else Util.exitWithMessage $ "Unsupported config version: " ++ show (configVersion cfg)
  where
    handleReadError :: IOError -> IO LByteString
    handleReadError e = if isDoesNotExistError e
                        then return $ Aeson.encode initialConfig
                        else throwM e

-- | Write tokens to a config file
writeTokens :: Tokens -> IO ()
writeTokens specs = do
  file <- configFilePath
  let bs = Aeson.encode Config { configVersion = currentConfigVersion, configTokens = specs }
  withFile file WriteMode $ \h -> LBS.hPut h bs
  Files.setFileMode file $ Files.ownerReadMode `Files.unionFileModes` Files.ownerWriteMode
