-- | Command line argument parsing
module Gamgee.CommandLine
    ( getCommand
    , versionOption
    ) where

import qualified Data.Version        as Version
import qualified Gamgee.Command      as Command
import qualified Gamgee.Token        as Token
import           Options.Applicative
import           Paths_gamgee        (version)
import           Universum


getCommand :: Parser Command.Command
getCommand = hsubparser (
    command "list" (info listTokens $ progDesc "List the names of all known tokens")
    <> command "add" (info addToken $ progDesc "Add a new token")
    <> command "delete" (info deleteToken $ progDesc "Delete a token")
  )
  <|> getOTP

listTokens :: Parser Command.Command
listTokens = pure Command.ListTokens

addToken :: Parser Command.Command
addToken = Command.AddToken <$> tokenSpecFromSecret

deleteToken :: Parser Command.Command
deleteToken = Command.DeleteToken <$> strOption (long "label" <> short 'l' <> help "Label of the token")

tokenSpecFromSecret :: Parser Token.TokenSpec
tokenSpecFromSecret = Token.TokenSpec
                  <$> pure Token.TOTP
                  <*> strOption (long "label" <> short 'l'
                        <> help "Label of the token")
                  <*> (Token.TokenSecretPlainText <$> strOption (long "secret" <> short 's'
                        <> help "Secret of the token"))
                  <*> strOption (long "issuer" <> short 'i' <> value (Token.TokenIssuer "")
                        <> help "Issuer of the token")
                  <*> option (eitherReader algoReader) (long "algorithm" <> short 'a' <> value Token.AlgorithmSHA1
                        <> help "HMAC algorithm - SHA1 (default), SHA256, or SHA512")
                  <*> option (eitherReader digitsReader) (long "digits" <> short 'd' <> value Token.Digits6
                        <> help "Number of digits in the OTP - 6 (default) or 8")
                  <*> (Token.TokenPeriod <$> option auto (long "period" <> short 'p' <> value 30
                                              <> help "OTP validity in seconds. Default - 30."))

algoReader :: String -> Either String Token.TokenAlgorithm
algoReader "SHA1"   = Right Token.AlgorithmSHA1
algoReader "SHA256" = Right Token.AlgorithmSHA256
algoReader "SHA512" = Right Token.AlgorithmSHA512
algoReader s        = Left $ "Invalid algorithm: " ++ s

digitsReader :: String -> Either String Token.TokenDigits
digitsReader "6" = Right Token.Digits6
digitsReader "8" = Right Token.Digits8
digitsReader s   = Left $ "Invalid number of digits: " ++ s ++ ". Must be 6 or 8."

versionOption :: Parser (a -> a)
versionOption = infoOption (Version.showVersion version) (long "version" <> help "Show version")

getOTP :: Parser Command.Command
getOTP = Command.GetOTP
     <$> strArgument (help "Get a one-time password" <> metavar "TOKEN-LABEL")
     <*> flag Command.OutputClipboardCopy Command.OutputStandardOutput
          (long "stdout" <> help "Send the OTP to stdout instead of clipboard")
