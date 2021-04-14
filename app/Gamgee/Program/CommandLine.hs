-- | Command line argument parsing
module Gamgee.Program.CommandLine
    ( Command (..)
    , OutputMode (..)
    , getCommand
    , versionOption
    ) where

import qualified Data.Version        as Version
import qualified Gamgee.Token        as Token
import           Options.Applicative
import           Paths_gamgee        (version)
import           Relude


data OutputMode = OutputStdOut
                | OutputClipboard
                deriving stock (Eq)

data Command = AddToken Token.TokenSpec
             | DeleteToken Token.TokenIdentifier
             | ListTokens
             | GetOTP Token.TokenIdentifier OutputMode
             | ChangePassword Token.TokenIdentifier
             | GetInfo

getCommand :: Parser Command
getCommand = hsubparser (
    command "list" (info listTokens $ progDesc "List the names of all known tokens")
    <> command "add" (info addToken $ progDesc "Add a new token")
    <> command "delete" (info deleteToken $ progDesc "Delete a token")
    <> command "change-password" (info changePassword $ progDesc "Change password of a token")
    <> command "info" (info getInfo $ progDesc "Print information about this Gamgee installation")
  )
  <|> getOTPOperation

listTokens :: Parser Command
listTokens = pure ListTokens

addToken :: Parser Command
addToken = AddToken <$> tokenSpec

tokenSpec :: Parser Token.TokenSpec
tokenSpec = Token.TokenSpec Token.TOTP
            <$> strOption (long "label" <> short 'l'
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

deleteToken :: Parser Command
deleteToken = DeleteToken <$> strOption (long "label" <> short 'l' <> help "Label of the token")

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

getOTPOperation :: Parser Command
getOTPOperation = GetOTP
                  <$> strArgument (help "Get a one-time password" <> metavar "TOKEN-LABEL")
                  <*> flag OutputClipboard OutputStdOut
                      (long "stdout" <> help "Send the OTP to stdout instead of clipboard")

changePassword :: Parser Command
changePassword = ChangePassword
                 <$> strOption (long "label" <> short 'l' <> help "Label of the token")

getInfo :: Parser Command
getInfo = pure GetInfo
