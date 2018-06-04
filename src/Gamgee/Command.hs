module Gamgee.Command
    ( Command (..)
    , OutputMode (..)
    , runCommand
    ) where

import qualified Data.HashMap.Strict as HashMap
import qualified Gamgee.Crypto       as Crypto
import qualified Gamgee.OTP          as OTP
import qualified Gamgee.Token        as Token
import qualified Gamgee.Util         as Util
import qualified System.Hclip        as Hclip
import           Universum


data Command = GetOTP Token.TokenIdentifier OutputMode
             | AddToken Token.TokenSpec
             | DeleteToken Token.TokenIdentifier
             | ListTokens

data OutputMode = OutputStandardOutput
                | OutputClipboardCopy

runCommand :: Command -> IO ()
runCommand (GetOTP ident outMode) = getOTP ident outMode
runCommand (AddToken spec)        = addToken spec
runCommand (DeleteToken ident)    = deleteToken ident
runCommand ListTokens             = listTokens

addToken :: Token.TokenSpec -> IO ()
addToken spec = do
  let ident = Token.getIdentifier spec
  tokens <- Token.readTokens
  if HashMap.member ident tokens
  then Util.exitWithMessage $ "A token named '" <> ident <> "' already exists."
  else do
    spec' <- Crypto.encryptSecret spec
    Token.writeTokens $ HashMap.insert ident spec' tokens

getOTP :: Token.TokenIdentifier -> OutputMode -> IO ()
getOTP ident mode = do
  tokens <- Token.readTokens
  case HashMap.lookup ident tokens of
    Nothing   -> Util.exitWithMessage $ "No such token: '" <> ident <> "'"
    Just spec -> OTP.computeTOTP spec >>= outputOTP mode

outputOTP :: OutputMode -> OTP.OTPString -> IO ()
outputOTP OutputStandardOutput = putStrLn . toString
outputOTP OutputClipboardCopy  = Hclip.setClipboard . toString

deleteToken :: Token.TokenIdentifier -> IO ()
deleteToken ident = do
  tokens <- Token.readTokens
  case HashMap.lookup ident tokens of
    Nothing -> Util.exitWithMessage $ "No such token: '" <> ident <> "'"
    Just _  -> Token.writeTokens $ HashMap.delete ident tokens

listTokens :: IO ()
listTokens = do
  tokens <- Token.readTokens
  mapM_ (putStrLn . toString . Token.getIdentifier) tokens
