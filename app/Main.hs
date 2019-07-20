module Main where

import qualified Data.Aeson                 as Aeson
import qualified Data.Time.Clock.POSIX      as Clock
import qualified Gamgee.Effects             as Eff
import qualified Gamgee.Operation           as Operation
import qualified Gamgee.Program.CommandLine as Cmd
import qualified Gamgee.Program.Effects     as Eff
import qualified Gamgee.Token               as Token
import qualified Options.Applicative        as Options
import           Polysemy                   (Lift, Member, Sem)
import qualified Polysemy                   as P
import qualified Polysemy.Error             as P
import qualified Polysemy.Input             as P
import qualified Polysemy.Output            as P
import           Relude

main :: IO ()
main = do
  command <- Options.execParser parserInfo
  case command of
    Cmd.AddToken spec -> runAddToken spec
    Cmd.DeleteToken t -> runDeleteToken t
    Cmd.ListTokens    -> runListTokens
    Cmd.GetOTP t mode -> runGetOTP t mode
    Cmd.GetInfo       -> runGetInfo

parserInfo :: Options.ParserInfo Cmd.Command
parserInfo = Options.info (Options.helper
                           <*> Cmd.versionOption
                           <*> Cmd.getCommand) Options.fullDesc

runAddToken :: Token.TokenSpec -> IO ()
runAddToken spec = Eff.runM_
                   $ Eff.runErrorStdErr
                   $ Eff.runCryptoRandomIO
                   $ Eff.runCrypto
                   $ Eff.runSecretInputIO
                   $ Eff.runByteStoreIO
                   $ Eff.runJSONStore
                   $ Eff.runStateJSON
                   $ Operation.addToken spec

runDeleteToken :: Token.TokenIdentifier -> IO ()
runDeleteToken t = Eff.runM_
                   $ Eff.runErrorStdErr
                   $ Eff.runByteStoreIO
                   $ Eff.runJSONStore
                   $ Eff.runStateJSON
                   $ Operation.deleteToken t

runListTokens :: IO ()
runListTokens = Eff.runM_
                $ Eff.runErrorStdErr
                $ Eff.runOutputStdOut
                $ Eff.runByteStoreIO
                $ Eff.runJSONStore
                $ Eff.runStateJSON Operation.listTokens

runGetOTP :: Token.TokenIdentifier -> Cmd.OutputMode -> IO ()
runGetOTP t o = do
  now <- Clock.getPOSIXTime
  Eff.runM_
    $ Eff.runErrorStdErr
    $ (if o == Cmd.OutputStdOut then Eff.runOutputStdOut else Eff.runOutputClipboard)
    $ Eff.runCryptoRandomIO
    $ Eff.runCrypto
    $ Eff.runSecretInputIO
    $ Eff.runByteStoreIO
    $ Eff.runJSONStore
    $ Eff.runStateJSON
    $ Eff.runTOTP
    $ Operation.getOTP t now

getConfig :: Member (Lift IO) r => Sem r (Maybe Token.Config)
getConfig = do
  res <- fmap (rightToMaybe @Eff.EffError)
         $ P.runError
         $ fmap (rightToMaybe @Eff.ByteStoreError)
         $ P.runError
         $ Eff.runByteStoreIO
         $ Eff.configStoreToByteStore Eff.jsonDecode
  return (join res)

runGetInfo :: IO ()
runGetInfo = do
  path <- Eff.configFilePath
  res <- P.runM
         $ P.runFoldMapOutput (decodeUtf8 . Aeson.encode @Aeson.Value)
         $ P.runConstInput path
         $ Operation.getInfo getConfig
  putTextLn $ fst res
