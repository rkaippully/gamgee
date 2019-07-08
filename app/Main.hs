{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import qualified Data.Time.Clock.POSIX      as Clock
import qualified Gamgee.Effects             as Eff
import qualified Gamgee.Operation           as Operation
import qualified Gamgee.Program.CommandLine as Cmd
import qualified Gamgee.Program.Effects     as Eff
import qualified Gamgee.Token               as Token
import qualified Options.Applicative        as Options
import           Relude

main :: IO ()
main = do
  command <- Options.execParser parserInfo
  case command of
    Cmd.AddToken spec -> runAddToken spec
    Cmd.DeleteToken t -> runDeleteToken t
    Cmd.ListTokens    -> runListTokens
    Cmd.GetOTP t mode -> runGetOTP t mode

parserInfo :: Options.ParserInfo Cmd.Command
parserInfo = Options.info (Options.helper
                           <*> Cmd.versionOption
                           <*> Cmd.getCommand) Options.fullDesc

runAddToken :: Token.TokenSpec -> IO ()
runAddToken spec = Eff.runM_
                   $ Eff.runErrorStdErr
                   $ Eff.runCryptoRandomIO
                   $ Eff.runCryptoIO
                   $ Eff.runSecretInputIO
                   $ Eff.runGamgeeFileStoreIO
                   $ Eff.runGamgeeJSONStore
                   $ Eff.runStateJSON
                   $ Operation.addToken spec

runDeleteToken :: Token.TokenIdentifier -> IO ()
runDeleteToken t = Eff.runM_
                   $ Eff.runErrorStdErr
                   $ Eff.runGamgeeFileStoreIO
                   $ Eff.runGamgeeJSONStore
                   $ Eff.runStateJSON
                   $ Operation.deleteToken t

runListTokens :: IO ()
runListTokens = Eff.runM_
                $ Eff.runErrorStdErr
                $ Eff.runOutputStdOut
                $ Eff.runGamgeeFileStoreIO
                $ Eff.runGamgeeJSONStore
                $ Eff.runStateJSON Operation.listTokens


runGetOTP :: Token.TokenIdentifier -> Cmd.OutputMode -> IO ()
runGetOTP t o = do
  now <- Clock.getPOSIXTime
  Eff.runM_
    $ Eff.runErrorStdErr
    $ (if o == Cmd.OutputStdOut then Eff.runOutputStdOut else Eff.runOutputClipboard)
    $ Eff.runCryptoRandomIO
    $ Eff.runCryptoIO
    $ Eff.runSecretInputIO
    $ Eff.runGamgeeFileStoreIO
    $ Eff.runGamgeeJSONStore
    $ Eff.runStateJSON
    $ Eff.runTOTP
    $ Operation.getOTP t now
