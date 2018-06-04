module Main where

import qualified Gamgee.Command      as Command
import qualified Gamgee.CommandLine  as CommandLine
import qualified Options.Applicative as Options
import           Universum

main :: IO ()
main =
  Options.execParser opts >>= Command.runCommand
  where
    opts = Options.info (Options.helper <*> CommandLine.versionOption <*> CommandLine.getCommand) Options.fullDesc
