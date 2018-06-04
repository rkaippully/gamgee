module Gamgee.Util
    ( exitWithMessage
    , getPassword
    ) where

import qualified System.IO as IO
import           Universum


exitWithMessage :: ToString a => a -> IO b
exitWithMessage msg = do
  IO.hPutStrLn stderr (toString msg)
  exitFailure

getPassword :: Text -> IO Text
getPassword prompt = do
  putText prompt
  IO.hFlush stdout
  password <- withoutEcho getLine
  IO.putChar '\n'
  return password

withoutEcho :: IO a -> IO a
withoutEcho action = do
  old <- IO.hGetEcho stdin
  bracket_ (IO.hSetEcho stdin False) (IO.hSetEcho stdin old) action
