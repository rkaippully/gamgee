module Gamgee.Effects.SecretInput
    ( -- * Effect
      SecretInput(..)

      -- * Actions
    , secretInput

      -- * Interpretations
    , runSecretInputIO
    ) where

import           Control.Exception.Safe (bracket_)
import           Polysemy               (Embed, Member, Sem)
import qualified Polysemy               as P
import           Relude
import qualified System.IO              as IO


----------------------------------------------------------------------------------------------------
-- Effect
----------------------------------------------------------------------------------------------------

-- | An effect that provides input to the application. Intended to be
-- used in contexts where the input is a secret such as
-- passwords. Interpretations may chose to "protect" the input
-- appropriately. For example, an IO interpretation may chose not to
-- echo the input to the console.
data SecretInput i m a where
  -- | Retrieve a secret input
  SecretInput :: Text              -- ^ A prompt
              -> SecretInput i m i

P.makeSem ''SecretInput


----------------------------------------------------------------------------------------------------
-- Interpretations
----------------------------------------------------------------------------------------------------

runSecretInputIO :: (Member (Embed IO) r) => Sem (SecretInput Text : r) a -> Sem r a
runSecretInputIO = P.interpret $ \case
  SecretInput prompt -> P.embed $ do
    putText prompt
    IO.hFlush stdout
    i <- withoutEcho getLine
    IO.putChar '\n'
    return i

    where
      withoutEcho :: IO a -> IO a
      withoutEcho f = do
        old <- IO.hGetEcho stdin
        bracket_ (IO.hSetEcho stdin False) (IO.hSetEcho stdin old) f
