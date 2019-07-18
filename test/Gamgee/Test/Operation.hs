module Gamgee.Test.Operation
  ( OutputMessage
  , runTest
  , addToken
  , deleteToken
  , listTokens
  , getOTP
  ) where

import           Control.Monad.ST      (ST, runST)
import qualified Crypto.Random         as CR
import           Data.STRef            (STRef)
import qualified Data.STRef            as STRef
import qualified Data.Time.Clock.POSIX as Clock
import qualified Gamgee.Effects        as Eff
import qualified Gamgee.Operation      as Operation
import qualified Gamgee.Test.Effects   as Eff
import qualified Gamgee.Token          as Token
import qualified Polysemy              as P
import qualified Polysemy.Error        as P
import           Relude


----------------------------------------------------------------------------------------------------
-- All operations supported by gamgee run using STRefs
----------------------------------------------------------------------------------------------------

type TestStore s = STRef s (Maybe LByteString)
type OutputMessage = Text

runTest :: Maybe LByteString -> (forall s. TestStore s -> ST s a) -> a
runTest store action = runST $ STRef.newSTRef store >>= action

addToken :: CR.DRG gen
         => gen
         -> TestStore s
         -> [Text]
         -> Token.TokenSpec
         -> ST s (Either Eff.EffError ())
addToken drg store input spec = P.runM
                                $ P.runError
                                $ Eff.runCryptoRandom drg
                                $ Eff.runCrypto
                                $ Eff.runListSecretInput input
                                $ Eff.runByteStoreST store
                                $ Eff.runGamgeeJSONStore
                                $ Eff.runStateJSON
                                $ Operation.addToken spec

deleteToken :: TestStore s -> Token.TokenIdentifier -> ST s (Either Eff.EffError ())
deleteToken store t = P.runM
                      $ P.runError
                      $ Eff.runByteStoreST store
                      $ Eff.runGamgeeJSONStore
                      $ Eff.runStateJSON
                      $ Operation.deleteToken t

listTokens :: TestStore s -> ST s (Either Eff.EffError [OutputMessage])
listTokens store = fmap fst
                   <$> P.runM
                       (P.runError
                        $ Eff.runOutputPure
                        $ Eff.runByteStoreST store
                        $ Eff.runGamgeeJSONStore
                        $ Eff.runStateJSON Operation.listTokens)

getOTP :: CR.DRG gen
       => gen
       -> TestStore s
       -> [Text]
       -> Token.TokenIdentifier
       -> Clock.POSIXTime
       -> ST s (Either Eff.EffError [OutputMessage])
getOTP drg store input tok time =
  fmap fst
  <$> P.runM
      (P.runError
       $ Eff.runOutputPure
       $ Eff.runCryptoRandom drg
       $ Eff.runCrypto
       $ Eff.runListSecretInput input
       $ Eff.runByteStoreST store
       $ Eff.runGamgeeJSONStore
       $ Eff.runStateJSON
       $ Eff.runTOTP
       $ Operation.getOTP tok time)
