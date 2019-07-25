module Gamgee.Test.Effects
  ( runOutputPure
  , runListSecretInput
  , runCryptoRandom
  , runByteStoreST
  ) where

import           Control.Monad.ST    (ST)
import qualified Crypto.Random       as CR
import qualified Crypto.Random.Types as CRT
import           Data.STRef          (STRef)
import qualified Data.STRef          as STRef
import qualified Gamgee.Effects      as Eff
import           Polysemy            (Lift, Member, Sem)
import qualified Polysemy            as P
import qualified Polysemy.Output     as P
import qualified Polysemy.State      as P
import           Relude


----------------------------------------------------------------------------------------------------
-- Interpret Output by accumulating it in a list
----------------------------------------------------------------------------------------------------

runOutputPure :: Sem (P.Output o : r) a -> Sem r ([o], a)
runOutputPure = P.runFoldMapOutput one


----------------------------------------------------------------------------------------------------
-- Interpret SecretInput by reading from a list
----------------------------------------------------------------------------------------------------

runListSecretInput :: [i] -> Sem (Eff.SecretInput i : r) a -> Sem r a
runListSecretInput is = fmap snd . P.runState is . P.reinterpret
  (\case
      Eff.SecretInput _ -> do
        s <- P.gets uncons
        whenJust s (P.put . snd)
        maybe
          (error "Ran out of input in SecretInput")
          return
          (fst <$> s)
  )


----------------------------------------------------------------------------------------------------
-- Interpret CryptoRandom with a DRG
----------------------------------------------------------------------------------------------------

runCryptoRandom :: CR.DRG gen => gen -> Sem (Eff.CryptoRandom : r) a -> Sem r a
runCryptoRandom gen = P.interpret $ \case
  Eff.RandomBytes count -> return (fst $ CR.withDRG gen (CRT.getRandomBytes count))


----------------------------------------------------------------------------------------------------
-- Interpret ByteStore using the ST monad
----------------------------------------------------------------------------------------------------

runByteStoreST :: Member (Lift (ST s)) r => STRef s (Maybe LByteString) -> Sem (Eff.ByteStore : r) a -> Sem r a
runByteStoreST ref = P.interpret $ \case
  Eff.ReadByteStore        -> P.sendM $ STRef.readSTRef ref
  Eff.WriteByteStore bytes -> P.sendM $ STRef.writeSTRef ref (Just bytes)
