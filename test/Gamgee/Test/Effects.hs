{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}

module Gamgee.Test.Effects
  ( runOutputPure
  , runListSecretInput
  , runCryptoRandom
  , runByteStoreST

  , runTest
  , runAddToken
  , runDeleteToken
  , runListTokens
  , runGetOTP
  ) where

import           Control.Monad.ST      (ST, runST)
import qualified Crypto.Random         as CR
import qualified Crypto.Random.Types   as CRT
import           Data.STRef            (STRef)
import qualified Data.STRef            as STRef
import qualified Data.Time.Clock.POSIX as Clock
import qualified Gamgee.Effects        as Eff
import qualified Gamgee.Operation      as Operation
import qualified Gamgee.Token          as Token
import           Polysemy              (Lift, Member, Sem)
import qualified Polysemy              as P
import qualified Polysemy.Error        as P
import qualified Polysemy.Output       as P
import qualified Polysemy.State        as P
import           Relude


----------------------------------------------------------------------------------------------------
-- Interpret Output by accumulating it in a list
----------------------------------------------------------------------------------------------------

runOutputPure :: Sem (P.Output o : r) a -> Sem r ([o], a)
runOutputPure = P.runFoldMapOutput one


----------------------------------------------------------------------------------------------------
-- Interpret SecretInput by reading from a list
----------------------------------------------------------------------------------------------------

runListSecretInput :: Member (P.Error Text) r =>  [i] -> Sem (Eff.SecretInput i : r) a -> Sem r a
runListSecretInput is = fmap snd . P.runState is . P.reinterpret
  (\case
      Eff.SecretInput _ -> do
        s <- P.gets uncons
        whenJust s (P.put . snd)
        maybe
          (P.throw "Ran out of input in SecretInput")
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
-- Interpret ByteStore using an STRef
----------------------------------------------------------------------------------------------------

runByteStoreST :: Member (Lift (ST s)) r => STRef s (Maybe LByteString) -> Sem (Eff.ByteStore : r) a -> Sem r a
runByteStoreST ref = P.interpret $ \case
  Eff.ReadByteStore        -> P.sendM $ STRef.readSTRef ref
  Eff.WriteByteStore bytes -> P.sendM $ STRef.writeSTRef ref (Just bytes)


----------------------------------------------------------------------------------------------------
-- All operations supported by gamgee run in ST Monad
----------------------------------------------------------------------------------------------------

type TestConfig s = STRef s (Maybe LByteString)
type ErrorMessage = Text
type OutputMessage = Text

runTest :: Maybe LByteString -> (forall s. TestConfig s -> ST s a) -> IO a
runTest cfg action = return $ runST (STRef.newSTRef cfg >>= action)

runAddToken :: CR.DRG gen
            => gen
            -> TestConfig s
            -> [Text]
            -> Token.TokenSpec
            -> ST s (Either ErrorMessage ())
runAddToken drg cfg input spec = P.runM
                                 $ P.runError
                                 $ runCryptoRandom drg
                                 $ Eff.runCrypto
                                 $ runListSecretInput input
                                 $ runByteStoreST cfg
                                 $ Eff.runGamgeeJSONStore
                                 $ Eff.runStateJSON
                                 $ Operation.addToken spec

runDeleteToken :: TestConfig s -> Token.TokenIdentifier -> ST s (Either ErrorMessage ())
runDeleteToken cfg t = P.runM
                       $ P.runError
                       $ runByteStoreST cfg
                       $ Eff.runGamgeeJSONStore
                       $ Eff.runStateJSON
                       $ Operation.deleteToken t

runListTokens :: TestConfig s -> ST s (Either ErrorMessage ([OutputMessage], ()))
runListTokens cfg = P.runM
                    $ P.runError
                    $ runOutputPure
                    $ runByteStoreST cfg
                    $ Eff.runGamgeeJSONStore
                    $ Eff.runStateJSON Operation.listTokens


runGetOTP :: CR.DRG gen
          => gen
          -> TestConfig s
          -> [Text]
          -> Token.TokenIdentifier
          -> Clock.POSIXTime
          -> ST s (Either ErrorMessage ([OutputMessage], ()))
runGetOTP drg cfg input tok time =
  P.runM
  $ P.runError
  $ runOutputPure
  $ runCryptoRandom drg
  $ Eff.runCrypto
  $ runListSecretInput input
  $ runByteStoreST cfg
  $ Eff.runGamgeeJSONStore
  $ Eff.runStateJSON
  $ Eff.runTOTP
  $ Operation.getOTP tok time
