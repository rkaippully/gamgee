{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Gamgee.Effects.CryptoRandom
    ( -- * Effect
      CryptoRandom(..)

      -- * Action
    , randomBytes

      -- * Interpretations
    , runCryptoRandomIO
    ) where

import qualified Crypto.Random.Types as CRT
import qualified Data.ByteArray      as BA
import           Polysemy            (Lift, Member, Sem)
import qualified Polysemy            as P
import           Relude


----------------------------------------------------------------------------------------------------
-- Effects
----------------------------------------------------------------------------------------------------

-- | An effect capable of providing random bytes for use with cryptonite
data CryptoRandom m a where
  -- ^ Generate random bytes
  RandomBytes :: BA.ByteArray b => Int -> CryptoRandom m b

P.makeSem ''CryptoRandom


----------------------------------------------------------------------------------------------------
-- Interpretations
----------------------------------------------------------------------------------------------------

runCryptoRandomIO :: Member (Lift IO) r => Sem (CryptoRandom : r) a -> Sem r a
runCryptoRandomIO = P.interpret $ \case
  RandomBytes count -> P.sendM $ CRT.getRandomBytes count
