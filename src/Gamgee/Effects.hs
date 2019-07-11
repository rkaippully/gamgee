{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module Gamgee.Effects
  ( module Gamgee.Effects.Crypto
  , module Gamgee.Effects.CryptoRandom
  , module Gamgee.Effects.JSONStore
  , module Gamgee.Effects.ByteStore
  , module Gamgee.Effects.SecretInput
  , module Gamgee.Effects.TOTP

  , runStateJSON
  ) where

import           Gamgee.Effects.ByteStore
import           Gamgee.Effects.Crypto
import           Gamgee.Effects.CryptoRandom
import           Gamgee.Effects.JSONStore
import           Gamgee.Effects.SecretInput
import           Gamgee.Effects.TOTP
import           Polysemy                    (Sem)
import qualified Polysemy                    as P
import qualified Polysemy.State              as P

----------------------------------------------------------------------------------------------------
-- Reinterpret State backed by a JSON store
----------------------------------------------------------------------------------------------------

runStateJSON :: Sem (P.State o : r) a -> Sem (JSONStore o : r) a
runStateJSON = P.reinterpret $ \case
  P.Get   -> jsonDecode
  P.Put s -> jsonEncode s
