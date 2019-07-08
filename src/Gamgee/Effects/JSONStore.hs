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

module Gamgee.Effects.JSONStore
  ( -- * Effect
    JSONStore (..)

    -- * Actions
  , jsonEncode
  , jsonDecode

  ) where

import qualified Polysemy as P


----------------------------------------------------------------------------------------------------
-- An abstract JSON store effect that encodes and decodes JSON objects
----------------------------------------------------------------------------------------------------

data JSONStore o m a where
  JsonEncode :: o -> JSONStore o m ()
  JsonDecode :: JSONStore o m o

P.makeSem ''JSONStore
