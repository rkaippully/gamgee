{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Property based tests
module Gamgee.Test.Property where

import           Relude
import           Test.QuickCheck       (allProperties)
import qualified Test.Tasty            as T
import qualified Test.Tasty.QuickCheck as T

prop_commutativeAdd :: Int -> Int -> Bool
prop_commutativeAdd x y = x + y == y + x

-- Template Haskell hack to make the following $allProperties work
return []

propertyTests :: T.TestTree
propertyTests = T.testProperties "Foo" $allProperties
