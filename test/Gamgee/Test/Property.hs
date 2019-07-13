{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Property based tests
module Gamgee.Test.Property where

import           Relude
import           Test.QuickCheck       (allProperties)
import qualified Test.Tasty            as T
import qualified Test.Tasty.QuickCheck as T

prop_commutativeMul :: Int -> Int -> Int -> Bool
prop_commutativeMul x y z = x * (y * z) == (x * y) * z

-- Template Haskell hack to make the following $allProperties work
return []

propertyTests :: T.TestTree
propertyTests = T.testProperties "Property Tests" $allProperties
