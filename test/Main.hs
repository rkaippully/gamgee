{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Gamgee.Test.Golden as G
import           Relude
import qualified Test.Tasty         as T


main :: IO ()
main = T.defaultMain $ T.testGroup "All Tests" [ unitTests ]

unitTests :: T.TestTree
unitTests = T.testGroup "Unit Tests" [ G.goldenTests ]
