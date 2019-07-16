{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Gamgee.Test.Golden   as G
import qualified Gamgee.Test.Property as P
import           Relude
import qualified Test.Tasty           as T


main :: IO ()
main = T.defaultMain $ T.testGroup "All Tests" [ G.goldenTests
                                               , P.propertyTests ]
