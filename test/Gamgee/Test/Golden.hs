{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Gamgee.Test.Golden
  ( goldenTests
  ) where

import qualified Crypto.Random         as CR
import qualified Gamgee.Test.Operation as Op
import qualified Gamgee.Token          as Token
import           Relude
import           System.FilePath       ((</>))
import qualified Test.Tasty            as T
import qualified Test.Tasty.Golden     as T


goldenTests :: T.TestTree
goldenTests = T.testGroup "Golden Tests" [ getOTPTest ]

goldenTest :: Show a => T.TestName -> IO a -> T.TestTree
goldenTest name action = T.goldenVsString name (goldenDir </> name ++ ".txt") (encodeUtf8 @String . show <$> action)
  where
    goldenDir = "test" </> "data" </> "golden"


----------------------------------------------------------------------------------------------------
-- getOTP tests
----------------------------------------------------------------------------------------------------

getOTPTest :: T.TestTree
getOTPTest = goldenTest "getOTPTest" $ do
  drg <- CR.getSystemDRG
  return $ Op.runTest Nothing $ \cfg -> do
    let spec = Token.TokenSpec {
                 Token.tokenType        = Token.TOTP
                 , Token.tokenLabel     = "test"
                 , Token.tokenSecret    = Token.TokenSecretPlainText "K5RHSRDNJZRSCTDHKM4VSYLN"
                 , Token.tokenIssuer    = Token.TokenIssuer ""
                 , Token.tokenAlgorithm = Token.AlgorithmSHA1
                 , Token.tokenDigits    = Token.Digits6
                 , Token.tokenPeriod    = Token.TokenPeriod 30 }
    Right () <- Op.addToken drg cfg ["nicepassword"] spec
    mapM (Op.getOTP drg cfg ["nicepassword"] (Token.getIdentifier spec) . fromInteger) [312 + i*30 | i <- [0..4]]
