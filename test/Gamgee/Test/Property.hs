{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Property based tests
module Gamgee.Test.Property
  ( propertyTests
  ) where

import           Control.Monad.ST          (ST)
import qualified Crypto.Random             as CR
import qualified Data.ByteArray.Encoding   as BE
import qualified Data.ByteString           as BS
import qualified Data.Text                 as Text
import qualified Gamgee.Effects            as Eff
import qualified Gamgee.Test.Operation     as Op
import qualified Gamgee.Token              as Token
import           Relude
import qualified Relude.Extra.Map          as Map
import           Test.QuickCheck           (allProperties)
import           Test.QuickCheck.Instances ()
import qualified Test.QuickCheck.Monadic   as T
import qualified Test.Tasty                as T
import qualified Test.Tasty.QuickCheck     as T


deriving newtype instance T.Arbitrary Token.TokenIdentifier
deriving newtype instance T.Arbitrary Token.TokenLabel
deriving newtype instance T.Arbitrary Token.TokenIssuer

instance T.Arbitrary Token.TokenSecret where
  arbitrary = Token.TokenSecretPlainText . decodeUtf8 <$> base32String
    where
      base32String :: T.Gen ByteString
      base32String = BE.convertToBase BE.Base32 <$> T.arbitrary `T.suchThat` (\s -> BS.length s `mod` 10 == 0)

instance T.Arbitrary Token.TokenAlgorithm where
  arbitrary = T.oneof [ pure Token.AlgorithmSHA1
                      , pure Token.AlgorithmSHA256
                      , pure Token.AlgorithmSHA512
                      ]

instance T.Arbitrary Token.TokenDigits where
  arbitrary = T.oneof [ pure Token.Digits6
                      , pure Token.Digits8
                      ]

instance T.Arbitrary Token.TokenPeriod where
  arbitrary = Token.TokenPeriod <$> T.choose (10, 120)

instance T.Arbitrary Token.TokenSpec where
  arbitrary = Token.TokenSpec
              <$> pure Token.TOTP
              <*> T.arbitrary
              <*> T.arbitrary
              <*> T.arbitrary
              <*> T.arbitrary
              <*> T.arbitrary
              <*> T.arbitrary

-- | Ensures that an effect succeeded, fail the ST monad otherwise
ensureSuccess :: ST s (Either Eff.EffError a) -> ST s a
ensureSuccess action = do
  res <- action
  either (fail . show) return res

withSystemDRG :: (T.Testable a) => (CR.SystemDRG -> T.PropertyM IO a) -> T.Property
withSystemDRG f = T.monadicIO $ do
  drg <- liftIO CR.getSystemDRG
  f drg

prop_addThenRemove :: Token.TokenSpec -> T.Property
prop_addThenRemove spec = withSystemDRG $ \drg -> T.assert (addThenRemove drg == Right [])
  where
    addThenRemove :: CR.SystemDRG -> Either Eff.EffError [Op.OutputMessage]
    addThenRemove drg = Op.runTest Nothing $ \store -> do
      ensureSuccess $ Op.addToken drg store [""] spec
      ensureSuccess $ Op.deleteToken store (Token.getIdentifier spec)
      Op.listTokens store

prop_addThenList :: [Token.TokenSpec] -> T.Property
prop_addThenList specList = withSystemDRG $ \drg -> T.assert (addThenList drg == Right (Token.unTokenIdentifier <$> Map.keys specs))
  where
    specs :: HashMap Token.TokenIdentifier Token.TokenSpec
    specs = fromList $ map (\spec -> (Token.getIdentifier spec, spec)) specList

    addThenList :: CR.SystemDRG -> Either Eff.EffError [Op.OutputMessage]
    addThenList drg = Op.runTest Nothing $ \store -> do
      mapM_ (ensureSuccess . Op.addToken drg store [""]) $ Map.elems specs
      Op.listTokens store

prop_addThenGetOTP :: Token.TokenSpec -> T.Property
prop_addThenGetOTP spec = withSystemDRG $ \drg ->
  forM_ (addThenGetOTP drg) $ \case
    Left err  -> fail $ "Expected an OTP but found " <> show err
    Right otp -> case Token.tokenDigits spec of
                   Token.Digits6 -> T.assert $ (Text.length <$> otp) == [6]
                   Token.Digits8 -> T.assert $ (Text.length <$> otp) == [8]

  where
    ident :: Token.TokenIdentifier
    ident = Token.getIdentifier spec

    addThenGetOTP :: CR.SystemDRG -> [Either Eff.EffError [Op.OutputMessage]]
    addThenGetOTP drg = Op.runTest Nothing $ \store -> do
      ensureSuccess $ Op.addToken drg store [""] spec
      mapM (Op.getOTP drg store [""] ident . fromInteger) [100 + i*30 | i <- [0..4]]

prop_doubleAddFails :: Token.TokenSpec -> T.Property
prop_doubleAddFails spec = withSystemDRG $
  \drg -> T.assert (doubleAdd drg == Left (Eff.AlreadyExists $ Token.getIdentifier spec))

  where
    doubleAdd :: CR.SystemDRG -> Either Eff.EffError ()
    doubleAdd drg = Op.runTest Nothing $ \store -> do
      ensureSuccess $ Op.addToken drg store [""] spec
      Op.addToken drg store [""] spec

prop_doubleDeleteFails :: Token.TokenSpec -> T.Property
prop_doubleDeleteFails spec = withSystemDRG $
  \drg -> T.assert (doubleDelete drg == Left (Eff.NoSuchToken ident))

  where
    ident :: Token.TokenIdentifier
    ident = Token.getIdentifier spec

    doubleDelete :: CR.SystemDRG -> Either Eff.EffError ()
    doubleDelete drg = Op.runTest Nothing $ \store -> do
      ensureSuccess $ Op.addToken drg store [""] spec
      ensureSuccess $ Op.deleteToken store ident
      Op.deleteToken store ident

prop_deleteOnEmptyFails :: Token.TokenIdentifier -> T.Property
prop_deleteOnEmptyFails ident = T.monadicIO $
  T.assert (deleteOnEmpty == Left (Eff.NoSuchToken ident))

  where
    deleteOnEmpty :: Either Eff.EffError ()
    deleteOnEmpty = Op.runTest Nothing $ \store -> Op.deleteToken store ident


-- Template Haskell hack to make the following $allProperties work
return []

propertyTests :: T.TestTree
propertyTests = T.testProperties "Property Tests" $allProperties
