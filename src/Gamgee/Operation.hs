{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Gamgee.Operation
  ( addToken
  , deleteToken
  , listTokens
  , getOTP
  ) where


import qualified Data.Time.Clock.POSIX as Clock
import qualified Gamgee.Effects        as Eff
import qualified Gamgee.Token          as Token
import           Polysemy              (Members, Sem)
import qualified Polysemy.Error        as P
import qualified Polysemy.Output       as P
import qualified Polysemy.State        as P
import           Relude
import qualified Relude.Extra.Map      as Map


addToken :: Members [ P.State Token.Tokens
                    , Eff.Crypto
                    , Eff.SecretInput Text
                    , P.Error Eff.EffError ] r
         => Token.TokenSpec
         -> Sem r ()
addToken spec = do
  let ident = Token.getIdentifier spec
  tokens <- P.get
  if ident `Map.member` tokens
  then P.throw $ Eff.AlreadyExists ident
  else do
    spec' <- Eff.encryptSecret spec
    P.put $ Map.insert ident spec' tokens

deleteToken :: Members [ P.State Token.Tokens
                       , P.Error Eff.EffError ] r
            => Token.TokenIdentifier
            -> Sem r ()
deleteToken ident = do
  tokens <- P.get
  case Map.lookup ident tokens of
    Nothing -> P.throw $ Eff.NoSuchToken ident
    Just _  -> P.put $ Map.delete ident tokens

listTokens :: Members [ P.State Token.Tokens
                      , P.Output Text ] r
           => Sem r ()
listTokens = do
  tokens <- P.get
  mapM_ (P.output . Token.unTokenIdentifier . Token.getIdentifier) (tokens :: Token.Tokens)

getOTP :: Members [ P.State Token.Tokens
                  , P.Error Eff.EffError
                  , P.Output Text
                  , Eff.TOTP ] r
       => Token.TokenIdentifier
       -> Clock.POSIXTime
       -> Sem r ()
getOTP ident time = do
  tokens <- P.get
  case Map.lookup ident tokens of
    Nothing   -> P.throw $ Eff.NoSuchToken ident
    Just spec -> Eff.getTOTP spec time >>= P.output
