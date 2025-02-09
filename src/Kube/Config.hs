{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Kube.Config (
  module Kube.Config.Types,
  defaultKubeConfigPath,
  loadKubeConfig,
  loadDefaultKubeConfig,
  resolveKubeCredentials,
  getCurrentContext,
) where

import Control.Applicative ((<|>))
import Data.Aeson.Key (toString)
import Data.Bifunctor (first)
import Data.ByteString.Base64 qualified as B64
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, strip, unpack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Yaml qualified as Y
import Kube.Config.Types
import Optics
import System.Environment (getEnv)
import System.Exit (ExitCode (..))
import System.Process qualified as P

defaultKubeConfigPath :: IO FilePath
defaultKubeConfigPath = getEnv "KUBECONFIG" <|> (<> ".kube/config") <$> getEnv "HOME"

loadKubeConfig :: FilePath -> IO (Either Error Config)
loadKubeConfig = fmap (first fromYamlError) . Y.decodeFileEither
 where
  fromYamlError :: Y.ParseException -> Error
  fromYamlError = ParseError . pack . show

loadDefaultKubeConfig :: IO (Either Error Config)
loadDefaultKubeConfig = defaultKubeConfigPath >>= loadKubeConfig

getCurrentContext :: Config -> Either Error Context
getCurrentContext cfg =
  maybeToRight (ContextNotFound ctxName) $ cfg ^. (#contexts % to unNamedMap % at ctxName)
 where
  ctxName = cfg ^. #currentContext
  maybeToRight :: e -> Maybe a -> Either e a
  maybeToRight e = maybe (Left e) Right

decodeBase64 :: Text -> Either Error Text
decodeBase64 t = case B64.decodeBase64Untyped (encodeUtf8 t) of
  Left e -> Left $ Base64DecodeError $ pack $ show e
  Right t' -> case decodeUtf8' t' of
    Left e -> Left $ UnicodeError $ pack $ show e
    Right t'' -> Right $ strip t''

runExecAuth :: Exec -> IO (Either Error Credentials)
runExecAuth Exec{command, args, env} = do
  let cmd = unlines $ unpack <$> command : fromMaybe [] args
  let envVars =
        fmap
          (first toString)
          . over (mapped % _2) (^. #value % to unpack)
          . M.toList
          . unNamedMap
          <$> env
  (exitCode, out, err) <- P.readCreateProcessWithExitCode (process cmd envVars) ""
  pure $ case exitCode of
    ExitSuccess -> Right $ Token $ strip $ pack out
    ExitFailure code -> Left $ ExecError code (pack out) (pack err)

process :: String -> Maybe [(String, String)] -> P.CreateProcess
process cmd envVars = (P.proc "sh" ["-c", cmd]){P.env = envVars}

resolveKubeCredentials :: AuthSource -> IO (Either Error Credentials)
resolveKubeCredentials (TokenFile f) = Right . Token . pack <$> readFile f
resolveKubeCredentials (TokenSource t) = pure $ Right $ Token t
resolveKubeCredentials (BasicAuthSource u p) = pure $ Right $ BasicAuth u p
resolveKubeCredentials (CertsSource c k) = do
  cert <- resolveCertSource c
  key <- resolveKeySource k
  pure $ Certs <$> cert <*> key
resolveKubeCredentials (ExecSource e) = runExecAuth e

resolveCertSource :: CertSource -> IO (Either Error Text)
resolveCertSource (CertFile f) = Right . pack <$> readFile f
resolveCertSource (CertInline t) = pure $ decodeBase64 t

resolveKeySource :: KeySource -> IO (Either Error Text)
resolveKeySource (KeyFile f) = Right . pack <$> readFile f
resolveKeySource (KeyInline t) = pure $ decodeBase64 t
