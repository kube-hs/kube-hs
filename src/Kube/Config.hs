{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
--   Module      :  Kube.Config
--   Description :  Functions for loading and parsing kubeconfig files.
module Kube.Config
  ( module Kube.Config.Types,
    defaultKubeConfigPath,
    loadKubeConfig,
    loadDefaultKubeConfig,
    resolveKubeCredentials,
    getCurrentContext,
    decodeBase64,
  )
where

import Control.Exception (IOException, try)
import Data.Aeson.Key (Key, toString)
import Data.Bifunctor (bimap, first)
import Data.ByteString.Base64 qualified as B64
import Data.Map.Strict qualified as M
import Data.Text (Text, pack, strip, unpack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Yaml qualified as Y
import Kube.Config.Types
import Optics
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Process qualified as P

-- | Get the default path to the kubeconfig file.
defaultKubeConfigPath :: IO (Either Error FilePath)
defaultKubeConfigPath = do
  kcPath <- lookupEnv "KUBECONFIG"
  case kcPath of
    Just path -> pure $ Right path
    Nothing -> do
      homeDir <- lookupEnv "HOME"
      case homeDir of
        Just home -> pure $ Right (home <> "/.kube/config")
        Nothing -> pure $ Left $ KubconfigPathError "Neither KUBECONFIG nor HOME environment variable is set."

-- | Load a kubeconfig file from the given path.
loadKubeConfig :: FilePath -> IO (Either Error Config)
loadKubeConfig = fmap (first fromYamlError) . Y.decodeFileEither
  where
    fromYamlError :: Y.ParseException -> Error
    fromYamlError = ParseError . pack . show

-- | Load the default kubeconfig file.
loadDefaultKubeConfig :: IO (Either Error Config)
loadDefaultKubeConfig = do
  eitherPath <- defaultKubeConfigPath
  case eitherPath of
    Left err -> pure $ Left err
    Right path -> loadKubeConfig path

-- | Get the current context from the config.
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
runExecAuth Exec {command, args, env} = do
  let executable :: FilePath
      executable = unpack command
  let arguments :: [String]
      arguments = maybe [] (map unpack) args
  let processEnv :: Maybe [(String, String)]
      processEnv = case env of
        Nothing -> Nothing
        Just (NamedMap envMap) -> Just $ map mapEnvVar $ M.toList envMap
          where
            mapEnvVar :: (Key, EnvVar) -> (String, String)
            mapEnvVar (key, EnvVar _name value) = (toString key, unpack value)
  let createProc :: P.CreateProcess
      createProc =
        (P.proc executable arguments)
          { P.env = processEnv,
            P.std_in = P.CreatePipe,
            P.std_out = P.CreatePipe,
            P.std_err = P.CreatePipe,
            P.cwd = Nothing
          }
  (exitCode, out, err) <- P.readCreateProcessWithExitCode createProc ""
  pure $ case exitCode of
    ExitSuccess -> Right $ Token $ strip $ pack out
    ExitFailure code -> Left $ ExecError code (pack out) (pack err)

safeReadFile :: FilePath -> IO (Either Error Text)
safeReadFile path = do
  result <- try (readFile path) :: IO (Either IOException String)
  pure $ bimap wrapIOError pack result
  where
    wrapIOError :: IOException -> Error
    wrapIOError e = FileReadError path (show e)

-- | Resolve the credentials from the given auth source.
resolveKubeCredentials :: AuthSource -> IO (Either Error Credentials)
resolveKubeCredentials (TokenFile f) = do
  result <- safeReadFile f
  pure $ fmap (Token . strip) result
resolveKubeCredentials (TokenSource t) = pure $ Right $ Token t
resolveKubeCredentials (BasicAuthSource u p) = pure $ Right $ BasicAuth u p
resolveKubeCredentials (CertsSource c k) = do
  cert <- resolveCertSource c
  key <- resolveKeySource k
  pure $ Certs <$> cert <*> key
resolveKubeCredentials (ExecSource e) = runExecAuth e

resolveCertSource :: CertSource -> IO (Either Error Text)
resolveCertSource (CertFile f) = safeReadFile f
resolveCertSource (CertInline t) = pure $ decodeBase64 t

resolveKeySource :: KeySource -> IO (Either Error Text)
resolveKeySource (KeyFile f) = safeReadFile f
resolveKeySource (KeyInline t) = pure $ decodeBase64 t
