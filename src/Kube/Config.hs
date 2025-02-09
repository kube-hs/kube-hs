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
import System.FilePath ((</>))
import System.Process (CmdSpec (ShellCommand), CreateProcess (..), StdStream (Inherit), readCreateProcessWithExitCode)

defaultKubeConfigPath :: IO FilePath
defaultKubeConfigPath = getEnv "KUBECONFIG" <|> (</> ".kube" </> "config") <$> getEnv "HOME"

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
  maybeToRight e = maybe (Left e) Right

decodeBase64 :: Text -> Either Error Text
decodeBase64 t = case B64.decodeBase64Untyped (encodeUtf8 t) of
  Left e -> Left $ Base64DecodeError $ pack $ show e
  Right t' -> case decodeUtf8' t' of
    Left e -> Left $ UnicodeError $ pack $ show e
    Right t'' -> Right $ strip t''

runExecAuth :: Exec -> IO (Either Error Text)
runExecAuth Exec{command, args, env} = do
  let cmd = unlines $ unpack <$> command : fromMaybe [] args
  let envVars =
        fmap
          (first toString)
          . over (mapped % _2) (^. #value % to unpack)
          . M.toList
          . unNamedMap
          <$> env
  (exitCode, out, err) <- readCreateProcessWithExitCode (process cmd envVars) ""
  pure $ case exitCode of
    ExitSuccess -> Right $ strip $ pack out
    ExitFailure code -> Left $ ExecError code (pack out) (pack err)

process :: String -> Maybe [(String, String)] -> CreateProcess
process cmd envVars =
  CreateProcess
    { cmdspec = ShellCommand cmd
    , env = envVars
    , cwd = Nothing
    , std_in = Inherit
    , std_out = Inherit
    , std_err = Inherit
    , close_fds = False
    , create_group = False
    , delegate_ctlc = False
    , detach_console = False
    , create_new_console = False
    , new_session = False
    , child_group = Nothing
    , child_user = Nothing
    , use_process_jobs = False
    }

resolveKubeCredentials :: AuthSource -> IO Credentials
resolveKubeCredentials (TokenFile f) = pack <$> readFile f <&> Token
resolveKubeCredentials (TokenSource t) = pure $ Token t
resolveKubeCredentials (BasicAuthSource u p) = pure $ BasicAuth u p
resolveKubeCredentials (CertsSource c k) = Certs <$> resolveCertSource c <*> resolveKeySource k
 where
  resolveCertSource (CertFile f) = pack <$> readFile f
  resolveCertSource (CertInline t) = either (fail . show) pure $ decodeBase64 t
  resolveKeySource (KeyFile f) = pack <$> readFile f
  resolveKeySource (KeyInline t) = either (fail . show) pure $ decodeBase64 t
resolveKubeCredentials (ExecSource e) = do
  res <- runExecAuth e
  either (fail . show) (pure . Token) res
