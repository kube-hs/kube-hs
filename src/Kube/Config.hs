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
  -- resolveKubeCredentials,
  getCurrentContext,
) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Text (pack)
import Data.Yaml qualified as Y
import Kube.Config.Types (Config (..), Context (..), Error (..), NamedMap (..), User (..))
import Optics
import System.Environment (getEnv)
import System.FilePath ((</>))

maybeToRight :: e -> Maybe a -> Either e a
maybeToRight e = maybe (Left e) Right

fromYamlError :: Y.ParseException -> Error
fromYamlError = ParseError . pack . show

defaultKubeConfigPath :: IO FilePath
defaultKubeConfigPath = getEnv "KUBECONFIG" <|> (</> ".kube" </> "config") <$> getEnv "HOME"

loadKubeConfig :: FilePath -> IO (Either Error Config)
loadKubeConfig = fmap (first fromYamlError) . Y.decodeFileEither

loadDefaultKubeConfig :: IO (Either Error Config)
loadDefaultKubeConfig = defaultKubeConfigPath >>= loadKubeConfig

getCurrentContext :: Config -> Either Error Context
getCurrentContext cfg =
  maybeToRight (ContextNotFound ctxName) (view (#contexts % to unNamedMap % at ctxName) cfg)
 where
  ctxName = view #currentContext cfg

-- resolveKubeCredentials :: Config -> Context -> Either Error User
-- resolveKubeCredentials cfg ctx = do
--   user <- maybeToRight (UserNotFound $ view #user ctx) (view (#users % to unNamedMap % at (view #user ctx)) cfg)
--   cluster <- maybeToRight (ClusterNotFound $ view #cluster ctx) (view (#clusters % to unNamedMap % at (view #cluster ctx)) cfg)
--   pure $ user & #cluster .~ cluster
