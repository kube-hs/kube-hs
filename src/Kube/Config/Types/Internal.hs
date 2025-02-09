{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Kube.Config.Types.Internal (
  Version,
  Kind,
  Config (..),
  Context (..),
  Preferences (..),
  Extension (..),
  EnvVar (..),
  ExecInteractiveMode,
  Exec (..),
  User (..),
  Error (..),
  NamedMap (..),
  AuthSource (..),
  CertSource (..),
  KeySource (..),
  Credentials (..),
) where

import Data.Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector as Vector
import GHC.Generics (Generic)
import Network.URI (URI)
import Optics.TH (makeFieldLabels)

newtype NamedMap a = NamedMap {unNamedMap :: Map Key a}
  deriving (Show, Eq)

instance (HasValueField a, FromJSON a) => FromJSON (NamedMap a) where
  parseJSON (Array arr) =
    NamedMap . Map.fromList <$> traverse parseNamed (toList arr)
   where
    parseNamed = withObject "Named" $ \v -> do
      name <- v .: "name"
      value <- v .: (valueField @a)
      pure (name, value)
  parseJSON _ = fail "Expected array of named objects"

instance (HasValueField a, ToJSON a) => ToJSON (NamedMap a) where
  toJSON (NamedMap m) =
    Array
      $ Vector.map
        ( \(name, val) ->
            object ["name" .= name, valueField @a .= val]
        )
      $ Vector.fromList
      $ Map.toList m

class HasValueField a where
  valueField :: Key

instance HasValueField Context where
  valueField = "context"

instance HasValueField Cluster where
  valueField = "cluster"

instance HasValueField User where
  valueField = "user"

instance HasValueField EnvVar where
  valueField = "value"

instance FromJSON Preferences
instance ToJSON Preferences

instance FromJSON Extension
instance ToJSON Extension

instance FromJSON Context
instance ToJSON Context

instance FromJSON EnvVar
instance ToJSON EnvVar

instance FromJSON Cluster
instance ToJSON Cluster

instance FromJSON AuthProvider
instance ToJSON AuthProvider

instance FromJSON Exec
instance ToJSON Exec

instance FromJSON User
instance ToJSON User

instance FromJSON Config
instance ToJSON Config

instance FromJSON ExecInteractiveMode
instance ToJSON ExecInteractiveMode

type Version = Text
type Kind = Text

data Preferences = Preferences
  { colors :: Maybe Bool
  , extensions :: Maybe [Extension]
  }
  deriving (Show, Eq, Generic)

data Extension = Extension
  { name :: Text
  }
  deriving (Show, Eq, Generic)

data Context = Context
  { cluster :: Text
  , user :: Text
  , namespace :: Maybe Text
  , extensions :: Maybe [Extension]
  }
  deriving (Show, Eq, Generic)

data EnvVar = EnvVar
  { name :: Text
  , value :: Text
  }
  deriving (Show, Eq, Generic)

data ExecInteractiveMode = Never | IfAvailable | Always deriving (Show, Eq, Generic)

data Exec = Exec
  { command :: Text
  , args :: Maybe [Text]
  , env :: Maybe (NamedMap EnvVar)
  , apiVersion :: Maybe Text
  , installHint :: Maybe Text
  , provideClusterInfo :: Maybe Bool
  , interactiveMode :: Maybe ExecInteractiveMode
  }
  deriving (Show, Eq, Generic)

data Cluster = Cluster
  { server :: URI
  , tlsServerName :: Maybe Text
  , insecureSkipTlsVerify :: Maybe Bool
  , certificateAuthority :: Maybe FilePath
  , certificateAuthorityData :: Maybe Text
  , proxyUrl :: Maybe Text
  , disableCompression :: Maybe Bool
  , extensions :: Maybe [Extension]
  }
  deriving (Show, Eq, Generic)

data AuthProvider
  = AuthProvider
  { name :: Text
  , config :: Map Text Text
  }
  deriving (Show, Eq, Generic)

data User
  = User
  { clinetCertificate :: Maybe Text
  , clientCertificateData :: Maybe Text
  , clientKey :: Maybe Text
  , clientKeyData :: Maybe Text
  , token :: Maybe Text
  , tokenFile :: Maybe Text
  , as :: Maybe Text
  , asUid :: Maybe Text
  , asGroups :: Maybe Text
  , asUserExtra :: Maybe Text
  , username :: Maybe Text
  , password :: Maybe Text
  , authProvider :: Maybe AuthProvider
  , exec :: Maybe Exec
  , extensions :: Maybe [Extension]
  }
  deriving (Show, Eq, Generic)

data Config
  = Config
  { apiVersion :: Maybe Version
  , kind :: Maybe Kind
  , preferences :: Maybe Preferences
  , clusters :: NamedMap Cluster
  , users :: NamedMap User
  , contexts :: NamedMap Context
  , currentContext :: Key
  , extensions :: Maybe [Extension]
  }
  deriving (Show, Eq, Generic)

data CertSource = CertFile FilePath | CertInline Text deriving (Show, Eq)
data KeySource = KeyFile FilePath | KeyInline Text deriving (Show, Eq)

data AuthSource
  = TokenFile FilePath
  | TokenSource Text
  | CertsSource CertSource KeySource
  | ExecSource Exec
  | BasicAuthSource Text Text
  deriving (Show, Eq)

data Credentials
  = Token Text
  | Certs Text Text
  | BasicAuth Text Text
  deriving (Show, Eq)

data Error
  = ParseError Text
  | ConfigNotFound FilePath
  | ContextNotFound Key
  | ClusterNotFound Key
  | UserNotFound Key
  | Base64DecodeError Text
  | FileReadError Text
  | UnicodeError Text
  | ExecError Int Text Text
  deriving (Show, Eq)

makeFieldLabels ''Config
