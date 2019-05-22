{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.SendGridV3.Api.Templates where

import           Control.Lens
import           Control.Lens.TH
import           Control.Error.Util (note)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH
import           Data.ByteString.Lazy (ByteString)
import           Data.Char (toLower)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Network.Wreq as W
import           Network.HTTP.Client (HttpException)

import qualified Network.SendGridV3.Api as Api
import           Network.SendGridV3.JSON (unPrefix)

-----------------------------------------------------------------------------

newtype TemplateId = TemplateId { unTemplateId :: Text }
  deriving (Show)

instance FromJSON TemplateId where
  parseJSON (String s) = pure $ TemplateId s
  parseJSON invalid = typeMismatch "TemplateId" invalid

newtype TemplateVersionId = TemplateVersionId { unTemplateVersionId :: Text }
  deriving (Show)

instance FromJSON TemplateVersionId where
  parseJSON (String s) = pure $ TemplateVersionId s
  parseJSON invalid = typeMismatch "TemplateVersionId" invalid

data TemplateVersion = TemplateVersion
  { -- | ID of the transactional template version
    _versionId          :: TemplateVersionId
    -- | ID of the transactional template
  , _versionTemplateId  :: TemplateId
    -- | Indicates if this is the active version of the template
  , _versionActive      :: Bool
    -- | The HTML content of the version
  , _versionHTMLContent :: Maybe Text
    -- | The subject of the version
  , _versionSubject     :: Text
  } deriving (Show)
makeLenses ''TemplateVersion

instance FromJSON TemplateVersion where
  parseJSON (Object v) =
    TemplateVersion
      <$> v .: "id"
      <*> v .: "template_id"
      <*> (toBool <$> v .: "active")
      <*> v .:? "html_content"
      <*> v .: "subject"
    where
      toBool :: Int -> Bool
      toBool i
        | i == 1    = True
        | otherwise = False

data Template = Template
  { -- | The ID of the transactional template
    _templateId       :: TemplateId
    -- | The name for the transactional template
  , _templateName     :: Text
    -- | List of versions of the template
  , _templateVersions :: [TemplateVersion]
  } deriving (Show)
makeLenses ''Template

$(deriveFromJSON defaultOptions
  { fieldLabelModifier = unPrefix "_template"
  } ''Template)

data Templates = Templates
  { _templates :: [Template]
  } deriving (Show)
makeLenses ''Templates

$(deriveFromJSON defaultOptions
  { fieldLabelModifier = unPrefix "_"
  } ''Templates)


-----------------------------------------------------------------------------

-- | Get all dynamic templates
getTemplates :: Api.ApiKey -> IO (Either Api.APIError Templates)
getTemplates key = do
  let opts = Api.defaultOpts key
      url = T.unpack $ Api.sendGridAPIRoot <> "templates?generations=dynamic"
  Api.tryAsJSON $ W.getWith opts url

-- | Get the given template
getTemplate :: Api.ApiKey -> TemplateId -> IO (Either Api.APIError Template)
getTemplate key tid = do
  let opts = Api.defaultOpts key
      url = T.unpack $ Api.sendGridAPIRoot <> "templates/" <> unTemplateId tid
  Api.tryAsJSON $ W.getWith opts url

-- | Get the HTML content of the active version of the given template
getTemplateActiveHTMLContent
  :: Api.ApiKey -> TemplateId -> IO (Either Api.APIError Text)
getTemplateActiveHTMLContent key tid =
  getTemplate key tid >>= \et -> pure $ do
    t <- et
    note Api.APINotFoundError $
      t ^? templateVersions
         . folded
         . filtered (view versionActive)
         . versionHTMLContent
         . _Just
