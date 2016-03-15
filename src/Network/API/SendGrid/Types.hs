{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.API.SendGrid.Types where

import Control.Lens (makeLenses, makePrisms, Lens', lens)
import Data.Aeson (pairs, (.=), fromEncoding, Encoding, FromJSON(..), (.:), withObject, ToJSON(..), object)
import Data.ByteString as BS (ByteString)
import Data.ByteString.Builder as B (toLazyByteString)
import Data.ByteString.Lazy as BSL (toStrict, ByteString)
import Data.CaseInsensitive (foldedCase)
import qualified Data.DList as D
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Text as T (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.These (These(..))
import Data.Time (UTCTime(..), defaultTimeLocale, formatTime)
import Network.HTTP.Client (RequestBody(RequestBodyBS), Response)
import Network.HTTP.Client.MultipartFormData (partFileRequestBody)
import Network.HTTP.Types.Header (Header)
import Network.Wreq (partBS, partText, Part)
import Network.Wreq.Types (Postable(..))
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Email.Validate as E (EmailAddress, toByteString)

newtype ApiKey
  = ApiKey Text deriving (Eq, Show, Ord)
makePrisms ''ApiKey

data Result
  = Success
  | SendGridErrors [Text]
  | OtherError (Response BSL.ByteString)
  deriving (Eq, Show)
makePrisms ''Result
instance FromJSON Result where
  parseJSON =
    withObject "expected SendGrid response to be an object" $ \o -> do
      m :: String <- o .: "message"
      if m == "success"
        then pure Success
        else SendGridErrors <$> o .: "errors"
instance ToJSON Result where
  toJSON Success =
    object
      [ "message" .= ("success" :: Text) ]
  toJSON (SendGridErrors es) =
    object
      [ "message" .= ("error" :: Text)
      , "errors" .= es
      ]
  toJSON (OtherError _) =
    object
      [ "message" .= ("unknown error" :: Text) ]

data NamedEmail
  = NamedEmail
  { _neEmail :: EmailAddress
  , _neName  :: Text
  } deriving (Eq, Show)
makeLenses ''NamedEmail

data File
 = File
  { _fileName    :: Text
  , _fileContent :: BS.ByteString
  } deriving (Eq, Show)
makeLenses ''File

data Content
  = Content
  { _contentFile :: File
  , _contentId   :: Text
  } deriving (Eq, Show)
makeLenses ''Content

data SendEmail
 = SendEmail
  { _sendTo       :: Either (NonEmpty NamedEmail) (NonEmpty EmailAddress)
  , _sendSubject  :: Text
  , _sendBody     :: These Html Text
  , _sendFrom     :: EmailAddress
  , _sendCc       :: Maybe (Either (NonEmpty NamedEmail) (NonEmpty EmailAddress))
  , _sendBcc      :: Maybe (Either (NonEmpty NamedEmail) (NonEmpty EmailAddress))
  , _sendFromName :: Maybe Text
  , _sendReplyTo  :: Maybe EmailAddress
  , _sendDate     :: Maybe UTCTime
  , _sendFiles    :: [File] -- ^ Don't duplicate files from `_sendContent` here
  , _sendContent  :: [Content]
  , _sendHeaders  :: [Header]
  }
-- Can't derive (`Eq` or `Show`) because of `Html`
makeLenses ''SendEmail

plainEmails :: Lens' (Maybe (Either (NonEmpty NamedEmail) (NonEmpty EmailAddress))) [EmailAddress]
plainEmails =
  lens getter setter
    where
      getter (Just (Right xs)) = NE.toList xs
      getter _ = []
      setter _ [] = Nothing
      setter _ (x : xs) = Just (Right $ x :| xs)

namedEmails :: Lens' (Maybe (Either (NonEmpty NamedEmail) (NonEmpty EmailAddress))) [NamedEmail]
namedEmails =
  lens getter setter
    where
      getter (Just (Left xs)) = NE.toList xs
      getter _ = []
      setter _ [] = Nothing
      setter _ (x : xs) = Just (Left $ x :| xs)

mkSendEmail :: Either (NonEmpty NamedEmail) (NonEmpty EmailAddress) -> Text -> These Html Text -> EmailAddress -> SendEmail
mkSendEmail to subject body from'
  = SendEmail
  { _sendTo       = to
  , _sendSubject  = subject
  , _sendBody     = body
  , _sendFrom     = from'
  , _sendCc       = Nothing
  , _sendBcc      = Nothing
  , _sendFromName = Nothing
  , _sendReplyTo  = Nothing
  , _sendDate     = Nothing
  , _sendFiles    = []
  , _sendContent  = []
  , _sendHeaders  = []
  }

emailsToParts :: Text -> Text -> Either (NonEmpty NamedEmail) (NonEmpty EmailAddress) -> [Part]
emailsToParts emailKey nameKey (Left namedEmails') =
  flip foldMap namedEmails'
     (\NamedEmail{..} ->
       [ partBS emailKey $ E.toByteString _neEmail
       , partText nameKey _neName
       ])
emailsToParts emailKey _ (Right emails) =
  partBS emailKey . E.toByteString <$> NE.toList emails

instance Postable SendEmail where
  postPayload = postPayload . sendEmailToParts

sendEmailToParts :: SendEmail -> [Part]
sendEmailToParts SendEmail{..} =
  D.toList $ foldMap D.fromList
  [ toParts
  , [subjectPart]
  , bodyParts
  , [fromPart]
  , ccParts
  , bccParts
  , fromNamePart
  , replyToPart
  , datePart
  , headerPart
  , fileParts
  , contentParts
  ]
    where
      fileToPart File{..} = partFileRequestBody ("files[" <> _fileName  <> "]") (T.unpack _fileName) (RequestBodyBS _fileContent)
      contentParts = foldMap contentToParts _sendContent
        where
          contentToParts (Content file@File{..} cId) =
            [fileToPart file, partText ("content[" <> _fileName <> "]") cId]
      fileParts = fileToPart <$> _sendFiles
      toParts = emailsToParts "to[]" "toname[]" _sendTo
      fromPart = partBS "from" $ E.toByteString _sendFrom
      ccParts = maybe [] (emailsToParts "cc[]" "ccname[]") _sendCc
      bccParts = maybe [] (emailsToParts "bcc[]" "bccname[]") _sendBcc
      fromNamePart = maybeToList $ partText "fromname" <$> _sendFromName
      replyToPart = maybeToList $ partBS "replyto" . E.toByteString <$> _sendReplyTo
      datePart = maybeToList $ partText "date" . T.pack . formatTime defaultTimeLocale sendGridDateFormat <$> _sendDate
      subjectPart = partText "subject" _sendSubject
      headerPart =
        case _sendHeaders of
          [] -> []
          headers -> [partBS "headers" . encodingToByteString $ encodeHeaders headers]
      bodyParts =
        case _sendBody of
          This html -> [partBS "html" . BSL.toStrict $ renderHtml html]
          That text -> [partText "text" text]
          These html text -> [partBS "html" . BSL.toStrict $ renderHtml html, partText "text" text]

encodingToByteString :: Encoding -> BS.ByteString
encodingToByteString = BSL.toStrict . B.toLazyByteString . fromEncoding

encodeHeaders :: [Header] -> Encoding
encodeHeaders = pairs . foldMap (\(key, value) -> decodeUtf8 (foldedCase key) .= decodeUtf8 value)

sendGridDateFormat :: String
sendGridDateFormat = "%a, %d %b %Y %H:%M:%S %z"
