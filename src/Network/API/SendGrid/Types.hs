{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.API.SendGrid.Types where

import Data.Aeson (pairs, (.=), fromEncoding, Encoding, FromJSON(..), (.:), withObject, ToJSON(..), object)
import Data.ByteString as BS (ByteString)
import Data.ByteString.Builder as B (toLazyByteString)
import Data.ByteString.Lazy as BSL (toStrict, ByteString)
import Data.CaseInsensitive (foldedCase)
import Data.DList as D (toList, fromList)
import Data.List.NonEmpty as NE (NonEmpty, toList)
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
  = ApiKey { unApiKey :: Text } deriving (Eq, Show, Ord)

data Result
  = Success
  | SendGridErrors [Text]
  | OtherError (Response BSL.ByteString)
  deriving (Eq, Show)
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
  { neEmail :: EmailAddress
  , neName  :: Text
  } deriving (Eq, Show)

data File
 = File
  { fileName    :: Text
  , fileContent :: BS.ByteString
  } deriving (Eq, Show)

data Content
  = Content
  { contentFile :: File
  , contentId   :: Text
  } deriving (Eq, Show)

data SendEmail
 = SendEmail
  { sendTo       :: Either (NonEmpty NamedEmail) (NonEmpty EmailAddress)
  , sendSubject  :: Text
  , sendBody     :: These Html Text
  , sendFrom     :: EmailAddress
  , sendCc       :: Maybe (Either (NonEmpty NamedEmail) (NonEmpty EmailAddress))
  , sendBcc      :: Maybe (Either (NonEmpty NamedEmail) (NonEmpty EmailAddress))
  , sendFromName :: Maybe Text
  , sendReplyTo  :: Maybe EmailAddress
  , sendDate     :: Maybe UTCTime
  -- Don't duplicate files from `sendContent` here
  , sendFiles    :: Maybe (NonEmpty File)
  , sendContent  :: Maybe (NonEmpty Content)
  , sendHeaders  :: Maybe (NonEmpty Header)
  }
-- Can't derive (`Eq` or `Show`) because of `Html`

mkSendEmail :: Either (NonEmpty NamedEmail) (NonEmpty EmailAddress) -> Text -> These Html Text -> EmailAddress -> SendEmail
mkSendEmail to subject body from
                 = SendEmail
  { sendTo       = to
  , sendSubject  = subject
  , sendBody     = body
  , sendFrom     = from
  , sendCc       = Nothing
  , sendBcc      = Nothing
  , sendFromName = Nothing
  , sendReplyTo  = Nothing
  , sendDate     = Nothing
  , sendFiles    = Nothing
  , sendContent  = Nothing
  , sendHeaders  = Nothing
  }

emailsToParts :: Text -> Text -> Either (NonEmpty NamedEmail) (NonEmpty EmailAddress) -> [Part]
emailsToParts emailKey nameKey (Left namedEmails) =
  flip foldMap namedEmails
     (\NamedEmail{..} ->
       [ partBS emailKey $ E.toByteString neEmail
       , partText nameKey neName
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
      fileToPart File{..} = partFileRequestBody ("files[" <> fileName  <> "]") (T.unpack fileName) (RequestBodyBS fileContent)
      contentParts = maybe [] (foldMap contentToParts . NE.toList) sendContent
        where
          contentToParts (Content file@File{..} contentId) =
            [fileToPart file, partText ("content[" <> fileName <> "]") contentId]
      fileParts = maybe [] (map fileToPart . NE.toList) sendFiles
      toParts = emailsToParts "to[]" "toname[]" sendTo
      fromPart = partBS "from" $ E.toByteString sendFrom
      ccParts = maybe [] (emailsToParts "cc[]" "ccname[]") sendCc
      bccParts = maybe [] (emailsToParts "bcc[]" "bccname[]") sendBcc
      fromNamePart = maybeToList $ partText "fromname" <$> sendFromName
      replyToPart = maybeToList $ partBS "replyto" . E.toByteString <$> sendReplyTo
      datePart = maybeToList $ partText "date" . T.pack . formatTime defaultTimeLocale sendGridDateFormat <$> sendDate
      subjectPart = partText "subject" sendSubject
      headerPart = maybeToList $ partBS "headers" . encodingToByteString . encodeHeaders . NE.toList <$> sendHeaders
      bodyParts =
        case sendBody of
          This html -> [partBS "html" . BSL.toStrict $ renderHtml html]
          That text -> [partText "text" text]
          These html text -> [partBS "html" . BSL.toStrict $ renderHtml html, partText "text" text]

encodingToByteString :: Encoding -> BS.ByteString
encodingToByteString = BSL.toStrict . B.toLazyByteString . fromEncoding

encodeHeaders :: [Header] -> Encoding
encodeHeaders = pairs . foldMap (\(key, value) -> decodeUtf8 (foldedCase key) .= decodeUtf8 value)

sendGridDateFormat :: String
sendGridDateFormat = "%a, %d %b %Y %H:%M:%S %z"