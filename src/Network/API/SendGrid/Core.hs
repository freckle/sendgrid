{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Contains functionality which is likely to be shared across multiple SendGrid endpoints
module Network.API.SendGrid.Core where

import Control.Lens (makeLenses, makePrisms, (.~), (&))
import Data.Aeson hiding (Result(..))
import Data.ByteString as BS (ByteString)
#if MIN_VERSION_aeson(0,10,0)
import Data.ByteString.Builder as B (toLazyByteString)
#endif
import Data.ByteString.Lazy as BSL (toStrict, ByteString)
import Data.CaseInsensitive (foldedCase)
import Data.HashMap.Strict (unionWith)
import Data.Monoid ((<>))
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Client (Response)
import Network.HTTP.Types (Status)
import Network.HTTP.Types.Header (Header)
import Network.Wreq (Options, defaults, header, checkResponse)
import Text.Email.Validate (EmailAddress)

baseSendGridUrl :: Text
baseSendGridUrl = "https://api.sendgrid.com/api/"

-- * Requests

data ApiKey

-- | Add our bearer token in the header to authenticate.
-- Also, don't treat non-200 statuses as exceptional.
authOptions :: Tagged ApiKey Text -> Options
authOptions (Tagged key) =
  defaults
    & header "Authorization" .~ ["Bearer " <> encodeUtf8 key]
    -- We'd rather deal with status code problems as values
    & checkResponse .~ Just (\_ _ -> pure ())

-- * Responses

-- | The information that's directly represented in SendGrid JSON.
-- Primarily for internal use.
data JResult
  = JSuccess
  | JSendGridErrors [Text]
  deriving (Eq, Show, Generic)
makePrisms ''JResult

-- | Result type for all SendGrid responses.
data Result
  = Success
  | SendGridErrors Status [Text] -- ^ These are the error messages SendGrid returns in JSON for application layer problems
  | ParseError (Response BSL.ByteString)
  -- ^ If the response couldn't be parsed as either a success or an application layer issue, return the whole response.
  deriving (Eq, Show, Generic)
makePrisms ''Result
instance FromJSON JResult where
  parseJSON =
    withObject "expected SendGrid response to be an object" $ \o -> do
      m :: String <- o .: "message"
      if m == "success"
        then pure JSuccess
        else JSendGridErrors <$> o .: "errors"
-- We really only provide this instance so we can use @_JSON@
instance ToJSON JResult where
  toJSON JSuccess =
    object
      [ "message" .= ("success" :: Text) ]
  toJSON (JSendGridErrors es) =
    object
      [ "message" .= ("error" :: Text)
      , "errors" .= es
      ]

-- * Miscellaneous helpers

-- | Based on RFC 2822
sendGridDateFormat :: String
sendGridDateFormat = "%a, %d %b %Y %H:%M:%S %z"

-- | @mergeObjects {key1: {subKey1: 1}} {key1: {subKey2: 2}} == {key1: {subKey1: 1, subKey2: 2}}@
--
-- @mergeObjects {key1: {subKey1: 1}} {key1: {subKey1: 2}} == {key1: {subKey1: 1}}@
mergeObjects :: Value -> Value -> Value
mergeObjects (Object hm1) (Object hm2) = Object $ unionWith mergeObjects hm1 hm2
mergeObjects x _ = x

-- | Email address and corresponding display name to be show in email clients.
data NamedEmail
  = NamedEmail
  { _email :: EmailAddress
  , _name  :: Text
  } deriving (Eq, Read, Show, Generic)
makeLenses ''NamedEmail

-- ** Encoding headers as JSON format ByteString
#if MIN_VERSION_aeson(0,10,0)

encodingToByteString :: Encoding -> BS.ByteString
encodingToByteString = BSL.toStrict . B.toLazyByteString . fromEncoding

encodeHeaders :: [Header] -> Encoding
encodeHeaders = pairs . foldMap (\(key, value) -> decodeUtf8 (foldedCase key) .= decodeUtf8 value)

headersToBS :: [Header] -> BS.ByteString
headersToBS = encodingToByteString . encodeHeaders

#else

headersToBS :: [Header] -> BS.ByteString
headersToBS = BSL.toStrict . encode . object . map (\(key, value) -> decodeUtf8 (foldedCase key) .= decodeUtf8 value)

#endif
