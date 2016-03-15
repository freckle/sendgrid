{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.API.SendGrid where

import Control.Lens ((^?), (.~), (&))
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.Lens (_JSON)
import Data.ByteString.Lazy as BSL (ByteString)
import Data.Monoid ((<>))
import Data.Text as T (unpack, Text)
import Data.Text.Encoding (encodeUtf8)
import Network.Wreq (postWith, responseBody, defaults, Options, header, Response, checkStatus)

import Network.API.SendGrid.Types

baseSendGridUrl :: Text
baseSendGridUrl = "https://api.sendgrid.com/api/"

sendEmailEndPoint :: Text
sendEmailEndPoint = baseSendGridUrl <> "mail.send.json"

sendEmail :: (MonadReader ApiKey m, MonadIO m, MonadError (Response ByteString) m) => SendEmail -> m Result
sendEmail e = do
  key <- ask
  rsp <- liftIO $ postWith (authOptions key) (T.unpack sendEmailEndPoint) e
  maybe (throwError rsp) pure $ rsp ^? responseBody . _JSON

authOptions :: ApiKey -> Options
authOptions key =
  defaults
    & header "Authorization" .~ ["Bearer " <> encodeUtf8 (unApiKey key)]
    -- We'd rather deal with status code problems as values
    & checkStatus .~ Just (\_ _ _ -> Nothing)
