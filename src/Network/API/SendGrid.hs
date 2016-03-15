{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.API.SendGrid
  ( module Network.API.SendGrid
  , module Network.API.SendGrid.Types
  ) where

import Control.Lens ((^?), (.~), (&), (^.))
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.Lens (_JSON)
import Data.Monoid ((<>))
import Data.Text as T (unpack, Text)
import Data.Text.Encoding (encodeUtf8)
import Network.Wreq (responseBody, defaults, Options, header, checkStatus)
import Network.Wreq.Session (postWith, Session)

import Network.API.SendGrid.Types

baseSendGridUrl :: Text
baseSendGridUrl = "https://api.sendgrid.com/api/"

sendEmailEndPoint :: Text
sendEmailEndPoint = baseSendGridUrl <> "mail.send.json"

-- | This type signature allows you to use `sendEmail` as either
-- e.g `SendEmail -> ReaderT (ApiKey, Session) IO (IO Result)` or
-- `SendEmail -> (ApiKey, Session) -> IO Result`
-- In the first form, it also has the useful effect of discouraging you from sending emails
-- in the midst of another action
sendEmail :: (MonadReader (ApiKey, Session) n, MonadIO m) => SendEmail -> n (m Result)
sendEmail e = do
  (key, session) <- ask
  pure $ do
    rsp <- liftIO $ postWith (authOptions key) session (T.unpack sendEmailEndPoint) e
    let mResult = rsp ^? responseBody . _JSON
    case mResult of
      Just result -> pure result
      Nothing -> pure $ OtherError rsp

sendEmailPlain :: (MonadIO m) => ApiKey -> Session -> SendEmail -> m Result
sendEmailPlain key session e = sendEmail e (key, session)

authOptions :: ApiKey -> Options
authOptions key =
  defaults
    & header "Authorization" .~ ["Bearer " <> encodeUtf8 (key ^. _ApiKey)]
    -- We'd rather deal with status code problems as values
    & checkStatus .~ Just (\_ _ _ -> Nothing)
