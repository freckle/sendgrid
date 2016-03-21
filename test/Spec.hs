{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens ((.~), (&))
import Data.Tagged (Tagged(..))
import Data.Text as T (pack, Text)
import Data.These (These(..))
import Data.Time (UTCTime(..), fromGregorian)
import Text.Email.Validate as E (unsafeEmailAddress)
import System.Environment (lookupEnv)
import Network.Wreq (checkStatus, defaults)
import Network.Wreq.Session (withSession, postWith)

import Network.API.SendGrid

main :: IO ()
main = do
  mKey <- fmap (Tagged . T.pack) <$> lookupEnv "API_KEY"
  withSession $ \session ->
    case mKey of
      Just key -> print =<< sendEmail exampleEmail (key, session)
      Nothing ->
        print =<<
        postWith (defaults & checkStatus .~ Just (\_ _ _ -> Nothing)) session "http://requestb.in/10ebisf1" exampleEmail

exampleEmail :: SendEmail Text
exampleEmail =
  mkSingleRecipEmail
    (unsafeEmailAddress "eric" "frontrowed.com")
    "Coming via sendgrid"
    (That "Text body")
    (unsafeEmailAddress "alex" "frontrowed.com")
  & files .~ [File "fileName.txt" "Attachment"]
  & date .~ Just (UTCTime (fromGregorian 2000 1 1) 0)
  & categories .~ ["transactional", "test"]
  & templateId .~ Just (Tagged "a96302be-bf72-409c-859b-cf4d317d0e2a")
  & inlineUnsubscribe .~ Just (Tagged 675)
