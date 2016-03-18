{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens ((.~), (&))
import Data.Aeson ((.=), object)
import Data.Text as T (pack)
import Data.These (These(..))
import Data.Time (UTCTime(..), fromGregorian)
import Text.Email.Validate as E (unsafeEmailAddress)
import System.Environment (lookupEnv)
import Network.Wreq.Session (withSession, post)

import Network.API.SendGrid

main :: IO ()
main = do
  mKey <- fmap (ApiKey . T.pack) <$> lookupEnv "API_KEY"
  withSession $ \session -> do
    case mKey of
      Just key -> print =<< sendEmail exampleEmail (key, session)
      Nothing -> print =<< post session "http://requestb.in/10ebisf1" exampleEmail

exampleEmail :: SendEmail
exampleEmail =
  mkSendEmail
    (Left [NamedEmail (unsafeEmailAddress "alex" "frontrowed.com") "FooFoo"])
    "Coming via sendgrid"
    (That "Text body")
    (unsafeEmailAddress "eric" "frontrowed.com")
  & sendFiles .~ [File "fileName.txt" "Attachment"]
  & sendDate .~ Just (UTCTime (fromGregorian 2000 1 1) 0)
  & sendCc . plainEmails .~ [unsafeEmailAddress "eric+2" "frontrowed.com"]
  -- Note: The previous line gets ignored because
  -- we can only use either all named emails or all unnamed emails
  & sendCc . namedEmails .~ [NamedEmail (unsafeEmailAddress "eric+1" "frontrowed.com") "Foo"]
  & sendCategories .~ ["transactional", "test"]
  & sendTemplateId .~ Just "a96302be-bf72-409c-859b-cf4d317d0e2a"
  & sendSmtp .~ Just (object [ "category" .= 'c', "arbitrary" .= 'd' ])
