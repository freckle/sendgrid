{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens ((.~), (&))
import Data.Text as T (pack)
import Data.These (These(..))
import Data.Time (UTCTime(..), fromGregorian)
import Text.Email.Validate as E (unsafeEmailAddress)
import System.Environment (getEnv)
import Network.Wreq.Session (withSession)

import Network.API.SendGrid

main :: IO ()
main =
  withSession $ \session -> do
    key <- ApiKey . T.pack <$> getEnv "API_KEY"
    print =<< sendEmail exampleEmail (key, session)

exampleEmail :: SendEmail
exampleEmail =
  (mkSendEmail
    (Left [NamedEmail (unsafeEmailAddress "eric" "frontrowed.com") "FooFoo"])
    "Coming via sendgrid"
    (That "Text body")
    (unsafeEmailAddress "eric" "frontrowed.com"))
  & sendFiles .~ [File "fileName.txt" "Attachment"]
  & sendDate .~ Just (UTCTime (fromGregorian 2000 1 1) 0)
  & sendCc . plainEmails .~ [unsafeEmailAddress "eric+2" "frontrowed.com"]
  -- Note: The previous line gets ignored because
  -- we can only use either all named emails or all unnamed emails
  & sendCc . namedEmails .~ [NamedEmail (unsafeEmailAddress "eric+1" "frontrowed.com") "Foo"]
  & sendSmtp . categories .~ ["transactional", "test"]
