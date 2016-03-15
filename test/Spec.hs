{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text as T (pack)
import Data.These (These(..))
import Data.Time (UTCTime(..), fromGregorian)
import Text.Email.Validate as E (unsafeEmailAddress)
import System.Environment (getEnv)
import Network.Wreq.Session (withSession)

import Network.API.SendGrid
import Network.API.SendGrid.Types

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
    { sendFiles = Just [File "fileName.txt" "Attachment"]
    , sendDate = Just (UTCTime (fromGregorian 2000 1 1) 0)
    }
