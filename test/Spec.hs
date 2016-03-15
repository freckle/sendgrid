{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.These (These(..))
import Data.Time (UTCTime(..), fromGregorian)
import Network.Wreq
import Text.Email.Validate as E (unsafeEmailAddress)

import Network.API.SendGrid.Types

main :: IO ()
main = print =<< post "http://requestb.in/o93m97o9" example

example :: SendEmail
example =
  (mkSendEmail
    (Left [NamedEmail (unsafeEmailAddress "foo" "foo.com") "FooFoo"])
    "Subject"
    (That "Text body")
    (unsafeEmailAddress "bar" "bar.com"))
    { sendFiles = Just [File "fileName" "fileBody"]
    , sendDate = Just (UTCTime (fromGregorian 1900 1 1) 0)
    }
