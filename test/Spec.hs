{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text as T (pack)
import Data.These (These(..))
import Data.Time (UTCTime(..), fromGregorian)
import Text.Email.Validate as E (unsafeEmailAddress)
import System.Environment (getEnv)

import Network.API.SendGrid
import Network.API.SendGrid.Types

main :: IO ()
main = do
  key <- ApiKey . T.pack <$> getEnv "API_KEY"
  print =<< runExceptT (runReaderT (sendEmail exampleEmail) key)

exampleEmail :: SendEmail
exampleEmail =
  (mkSendEmail
    (Left [NamedEmail (unsafeEmailAddress "alex" "frontrowed.com") "FooFoo"])
    "Coming via sendgrid"
    (That "Text body")
    (unsafeEmailAddress "eric" "frontrowed.com"))
    { sendFiles = Just [File "fileName.txt" "Attachment"]
    , sendDate = Just (UTCTime (fromGregorian 2000 1 1) 0)
    }
