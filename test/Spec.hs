{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Arrow ((***))
import Control.Lens ((.~), (&), (%~))
import Control.Monad.Reader (runReaderT)
import Data.Aeson (object, (.=))
import Data.Tagged (Tagged(..))
import Data.Text as T (pack, Text)
import Data.These (These(..))
import Data.Time (UTCTime(..), fromGregorian)
import Data.Vector.Sized as V (Vector, empty, cons)
import GHC.TypeLits (type (+))
import Text.Email.Validate as E (unsafeEmailAddress)
import System.Environment (lookupEnv)
import Network.Wreq (checkResponse, defaults)
import Network.Wreq.Session (withSession, postWith)

import Network.API.SendGrid

main :: IO ()
main = do
  mKey <- fmap (Tagged . T.pack) <$> lookupEnv "API_KEY"
  withSession $ \session ->
    case mKey :: Maybe (Tagged ApiKey Text) of
      Just key -> print =<< runReaderT (sendEmail exampleEmail') (key, session)
      Nothing ->
        print =<<
        postWith (defaults & checkResponse .~ Just (\_ _ -> pure ())) session "http://requestb.in/10ebisf1" exampleEmail

infixr 5 #:
(#:) :: a -> Vector n a -> Vector (n + 1) a
a #: b = a `cons` b

exampleEmail':: SendEmail Text (Vector 1) (Vector 2) (Vector 2)
exampleEmail' =
  exampleEmail
  & files .~ [File "fileName.txt" "Attachment"]
  & date .~ Just (UTCTime (fromGregorian 2000 1 1) 0)
  & categories .~ ["transactional", "test"]
  & templateId .~ Just (Tagged "a96302be-bf72-409c-859b-cf4d317d0e2a")
  & inlineUnsubscribe .~ Just (Tagged 675)
  & ccsWipe .~ (unsafeEmailAddress "eric+2" "frontrowed.com" #: empty)
  & ccsWipe %~ (unsafeEmailAddress "eric+3" "frontrowed.com" #:)
  & ccNames .~ (Just $ "foo" #: "bar" #: empty)
  & bccsWipe .~ (unsafeEmailAddress "eric+4" "frontrowed.com" #: empty)
  & bccNames .~ (Just $ "baz" #: empty)
  & bccsAll %~ ((unsafeEmailAddress "eric+5" "frontrowed.com" #:) *** fmap ("qux" #:))
  & senderName .~ Just "oul"
  & recipientNames .~ (Just $ "me" #: empty)
  & smtp .~ Just (object ["hello" .= ("ignored" :: Text)])

exampleEmail :: SendEmail Text (Vector 1) (Vector 0) (Vector 0)
exampleEmail =
  mkSingleRecipEmail
    (unsafeEmailAddress "eric" "frontrowed.com")
    "Coming via sendgrid"
    (That "Text body")
    (unsafeEmailAddress "alex" "frontrowed.com")
