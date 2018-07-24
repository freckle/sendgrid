{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Arrow ((***))
import Control.Lens ((.~), (&), (%~))
import Data.Functor.Const (Const)
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
(#:) :: a -> Vector n a -> Vector (1 + n) a
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

exampleEmail :: SendEmail Text (Vector 1) (Const ()) (Const ())
exampleEmail =
  mkSingleRecipEmail
    (unsafeEmailAddress "eric" "frontrowed.com")
    "Coming via sendgrid"
    (That "Text body")
    (unsafeEmailAddress "alex" "frontrowed.com")








email1 :: SendEmail Text (Vector 1) (Const ()) (Const ())
email1 =
  let [sender, recipient] = map (uncurry unsafeEmailAddress) [
        ("sender", "source.com"),
        ("receiver", "person.com")
        ]
  in
    mkSingleRecipEmail
      recipient
      "Coming via sendgrid"
      (That "Text body")
      sender

email2 :: SendEmail Text (Vector 1) (Const ()) (Const ())
email2 = email1
  & categories .~ ["email", "test", "foo", "bar"]
  & templateId .~ Just "ff469da0-4a45-4263-2414-5ac770565e4d"

email3 =
  let cc1 = unsafeEmailAddress "cc1" "example.com"
      cc2 = unsafeEmailAddress "cc2" "example.com"
      n1  = "CC 1"
      n2  = "CC 2"
  in email2
     & ccsAll .~ (cc2 #: cc1 #: empty, Just $ n2 #: n1 #: empty)

email4 =
  let cc1 = unsafeEmailAddress "cc1" "example.com"
      cc2 = unsafeEmailAddress "cc2" "example.com"
      n1  = "CC 1"
      n2  = "CC 2"
  in email2
     & ccsWipe .~ (cc2 #: cc1 #: empty)
     & ccNames .~ (Just $ n2 #: n1 #: empty)


main' = do
  key <- fmap (Tagged . T.pack) <$> lookupEnv "API_KEY"
  maybe
    (fail "Could not lookup API_KEY in environment")
    sendMyEmail
    key

sendMyEmail :: Tagged ApiKey Text -> IO ()
sendMyEmail apikey = do
  result <- withSession $ \session ->
    runReaderT (sendEmail email4) (apikey, session)
  print result

