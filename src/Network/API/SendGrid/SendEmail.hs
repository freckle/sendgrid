{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Contains the types and functions necessary for sending an email via SendGrid.
module Network.API.SendGrid.SendEmail where

import Control.Lens (makeLenses, Lens', lens, (^?))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Data.Aeson hiding (Result(..))
import Data.Aeson.Lens (_JSON)
import Data.ByteString as BS (ByteString)
import Data.ByteString.Lazy as BSL (toStrict)
import qualified Data.DList as D
import Data.HashMap.Strict as H (empty)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList, fromMaybe)
import Data.Monoid ((<>))
import Data.Text as T (Text, pack, unpack)
import Data.These (These(..))
import Data.Time (UTCTime(..), defaultTimeLocale, formatTime)
import Network.HTTP.Client (RequestBody(RequestBodyBS))
import Network.HTTP.Client.MultipartFormData (partFileRequestBody)
import Network.HTTP.Types.Header (Header)
import Network.Wreq (responseBody, partBS, partText, Part)
import Network.Wreq.Session (postWith, Session)
import Network.Wreq.Types (Postable(..))
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Email.Validate as E (EmailAddress, toByteString)

import Network.API.SendGrid.Core

sendEmailEndPoint :: Text
sendEmailEndPoint = baseSendGridUrl <> "mail.send.json"

-- * Types

-- | Type for attached files.
-- Consists of content and the name that content will appear under in the email.
data File
 = File
  { _fileName    :: Text
  , _fileContent :: BS.ByteString
  } deriving (Eq, Show)
makeLenses ''File

-- | Type for SendGrid's content field.
-- Consists of a file and name by which you can refer to that file.
data Content
  = Content
  { _file :: File
  , _contentId   :: Text
  } deriving (Eq, Show)
makeLenses ''Content

-- | The configuration type for your email sending request.
data SendEmail
 = SendEmail
  { _recipients :: Either (NonEmpty NamedEmail) (NonEmpty EmailAddress)
  , _replyTo    :: Maybe EmailAddress
  , _cc         :: Maybe (Either (NonEmpty NamedEmail) (NonEmpty EmailAddress))
  , _bcc        :: Maybe (Either (NonEmpty NamedEmail) (NonEmpty EmailAddress))
  , _sender     :: EmailAddress
  , _senderName :: Maybe Text
  , _subject    :: Text
  , _body       :: These Html Text
  , _date       :: Maybe UTCTime
  , _files      :: [File] -- ^ Don't duplicate files from @_content@ here. @_content@ already handles the file attaching.
  , _content    :: [Content]
  , _headers    :: [Header]
  , _categories :: [Text] -- ^ A typeful way to specify this subpart of the SendGrid's SMTP param
  , _templateId :: Maybe Text -- ^ A typeful way to specify this subpart of the SendGrid's SMTP param
  , _smtp       :: Maybe Value
    -- ^ Escape hatch for other uses of SendGrid's SMTP param.
    -- If the keys you define here don't overlap with @categories@ or @templateId@, everything will be merged sensibly.
    -- If they do overlap, the @_categories@ and @_templateId@ take precedence.
  }
-- Can't derive @Eq@ or @Show@ because of @Html@
makeLenses ''SendEmail

-- | Makes using @cc@ and @bcc@ friendlier. e.g.
--
-- @email & bcc . plainEmails .~ [ email1, email2 ]@
plainEmails :: Lens' (Maybe (Either (NonEmpty NamedEmail) (NonEmpty EmailAddress))) [EmailAddress]
plainEmails =
  lens getter setter
    where
      getter (Just (Right xs)) = NE.toList xs
      getter _ = []
      setter _ [] = Nothing
      setter _ (x : xs) = Just (Right $ x :| xs)

-- | Makes using @cc@ and @bcc@ friendlier. e.g.
--
-- @email & bcc . namedEmails .~ [ namedEmail1, namedEmail2 ]@
namedEmails :: Lens' (Maybe (Either (NonEmpty NamedEmail) (NonEmpty EmailAddress))) [NamedEmail]
namedEmails =
  lens getter setter
    where
      getter (Just (Left xs)) = NE.toList xs
      getter _ = []
      setter _ [] = Nothing
      setter _ (x : xs) = Just (Left $ x :| xs)

-- * Constructors

-- | Helper constructor to make the minimal @SendEmail@,
-- one with everything possible set to @Nothing@ or @[]@.
mkSendEmail :: Either (NonEmpty NamedEmail) (NonEmpty EmailAddress) -> Text -> These Html Text -> EmailAddress -> SendEmail
mkSendEmail to subject' body' from
  = SendEmail
  { _recipients = to
  , _replyTo    = Nothing
  , _cc         = Nothing
  , _bcc        = Nothing
  , _sender     = from
  , _senderName = Nothing
  , _subject    = subject'
  , _body       = body'
  , _date       = Nothing
  , _files      = []
  , _content    = []
  , _headers    = []
  , _categories = []
  , _templateId = Nothing
  , _smtp       = Nothing
  }

-- | Convenience constructor to make an email intended for a single recipient.
mkSingleRecipEmail :: EmailAddress -> Text -> These Html Text -> EmailAddress -> SendEmail
mkSingleRecipEmail to = mkSendEmail (Right $ pure to)

-- * Serializing @SendEmail@ for SendGrid

instance Postable SendEmail where
  postPayload = postPayload . sendEmailToParts

sendEmailToParts :: SendEmail -> [Part]
sendEmailToParts SendEmail{..} =
  D.toList $ foldMap D.fromList
  [ toParts
  , [subjectPart]
  , bodyParts
  , [fromPart]
  , ccParts
  , bccParts
  , fromNamePart
  , replyToPart
  , datePart
  , headerPart
  , fileParts
  , contentParts
  , smtpPart
  ]
    where
      fileToPart File{..} = partFileRequestBody ("files[" <> _fileName  <> "]") (T.unpack _fileName) (RequestBodyBS _fileContent)
      contentParts = foldMap contentToParts _content
        where
          contentToParts (Content file'@File{..} cId) =
            [fileToPart file', partText ("content[" <> _fileName <> "]") cId]
      fileParts = fileToPart <$> _files
      toParts = emailsToParts "to[]" "toname[]" _recipients
      fromPart = partBS "from" $ E.toByteString _sender
      ccParts = maybe [] (emailsToParts "cc[]" "ccname[]") _cc
      bccParts = maybe [] (emailsToParts "bcc[]" "bccname[]") _bcc
      fromNamePart = maybeToList $ partText "fromname" <$> _senderName
      replyToPart = maybeToList $ partBS "replyto" . E.toByteString <$> _replyTo
      datePart = maybeToList $ partText "date" . T.pack . formatTime defaultTimeLocale sendGridDateFormat <$> _date
      subjectPart = partText "subject" _subject
      smtpPart =
        maybe [] (pure . partBS "x-smtpapi" . BSL.toStrict . encode) $
        smtpValue _templateId _categories _smtp
      headerPart =
        case _headers of
          [] -> []
          headers' -> [partBS "headers" . headersToBS $ headers']
      bodyParts =
        case _body of
          This html -> [partBS "html" . BSL.toStrict $ renderHtml html]
          That text -> [partText "text" text]
          These html text -> [partBS "html" . BSL.toStrict $ renderHtml html, partText "text" text]

emailsToParts :: Text -> Text -> Either (NonEmpty NamedEmail) (NonEmpty EmailAddress) -> [Part]
emailsToParts emailKey nameKey (Left namedEmails') =
  flip foldMap namedEmails'
     (\NamedEmail{..} ->
       [ partBS emailKey $ E.toByteString _email
       , partText nameKey _name
       ])
emailsToParts emailKey _ (Right emails) =
  partBS emailKey . E.toByteString <$> NE.toList emails

smtpValue :: Maybe Text -> [Text] -> Maybe Value -> Maybe Value
smtpValue templateId' categories' custom =
  if merged == mempty'
  then Nothing
  else Just merged
  where
    merged =
      foldl'
        mergeObjects
        mempty'
        [ maybe mempty' templateIdToSmtpHeader templateId'
        , maybe mempty' categoriesToSmtpHeader $ NE.nonEmpty categories'
        , fromMaybe mempty' custom
        ]
    mempty' = Object H.empty

categoriesToSmtpHeader :: NonEmpty Text -> Value
categoriesToSmtpHeader cs =
  object
    [ "category" .= toJSON (NE.toList cs)
    ]

templateIdToSmtpHeader :: Text -> Value
templateIdToSmtpHeader tId =
  object
    [ "filters" .=
      object
        [ "templates" .=
          object
            [ "settings" .=
              object
                [ "template_id" .= tId
                , "enable" .= (1 :: Int)
                ]
            ]
        ]
    ]


-- * Sending email

-- | Simple function for sending email via SendGrid.
sendEmailSimple :: (MonadIO m) => ApiKey -> Session -> SendEmail -> m Result
sendEmailSimple key session e = sendEmail e (key, session)

-- | This type signature allows you to use @sendEmail@ as either
--
-- @SendEmail -> ReaderT (ApiKey, Session) IO (IO Result)@
--
-- or
--
-- @SendEmail -> (ApiKey, Session) -> IO Result@
--
-- In the first form, it also has the useful effect of discouraging you from sending emails
-- in the midst of another action.
sendEmail :: (MonadReader (ApiKey, Session) n, MonadIO m) => SendEmail -> n (m Result)
sendEmail e = do
  (key, session) <- ask
  pure $ do
    rsp <- liftIO $ postWith (authOptions key) session (T.unpack sendEmailEndPoint) e
    let mResult = rsp ^? responseBody . _JSON
    case mResult of
      Just result -> pure result
      Nothing -> pure $ OtherError rsp
