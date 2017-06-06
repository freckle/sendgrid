# SendGrid

Haskell interface to the [SendGrid API].


# Quick Start

tl;dr:

- install with [Stack]
- use `mkSingleRecip` email to construct an email
- use [Lenses] to update properties of the email (such as [templates],
  [categories], or [unsubscribe group]s.
- run the reader monad with the `ApiKey`

## Requirements:

- [Stack]

## Installation:

Add the following to your `stack.yaml` under `packages`:

```yaml
packages:
- location:
    git: git://github.com/frontrowed/sendgrid
    commit: <COMMIT>
```

## Usage

The primary data type is `SendEmail`, which represents the components
of an email message that may be sent via SendGrid. Properties of a
`SendEmail` include the standard email components such as sender, CC
and BCC recipients, attachements, and so forth. Additionally,
SendGrid-specific attributes include [templates], [categories], and
[unsubscribe group]s.

Consider the following definition of a simple email from
`sender@source.com` to `receiver@person.com`. It has the subject
"Coming via sendgrid" and contents "Text body". The type indicates
that the message categories are `Text`, one recipient (`Vector 1`),
and no CC or BCC recipients (`Vector 0`).

```haskell
email1 :: SendEmail Text (Vector 1) (Vector 0) (Vector 0)
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
```

`SendEMail` types can additionaly be modified using [Lenses] to modify
the components. Use the `SendEmail` lenses and lens operator set the
desired attributes. For instance, to set the [categories] and [unsubscribe group]:

```haskell
email2 :: SendMail Text (Vector 1) (Vector 0) (Vector 0)
email2 = email1
  & categories .~ ["email", "test", "foo", "bar"]
  & templateId .~ Just "ff469da0-4a45-4263-2414-5ac770565e4d"
```

We use type indices to typecheck the number of CC and BCC recipients.
To set and add to each of these, you need to construct a [typechecked vector]

To add CC and BCC recipients you can use the `ccsAll` and ``bccsAll`
lens, which accepts a tuple of type-indexed vectors where the second
is wrapped with Maybe.

```haskell
email3 =
  let cc1 = unsafeEmailAddress "cc1" "example.com"
      cc2 = unsafeEmailAddress "cc2" "example.com"
      n1  = "CC 1 name"
      n2  = "CC 2 name"
  in email2
     & ccsAll .~ (cc2 #: cc1 #: empty, Just $ n2 #: n1 #: empty)
```

This can be rather unwieldy, so the `ccsWipe` and `bccsWipe` lenses
are provided.  As the names hint at, these "wipe" the names list so
make sure to use the paired `ccNames` and `bccNames` lenses to set
the names.

```haskell
email4 :: SendMail Text (Vector 1) (Vector 2) (Vector 0)
email4 =
  let cc1 = unsafeEmailAddress "cc1" "example.com"
      cc2 = unsafeEmailAddress "cc2" "example.com"
      n1  = "CC 1"
      n2  = "CC 2"
  in email2
     & ccsWipe .~ (cc2 #: cc1 #: empty)
     & ccNames .~ (Just $ n2 #: n1 #: empty)
```


Finally, to actually send an email, you'll need to use an `ApiKey` and
construct a [Wreq] session.  One approach is to set this as an
environmental variable and then load it using
`System.Environment.lookupEnv`

```haskell
main = do
  key <- fmap (Tagged . T.pack) <$> lookupEnv "API_KEY"
  maybe
    (fail "Could not lookup API_KEY in environment")
    sendMyEmail
    key
```

The `sendEmail` function operates in the Reader Monad with
configuration given as 2-tuple type `(Tagged ApiKey Text, Session)`.


```haskell
sendMyEmail :: Tagged ApiKey Text -> IO ()
sendMyEmail apikey = do
  result <- withSession $ \session ->
    runReaderT (sendEmail email4) (apikey, session)
  print result
```



[SendGrid API]: https://sendgrid.com/docs/API_Reference/index.html
[Stack]: https://docs.haskellstack.org/en/stable/README/
[templates]: https://sendgrid.com/docs/User_Guide/Transactional_Templates/index.html
[categories]: https://sendgrid.com/docs/User_Guide/Statistics/categories.html
[unsubscribe group]: https://sendgrid.com/docs/User_Guide/Suppressions/index.html
[Lenses]: https://hackage.haskell.org/package/lens-tutorial-1.0.2/docs/Control-Lens-Tutorial.html
[typechecked vector]: https://hackage.haskell.org/package/vector-sized
[Wreq]: https://hackage.haskell.org/package/wreq
