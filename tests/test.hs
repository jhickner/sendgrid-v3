{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens                             ( (^.) )
import           Data.Either (isRight)
import           Data.List.NonEmpty                       ( fromList )
import           Data.Text                     as T
import           Network.SendGridV3.Api
import           Network.SendGridV3.Api.Templates
import           Network.Wreq
import           System.Environment
import           Test.Tasty
import           Test.Tasty.HUnit

testMail :: MailAddress -> Mail () ()
testMail addr = mail [personalization (fromList [addr])]
                     addr
                     "Mail Subject"
                     (fromList [mailContentText "Test Content"])

main :: IO ()
main = do
  sendgridKey  <- getSendGridKey
  testMailAddr <- getTestEmailAddress
  let templateId = TemplateId "d-233ffd1f4e314b279db9b32d5eebb687"
  defaultMain $ testGroup
    "SendGrid v3 API"
    [ testCase "Send email simple" $ do
      eResponse <- sendMail sendgridKey (testMail testMailAddr)
      case eResponse of
        Left  err -> error "Failed to send simple email"
        Right r   -> r ^. responseStatus . statusCode @?= 202
    , testCase "Send email with opts" $ do
      eResponse <- sendMail
        sendgridKey
        ((testMail testMailAddr) { _mailSendAt = Just 1516468000 })
      case eResponse of
        Left  err -> error "Failed to send email with opts"
        Right r   -> r ^. responseStatus . statusCode @?= 202
    , testCase "Send an email payload with categories correctly" $ do
      let email =
            (testMail testMailAddr) { _mailCategories = Just ["fake-category"] }
      eResponse <- sendMail sendgridKey email
      case eResponse of
        Left  err -> error "Failed to send email with opts"
        Right r   -> r ^. responseStatus . statusCode @?= 202

    -- NOTE: This test will fail if gzipped payloads are not enabled for your account
    -- (contact support to enable them)
    , testCase "Send email gzipped" $ do
      eResponse <- sendMailGZipped sendgridKey (testMail testMailAddr)
      case eResponse of
        Left  err -> error "Failed to send gzipped email"
        Right r   -> r ^. responseStatus . statusCode @?= 202

    , testCase "Get template" $ do
      eResponse <- getTemplate sendgridKey templateId
      isRight eResponse @? "Invalid response"

    , testCase "Get template html content" $ do
      eResponse <- getTemplateActiveHTMLContent sendgridKey templateId
      isRight eResponse @? "Invalid response"
    ]

getSendGridKey :: IO ApiKey
getSendGridKey = do
  envKey <- lookupEnv "SENDGRID_API_KEY"
  case envKey of
    Nothing ->
      error
        "Please supply a Sendgrid api key for testing via the ENV var `SENDGRID_API_KEY`"
    Just k -> return $ ApiKey $ T.pack k

getTestEmailAddress :: IO MailAddress
getTestEmailAddress = do
  envAddr <- lookupEnv "SENDGRID_TEST_MAIL"
  case envAddr of
    Nothing ->
      error
        "Please supply an email address for testing via the ENV var `SENDGRID_TEST_MAIL`"
    Just a -> return $ MailAddress (T.pack a) "John Doe"
