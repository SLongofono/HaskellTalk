{-# LANGUAGE OverloadedStrings #-}

module Twitter where

import Data.Aeson
import Network.HTTP.Conduit
import Control.Applicative
import Control.Monad
import Web.Authenticate.OAuth
import Data.ByteString.Char8 (pack)

tokens = ("redacted",
          "redacted")

auth = newOAuth {
        oauthServerName = "api.twitter.com",
        oauthConsumerKey="redacted",
        oauthConsumerSecret="redacted"
        }

tweet :: String -> IO ()
tweet t = do
    initReq <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
    let req = (flip urlEncodedBody) initReq $ 
               [("status",pack t)]
    req' <- (flip (signOAuth auth)) req $ (uncurry newCredential) tokens
    manager <- newManager tlsManagerSettings
    _ <- httpLbs req' manager
    return ()
