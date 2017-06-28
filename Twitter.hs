{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Twitter where


import Data.Aeson
import Network.HTTP.Conduit
import Network.HTTP.Simple hiding (httpLbs)
import Control.Applicative
import Control.Monad
import Web.Authenticate.OAuth
import Data.ByteString.Char8 (pack)

import GHC.Generics

data Tweet = Tweet {
        id :: Integer
      , text :: String
        } deriving (Generic, Show)

instance FromJSON Tweet

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
    httpLbs req' manager >>= (print . responseStatus)

getTweets :: String -> Integer -> Int-> IO [Tweet]
getTweets userID maxId count = do
    initReq <- parseRequest $
        "https://api.twitter.com/1.1/statuses/user_timeline.json?user_id="
        ++ userID ++ "&count=" ++ (show (if count > 180 then 180 else count))
        ++ (if maxId /= 0 then "&max_id=" ++ (show maxId) else [])
    req <- (flip (signOAuth auth)) initReq $ (uncurry newCredential) tokens
    response <- httpJSON req
    let tws = responseBody response
    let lastId = Twitter.id $ Prelude.last tws
    fol <- if count <= 180 then return []
                 else getTweets userID lastId (count-180)
    return $ tws++fol
