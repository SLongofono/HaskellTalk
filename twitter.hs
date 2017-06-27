{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Control.Applicative
import Control.Monad

newtype BearerToken = BearerToken {token :: String} deriving Show

instance FromJSON BearerToken where
    parseJSON (Object v) = BearerToken <$> v .: "access_token" 

    parseJSON _ = mzero

main = do
    initReq <- parseUrl "https://api.twitter.com/oauth2/token"
    let req' = initReq {secure=True, requestHeaders=
                            [("Authorization","Basic redacted")]}
    let req = (flip urlEncodedBody) req' $
               [ ("grant_type","client_credentials")]
    manager <- newManager tlsManagerSettings
    response <- (httpJSON req) :: IO (Response BearerToken)
    print $ token $ getResponseBody response
