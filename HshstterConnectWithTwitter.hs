{-# LANGUAGE TupleSections, ScopedTypeVariables, DeriveDataTypeable #-}

module HshstterConnectWithTwitter where

import OAuth
import TweetJSON

import Data.List
import Data.Maybe
import Prelude hiding (catch)
import Data.Typeable
import System.FilePath
import Network.HTTP
import System.IO
import Network.URI
import System.Directory
import Control.Exception
import Network.HTTP
import Control.Monad
import Control.Applicative
import Codec.Binary.UTF8.String (decodeString, encodeString)

-- Request Token��Parameter���Ǽ���
getRequestTokenParameter :: OAuth -> IO (Parameter, Parameter)
getRequestTokenParameter oauth = do
  -- Request Token����
  requestTokenParameters <- parseParameter <$> oauthRequest oauth requestTokenURL "" []
  [requestToken, requestTokenSecret] <- mapM (getParameter requestTokenParameters) ["oauth_token", "oauth_token_secret"]
  return (requestToken, requestTokenSecret)

-- AccessToken��String���Ǽ���
getAccessTokenString :: OAuth -> String -> [Parameter] -> IO [String]
getAccessTokenString oauth requestTokenSecret param = do
  -- Access Token����
  accessTokenParameters <- parseParameter <$> oauthRequest oauth accessTokenURL requestTokenSecret param
  mapM (fmap snd . getParameter accessTokenParameters) ["oauth_token", "oauth_token_secret", "user_id", "screen_name"]

-- ������饤��Υǡ��������
getTimelineData :: OAuth -> [Parameter] -> String -> IO [Tweet]
getTimelineData oauth param dataName = getTweetList =<< apiRequest oauth ("/1/statuses/" ++ dataName) GET param

data TweetErrorType = EmptyTweet | CharactorExceeded | APIError deriving (Show, Typeable)
data TweetError = TweetError TweetErrorType deriving (Show, Typeable)
instance Exception TweetError

-- �ĥ����Ȥ���������
sendTweet :: OAuth -> String -> IO String
sendTweet oauth tweetText = do
  let lengthOfTweet = length tweetText
  if lengthOfTweet == 0
    then throw (TweetError EmptyTweet)
    else if lengthOfTweet > 140 then throw (TweetError CharactorExceeded)
    else apiRequest oauth "/1/statuses/update" POST [("status", encodeString tweetText)] `catch` \(_::SomeException) -> throw (TweetError APIError)

-- ���������������������
downroadIcon :: String -> IO String
downroadIcon url = do
  let request = defaultGETRequest $ fromJust $ parseURI url
      outFile = "./icon/" ++ takeFileName url
  iconImage <- getResponseBody =<< simpleHTTP request
  fin <- openBinaryFile outFile WriteMode
  hPutStr fin iconImage
  hClose fin
  return outFile