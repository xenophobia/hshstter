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

-- Request TokenをParameter型で取得
getRequestTokenParameter :: OAuth -> IO (Parameter, Parameter)
getRequestTokenParameter oauth = do
  -- Request Token取得
  requestTokenParameters <- parseParameter <$> oauthRequest oauth requestTokenURL "" []
  [requestToken, requestTokenSecret] <- mapM (getParameter requestTokenParameters) ["oauth_token", "oauth_token_secret"]
  return (requestToken, requestTokenSecret)

-- AccessTokenをString型で取得
getAccessTokenString :: OAuth -> String -> [Parameter] -> IO [String]
getAccessTokenString oauth requestTokenSecret param = do
  -- Access Token取得
  accessTokenParameters <- parseParameter <$> oauthRequest oauth accessTokenURL requestTokenSecret param
  mapM (fmap snd . getParameter accessTokenParameters) ["oauth_token", "oauth_token_secret", "user_id", "screen_name"]

-- タイムラインのデータを取得
getTimelineData :: OAuth -> [Parameter] -> String -> IO [Tweet]
getTimelineData oauth param dataName = getTweetList =<< apiRequest oauth ("/1/statuses/" ++ dataName) GET param

data TweetErrorType = EmptyTweet | CharactorExceeded | APIError deriving (Show, Typeable)
data TweetError = TweetError TweetErrorType deriving (Show, Typeable)
instance Exception TweetError

-- ツイートを送信する
sendTweet :: OAuth -> String -> IO ()
sendTweet oauth tweetText = do
  let lengthOfTweet = length tweetText
  if lengthOfTweet == 0
    then throw (TweetError EmptyTweet)
    else if lengthOfTweet > 140 then throw (TweetError CharactorExceeded)
    else const () <$> apiRequest oauth "/1/statuses/update" POST [("status", encodeString tweetText)] `catch` \(_::SomeException) -> throw (TweetError APIError)

-- アイコン画像を取得
getIcon :: String -> IO String
getIcon url = do
  let outFile = "./icon/" ++ urlEncode url
  fileExist <- doesFileExist outFile
  if fileExist
    then return outFile
    else do
      let request = defaultGETRequest $ fromJust $ parseURI url
      iconImage <- getResponseBody =<< simpleHTTP request
      fin <- openBinaryFile outFile WriteMode
      hPutStr fin iconImage
      hClose fin
      return outFile

type ID = String
-- リツイート
retweet :: OAuth -> ID -> IO ()
retweet oauth rt_id = const () <$> apiRequest oauth ("/1/statuses/retweet/" ++ rt_id) POST [("id", rt_id)] `catch` \(_::SomeException) -> throw (TweetError APIError)

-- ふぁぼ
favorite :: OAuth -> ID -> IO ()
favorite oauth fav_id = const () <$> apiRequest oauth ("/1/favorites/create/" ++ fav_id) POST [("id", fav_id)] `catch` \(_::SomeException) -> throw (TweetError APIError)