{-# LANGUAGE TupleSections #-}
import OAuth
import TweetJSON

import Network.HTTP
import Network.URI
import Text.JSON
import Data.Maybe
import Data.List
import System.IO
import System.Random
import System.Time
import Control.Arrow
import Control.Applicative
import Data.Digest.Pure.SHA
import qualified System.IO.UTF8 as U8
import Codec.Binary.UTF8.String (decodeString, encodeString)
import Data.Char
import qualified Codec.Binary.Base64 as B64
import qualified Data.ByteString.Lazy as L

-- アクセストークンを新規に取得
getAccessToken :: String -> String -> IO (Parameter, Parameter)
getAccessToken consumerKey consumerSecret = do
  putStrLn "Access Token is not found."
  let oauth_ = OAuth consumerKey consumerSecret "" ""
  -- リクエストトークン発行要求リクエスト生成
  requestForGetRequestToken <- oauthRequest oauth_ requestTokenURL "" []
  -- リクエストトークン取得
  requestTokenParameters <- (fmap $ parseParameter . rspBody) . simpleHTTPIO $ requestForGetRequestToken
  requestToken <- getParameter requestTokenParameters "oauth_token"
  requestTokenSecret <- getParameter requestTokenParameters "oauth_token_secret"
  -- 認証ページのアドレス表示
  putStrLn $ authorizeURL ++ "?" ++ urlEncodeVars [requestToken]
  -- PIN入力 -> oauth_verifierパラメータとして束縛
  verifier <- ("oauth_varifier",) <$> getLine
  -- アクセストークン発行要求リクエスト生成
  requestForGetAccessToken <- oauthRequest oauth_ accessTokenURL (snd requestTokenSecret) [requestToken, verifier]
  -- アクセストークン取得
  accessTokenParameters <- (fmap $ parseParameter . rspBody) . simpleHTTPIO $ requestForGetAccessToken
  accessToken <- getParameter accessTokenParameters "oauth_token"
  accessTokenSecret <- getParameter accessTokenParameters "oauth_token_secret"
  -- アクセストークン保持ファイルaccess.iniにアクセストークンをセーブ
  fout <- openFile "./access.ini" WriteMode
  hPutStrLn fout (snd accessToken)
  hPutStrLn fout (snd accessTokenSecret)
  hClose fout
  return (accessToken, accessTokenSecret)
  
-- アクセストークンを読み込む
restoreAccessToken :: IO (Parameter, Parameter)
restoreAccessToken = do
  -- アクセストークン読み込み
  fin <- openFile "./access.ini" ReadMode
  accessToken <- hGetLine fin
  accessTokenSecret <- hGetLine fin
  hClose fin
  putStrLn "Access Token successfully restored."
  return (("oauth_token", accessToken), ("oauth_token_secret", accessTokenSecret))

main :: IO ()
main = do
  -- Consumer Key / Consumer Secret読み込み
  fin <- openFile "./config.ini" ReadMode
  consumerKey <- hGetLine fin
  consumerSecret <- hGetLine fin
  hClose fin
  -- アクセストークン取得
  (accessToken, accessTokenSecret) <- restoreAccessToken `catch` \_ -> getAccessToken consumerKey consumerSecret
  let oauth = OAuth consumerKey consumerSecret (snd accessToken) (snd accessTokenSecret)
  -- タイムラインから最新のツイートを１つ取得
  newestTweet <- apiRequest oauth "home_timeline" GET []
  res <- simpleHTTPIO newestTweet
  let tryJSON = case decode (rspBody res) of
                  Ok a -> a
                  Error _ -> JSNull
  timeline <- (getTimeline tryJSON)
  mapM_ print timeline
  