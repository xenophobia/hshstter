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

-- ���������ȡ�����򿷵��˼���
getAccessToken :: String -> String -> IO (Parameter, Parameter)
getAccessToken consumerKey consumerSecret = do
  putStrLn "Access Token is not found."
  let oauth_ = OAuth consumerKey consumerSecret "" ""
  -- �ꥯ�����ȥȡ�����ȯ���׵�ꥯ����������
  requestForGetRequestToken <- oauthRequest oauth_ requestTokenURL "" []
  -- �ꥯ�����ȥȡ��������
  requestTokenParameters <- (fmap $ parseParameter . rspBody) . simpleHTTPIO $ requestForGetRequestToken
  requestToken <- getParameter requestTokenParameters "oauth_token"
  requestTokenSecret <- getParameter requestTokenParameters "oauth_token_secret"
  -- ǧ�ڥڡ����Υ��ɥ쥹ɽ��
  putStrLn $ authorizeURL ++ "?" ++ urlEncodeVars [requestToken]
  -- PIN���� -> oauth_verifier�ѥ�᡼���Ȥ���«��
  verifier <- ("oauth_varifier",) <$> getLine
  -- ���������ȡ�����ȯ���׵�ꥯ����������
  requestForGetAccessToken <- oauthRequest oauth_ accessTokenURL (snd requestTokenSecret) [requestToken, verifier]
  -- ���������ȡ��������
  accessTokenParameters <- (fmap $ parseParameter . rspBody) . simpleHTTPIO $ requestForGetAccessToken
  accessToken <- getParameter accessTokenParameters "oauth_token"
  accessTokenSecret <- getParameter accessTokenParameters "oauth_token_secret"
  -- ���������ȡ������ݻ��ե�����access.ini�˥��������ȡ�����򥻡���
  fout <- openFile "./access.ini" WriteMode
  hPutStrLn fout (snd accessToken)
  hPutStrLn fout (snd accessTokenSecret)
  hClose fout
  return (accessToken, accessTokenSecret)
  
-- ���������ȡ�������ɤ߹���
restoreAccessToken :: IO (Parameter, Parameter)
restoreAccessToken = do
  -- ���������ȡ������ɤ߹���
  fin <- openFile "./access.ini" ReadMode
  accessToken <- hGetLine fin
  accessTokenSecret <- hGetLine fin
  hClose fin
  putStrLn "Access Token successfully restored."
  return (("oauth_token", accessToken), ("oauth_token_secret", accessTokenSecret))

main :: IO ()
main = do
  -- Consumer Key / Consumer Secret�ɤ߹���
  fin <- openFile "./config.ini" ReadMode
  consumerKey <- hGetLine fin
  consumerSecret <- hGetLine fin
  hClose fin
  -- ���������ȡ��������
  (accessToken, accessTokenSecret) <- restoreAccessToken `catch` \_ -> getAccessToken consumerKey consumerSecret
  let oauth = OAuth consumerKey consumerSecret (snd accessToken) (snd accessTokenSecret)
  -- ������饤�󤫤�ǿ��Υĥ����Ȥ򣱤ļ���
  newestTweet <- apiRequest oauth "home_timeline" GET []
  res <- simpleHTTPIO newestTweet
  let tryJSON = case decode (rspBody res) of
                  Ok a -> a
                  Error _ -> JSNull
  timeline <- (getTimeline tryJSON)
  mapM_ print timeline
  