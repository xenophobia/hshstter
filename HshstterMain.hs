{-# LANGUAGE TupleSections #-}

module HshstterMain where

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

import Graphics.UI.Gtk hiding (add, disconnect)
import Graphics.UI.Gtk.Glade
import Control.Concurrent

-- GUI�ǡ�����
data GUI = GUI {
      mainWin :: !Window,
      tweetEntry :: !Entry,
      tweetButton :: !Button,
      timeline :: !TextView,
      timelineWindow :: !ScrolledWindow,
      accessTokenGetWin :: !Window
    }

-- ���������ȡ�����򿷵��˼���
getNewAccessToken :: GUI -> String -> String -> IO (Parameter, Parameter)
getNewAccessToken gui consumerKey consumerSecret = do
  putStrLn "Access Token is not found."
  oauth_ <- newOAuth consumerKey consumerSecret "" ""
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

xmlNewIO :: FilePath -> IO GladeXML
xmlNewIO gladePath = do
  maybeXML <- xmlNew gladePath
  case maybeXML of
    Nothing -> fail "XML format error."
    Just xml -> return xml

-- .glade�ե������XML�����ˤ���ɡ�GUI�����Ѵ�
loadGlade :: FilePath -> IO GUI
loadGlade gladePath = do
  -- XML�����
  xml <- xmlNewIO gladePath

  -- �ᥤ�󥦥���ɥ������
  guiMainWin <- xmlGetWidget xml castToWindow "mainWin"
  -- ���������ȡ��������������ɥ������
  guiAccessTokenWin <- xmlGetWidget xml castToWindow "accessTokenGetWin"
  -- �ĥ����������������
  guiTweetEntry <- xmlGetWidget xml castToEntry "tweetEntry"
  -- �ĥ����ȥܥ�������
  guiTweetButton <- xmlGetWidget xml castToButton "tweetButton"
  -- ������饤��ɽ���������
  guiTimeline <- xmlGetWidget xml castToTextView "timeline"
  -- ��������С������
  guiTimelineWindow <- xmlGetWidget xml castToScrolledWindow "timelineWindow"
  
  return $ GUI guiMainWin guiTweetEntry guiTweetButton guiTimeline guiTimelineWindow guiAccessTokenWin

-- ������饤���ɽ��
showTimeline :: GUI -> OAuth -> IO Bool
showTimeline gui oauth = do
  -- ������饤�󤫤�ǿ��Υĥ����Ȥ����
  newestTweet <- apiRequest oauth "home_timeline" GET []
  res <- simpleHTTPIO newestTweet
  let tryJSON = case decode (rspBody res) of
                  Ok a -> a
                  Error _ -> JSNull
  tweets <- (getTimeline tryJSON)
  -- text buffer����
  buffer <- textViewGetBuffer (timeline gui)
  let tl = foldl (\s -> \t -> s ++ (show t) ++ "\n") "" tweets
  textBufferSetText buffer tl
  return True

-- �ĥ����Ȥ���
tweet :: GUI -> OAuth -> IO ()
tweet gui oauth = do
  tweetText <- entryGetText (tweetEntry gui)
  tweet <- apiRequest oauth "update" POST [("status", encodeString tweetText)]
  res <- simpleHTTPIO tweet
  -- �ĥ�������������ꥻ�å�
  entrySetText (tweetEntry gui) ""
  -- ������饤��򹹿�
  showTimeline gui oauth
  return ()

main :: FilePath -> IO ()
main gladePath = do

  -- GTK+�����ƥ�����
  initGUI

  -- ¾�Υ���åɤ����ˤ������褦�ˤ���
  timeoutAddFull (yield >> return True)
                 priorityDefaultIdle 100

  -- .glade�ե���������
  gui <- loadGlade gladePath  -- ������ɥ���ɽ��
  widgetShowAll . mainWin $ gui
  

  -- x�ܥ���ǥ�����ɥ���õ�
  onDestroy (mainWin gui) mainQuit
  
  -- OAuth��Ϣ
  -- Consumer Key / Consumer Secret�ɤ߹���
  fin <- openFile "./config.ini" ReadMode
  consumerKey <- hGetLine fin
  consumerSecret <- hGetLine fin
  hClose fin
  -- ���������ȡ��������
  (accessToken, accessTokenSecret) <- restoreAccessToken `catch` \_ -> getNewAccessToken gui consumerKey consumerSecret
  oauth <- newOAuth consumerKey consumerSecret (snd accessToken) (snd accessTokenSecret)

  -- ������饤��ɽ��
  showTimeline gui oauth
  timeoutAdd (showTimeline gui oauth) 30000

  -- "tweet"�ܥ���ǥĥ�����
  onClicked (tweetButton gui) (tweet gui oauth)

  -- �ᥤ��롼��
  mainGUI

