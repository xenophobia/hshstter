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
      accessTokenGetWin :: !Window,
      hint :: !Label,
      authorizationButton :: !Button,
      cancelButton :: !Button,
      pinEntry :: !Entry,
      authorizationURL :: !Entry
    }

-- ���������ȡ�����������Ƥ��ʤ��ä���硢OAuthǧ�ڤ�桼���˹ԤʤäƤ�餦
authorization :: GUI -> OAuth -> IO ()
authorization gui oauth = do
  widgetShowAll (accessTokenGetWin gui)
  -- �ꥯ�����ȥȡ�����ȯ���׵�ꥯ����������
  requestForGetRequestToken <- oauthRequest oauth requestTokenURL "" []
  -- �ꥯ�����ȥȡ��������
  requestTokenParameters <- (fmap $ parseParameter . rspBody) . simpleHTTPIO $ requestForGetRequestToken
  requestToken <- getParameter requestTokenParameters "oauth_token"
  requestTokenSecret <- getParameter requestTokenParameters "oauth_token_secret"
  
  -- ǧ�ڥڡ�����URL����
  entrySetText (authorizationURL gui) (authorizeURL ++ "?" ++ urlEncodeVars [requestToken])
  onClicked (authorizationButton gui) $ tryGetNewAccessToken gui oauth requestToken requestTokenSecret
  return ()
      where
        tryGetNewAccessToken gui oauth requestToken requestTokenSecret =
            getNewAccessToken gui oauth requestToken requestTokenSecret
                                  `catch` \_ -> do
                                    -- ǧ�ڤ˼��Ԥ�����ƻ��
                                    entrySetText (pinEntry gui) ""
                                    labelSetText (hint gui) "Sorry, Failed to authorize your account. Please try again."

-- ���������ȡ�����򿷵��˼���
getNewAccessToken :: GUI -> OAuth -> Parameter -> Parameter -> IO ()
getNewAccessToken gui oauth requestToken requestTokenSecret = do
  -- PIN���� -> oauth_verifier�ѥ�᡼���Ȥ���«��
  verifier <- ("oauth_varifier",) <$> entryGetText (pinEntry gui)
  -- ���������ȡ�����ȯ���׵�ꥯ����������
  requestForGetAccessToken <- oauthRequest oauth accessTokenURL (snd requestTokenSecret) [requestToken, verifier]
  -- ���������ȡ��������
  accessTokenParameters <- (fmap $ parseParameter . rspBody) . simpleHTTPIO $ requestForGetAccessToken
  accessToken <- getParameter accessTokenParameters "oauth_token"
  accessTokenSecret <- getParameter accessTokenParameters "oauth_token_secret"
  -- ���������ȡ������ݻ��ե�����access.ini�˥��������ȡ�����򥻡���
  fout <- openFile "./access.ini" WriteMode
  hPutStrLn fout (snd accessToken)
  hPutStrLn fout (snd accessTokenSecret)
  hClose fout
  setAccessToken oauth (snd accessToken)
  setAccessTokenSecret oauth (snd accessTokenSecret)
  mainRoutine gui oauth

-- ���������ȡ�������ɤ߹���
restoreAccessToken :: GUI -> OAuth -> IO ()
restoreAccessToken gui oauth = do
  -- ���������ȡ������ɤ߹���
  fin <- openFile "./access.ini" ReadMode
  accessToken <- hGetLine fin
  accessTokenSecret <- hGetLine fin
  hClose fin
  setAccessToken oauth accessToken
  setAccessTokenSecret oauth accessTokenSecret
  mainRoutine gui oauth

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
  -- �ĥ����������������
  guiTweetEntry <- xmlGetWidget xml castToEntry "tweetEntry"
  -- �ĥ����ȥܥ�������
  guiTweetButton <- xmlGetWidget xml castToButton "tweetButton"
  -- ������饤��ɽ���������
  guiTimeline <- xmlGetWidget xml castToTextView "timeline"
  -- ��������С������
  guiTimelineWindow <- xmlGetWidget xml castToScrolledWindow "timelineWindow"
  -- ���������ȡ��������������ɥ������
  guiAccessTokenWin <- xmlGetWidget xml castToWindow "accessTokenGetWin"
  -- ǧ���ѥ�å����������
  guiHint <- xmlGetWidget xml castToLabel "hint"
  -- Authorization�ܥ�������
  guiAuthorizationButton <- xmlGetWidget xml castToButton "authorizationButton"
  -- Authorization Cancel�ܥ�������
  guiCancelButton <- xmlGetWidget xml castToButton "cancelButton"
  -- PIN�����������
  guiPinEntry <- xmlGetWidget xml castToEntry "pinEntry"
  -- ǧ����URLɽ��
  guiAuthorizationURL <- xmlGetWidget xml castToEntry "authorizationURL"
  
  return $ GUI guiMainWin guiTweetEntry guiTweetButton guiTimeline guiTimelineWindow
                      guiAccessTokenWin guiHint guiAuthorizationButton guiCancelButton guiPinEntry guiAuthorizationURL

-- ������饤���ɽ��
showTimeline :: GUI -> OAuth -> IO Bool
showTimeline gui oauth = do
  -- ������饤�󤫤�ǿ��Υĥ����Ȥ����
  newestTweet <- apiRequest oauth "home_timeline" GET []
  res <- (fmap rspBody . simpleHTTPIO) newestTweet `catch` \_ -> return "error"
  let tryJSON = case decode res of
                  Ok a -> a
                  Error _ -> JSNull
  tweets <- getTimeline tryJSON `catch` \_ -> return []
  if null tweets then
      return False
  else
      do
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
  simpleHTTPIO_ tweet `catch` \_ -> return ()
  -- �ĥ�������������ꥻ�å�
  entrySetText (tweetEntry gui) ""
  -- ������饤��򹹿�
  showTimeline gui oauth
  return ()

-- �ᥤ�󥦥���ɥ�ɽ����������饤��ɽ�������������޺�ư���ĥ����ȥܥ���˥ϥ�ɥ������ʡ�ǧ���ѥ�����ɥ��õ��
mainRoutine :: GUI -> OAuth -> IO ()
mainRoutine gui oauth = do
  -- �ᥤ�󥦥���ɥ�ɽ��
  widgetShowAll (mainWin gui)
  -- ������饤�󹹿�
  showTimeline gui oauth
  -- ������饤������Υ��󥿡��Х뤴�Ȥ˹���
  timeoutAdd (showTimeline gui oauth) 30000
  -- "tweet"�ܥ���ǥĥ�����
  onClicked (tweetButton gui) (tweet gui oauth)
  -- ǧ���ѥ�����ɥ��õ�
  widgetHideAll (accessTokenGetWin gui)
  

main :: FilePath -> IO ()
main gladePath = do

  -- GTK+�����ƥ�����
  initGUI

  -- ¾�Υ���åɤ����ˤ������褦�ˤ���
  timeoutAddFull (yield >> return True)
                 priorityDefaultIdle 100

  -- .glade�ե���������
  gui <- loadGlade gladePath  -- ������ɥ���ɽ��
  

  -- x/Cansel�ܥ���ǥ�����ɥ���õ�
  onDestroy (mainWin gui) mainQuit
  onDestroy (accessTokenGetWin gui) mainQuit
  onClicked (cancelButton gui) mainQuit

  -- OAuth��Ϣ
  -- Consumer Key / Consumer Secret�ɤ߹���
  fin <- openFile "./config.ini" ReadMode
  consumerKey <- hGetLine fin
  consumerSecret <- hGetLine fin
  hClose fin
  oauth <- newOAuth consumerKey consumerSecret "" ""

  -- ���������ȡ��������
  restoreAccessToken gui oauth `catch` \_ -> authorization gui oauth

  -- �ᥤ��롼��
  mainGUI

