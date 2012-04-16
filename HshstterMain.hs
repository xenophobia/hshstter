{-# LANGUAGE TupleSections #-}

module HshstterMain where

import OAuth
import TweetJSON

import Debug.Trace

import Network.Curl
import Network.HTTP
import System.IO
import Control.Monad
import Control.Applicative
import Codec.Binary.UTF8.String (decodeString, encodeString)

import Graphics.UI.Gtk hiding (add)
import Graphics.UI.Gtk.Glade
import Control.Concurrent

-- GUI�ǡ�����
data GUI = GUI {
      mainWin :: !Window,
      information :: !Label,
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

-- Access Token�������Ƥ��ʤ��ä���硢OAuthǧ�ڤ�桼���˹ԤʤäƤ�餦
authorization :: GUI -> OAuth -> IO ()
authorization gui oauth = do
  -- ǧ���ѥ�����ɥ�ɽ��
  widgetShowAll (accessTokenGetWin gui)
  -- �ꥯ�����ȥȡ��������
  requestTokenParameters <- parseParameter <$> oauthRequest oauth requestTokenURL "" []
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

-- Access Token�򿷵��˼���
getNewAccessToken :: GUI -> OAuth -> Parameter -> Parameter -> IO ()
getNewAccessToken gui oauth requestToken requestTokenSecret = do
  -- PIN���� -> oauth_verifier�ѥ�᡼���Ȥ���«��
  verifier <- ("oauth_varifier",) <$> entryGetText (pinEntry gui)
  -- Access Token����
  accessTokenParameters <- parseParameter . (\s -> trace s s) <$> oauthRequest oauth accessTokenURL (snd requestTokenSecret) [requestToken, verifier]
  accessToken <- getParameter accessTokenParameters "oauth_token"
  accessTokenSecret <- getParameter accessTokenParameters "oauth_token_secret"
  my_user_id <- getParameter accessTokenParameters "user_id"
  my_screen_name <- getParameter accessTokenParameters "screen_name"
  -- Access Token�ݻ��ե�����access.ini��Access Token�ڤӥ桼������򥻡���
  fout <- openFile "./access.ini" WriteMode
  hPutStrLn fout (snd accessToken)
  hPutStrLn fout (snd accessTokenSecret)
  hPutStrLn fout (snd my_user_id)
  hPutStrLn fout (snd my_screen_name)
  hClose fout
  -- Access Token�����ꤷ��OAuth������ˡ��ᥤ��롼�����Ƥ�
  mainRoutine gui $ OAuth (consumerKey oauth) (consumerSecret oauth) (snd accessToken) (snd accessTokenSecret) (snd my_user_id) (snd my_screen_name)

-- Access Token���ɤ߹���
restoreAccessToken :: GUI -> OAuth -> IO ()
restoreAccessToken gui oauth = do
  -- Access Token�ɤ߹���
  fin <- openFile "./access.ini" ReadMode
  accessToken <- hGetLine fin
  accessTokenSecret <- hGetLine fin
  my_user_id <- hGetLine fin
  my_screen_name <- hGetLine fin
  hClose fin
  -- Access Token�����ꤷ��OAuth������ˡ��ᥤ��롼�����Ƥ�
  mainRoutine gui $ OAuth (consumerKey oauth) (consumerSecret oauth) accessToken accessTokenSecret my_user_id my_screen_name

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

  guiMainWin <- xmlGetWidget xml castToWindow "mainWin"  -- �ᥤ�󥦥���ɥ������
  guiInformation <- xmlGetWidget xml castToLabel "information"  -- ����
  guiTweetEntry <- xmlGetWidget xml castToEntry "tweetEntry"  -- �ĥ����������������
  guiTweetButton <- xmlGetWidget xml castToButton "tweetButton"  -- �ĥ����ȥܥ�������
  guiTimeline <- xmlGetWidget xml castToTextView "timeline"  -- ������饤��ɽ���������
  guiTimelineWindow <- xmlGetWidget xml castToScrolledWindow "timelineWindow"  -- ��������С������
  guiAccessTokenWin <- xmlGetWidget xml castToWindow "accessTokenGetWin"  -- Access Token����������ɥ������
  guiHint <- xmlGetWidget xml castToLabel "hint"  -- ǧ���ѥ�å����������
  guiAuthorizationButton <- xmlGetWidget xml castToButton "authorizationButton"  -- Authorization�ܥ�������
  guiCancelButton <- xmlGetWidget xml castToButton "cancelButton"  -- Cancel�ܥ�������
  guiPinEntry <- xmlGetWidget xml castToEntry "pinEntry"  -- PIN�����������
  guiAuthorizationURL <- xmlGetWidget xml castToEntry "authorizationURL"  -- ǧ����URLɽ��

  return $ GUI guiMainWin guiInformation guiTweetEntry guiTweetButton guiTimeline guiTimelineWindow
                      guiAccessTokenWin guiHint guiAuthorizationButton guiCancelButton guiPinEntry guiAuthorizationURL

-- ������饤��򹹿�
updateTimeline :: GUI -> OAuth -> Curl -> IO Bool
updateTimeline gui oauth curl = do
  -- ������饤�󤫤�ǿ��Υĥ����Ȥ����
  res <- apiRequest curl oauth "home_timeline" GET [] `catch` \_ -> return "error"
  tweets <- getTimeline res `catch` \_ -> return []
  unless (null tweets) $ do
    buffer <- textViewGetBuffer (timeline gui)
    let tl = foldl (\s -> \t -> s ++ (show t) ++ "\n") "" tweets
    textBufferSetText buffer tl
  reset curl
  return True

-- �ĥ����Ȥ���
tweet :: GUI -> OAuth -> Curl -> Curl -> IO ()
tweet gui oauth curlTweet curlTimeline = do
  -- tweet �ܥ����̵����
  widgetSetSensitivity (tweetButton gui) False
  -- �ĥ���������
  catch sendTweet tweetErrorHandle
  -- ������饤��򹹿�
  updateTimeline gui oauth curlTimeline
  reset curlTweet
  -- tweet �ܥ����ͭ����
  widgetSetSensitivity (tweetButton gui) True
      where
        sendTweet = do -- �ĥ�����
          tweetText <- entryGetText (tweetEntry gui)
          let lengthOfTweet = length tweetText
          if lengthOfTweet == 0
            then fail "Please input some messages."
            else if lengthOfTweet > 140 then fail "You cannot transmit a tweet exceeding 140 characters."
            else do
              apiRequest curlTweet oauth "update" POST [("status", encodeString tweetText)]
              -- �ĥ�������������ꥻ�å�
              entrySetText (tweetEntry gui) ""
              -- �����٥�����
              labelSetText (information gui) ("From: " ++ (OAuth.screen_name oauth))
        tweetErrorHandle err = case show err of
                                 "user error (Please input some messages.)" ->
                                     labelSetText (information gui) ("Error: Please input some messages.")
                                 "user error (You cannot transmit a tweet exceeding 140 characters.)" ->
                                     labelSetText (information gui) ("Error: You cannot transmit a tweet exceeding 140 characters.")
                                 _ -> labelSetText (information gui) ("Error: Please try again.")


-- �ᥤ�󥦥���ɥ�ɽ����������饤��ɽ�������������޺�ư���ĥ����ȥܥ���˥ϥ�ɥ������ʡ�ǧ���ѥ�����ɥ��õ��
mainRoutine :: GUI -> OAuth -> IO ()
mainRoutine gui oauth = do
  -- �����٥�����
  labelSetText (information gui) ("From: " ++ (OAuth.screen_name oauth))

  -- �ᥤ�󥦥���ɥ�ɽ��
  widgetShowAll (mainWin gui)
  -- ������饤�󹹿�
  curlTimeline <- initialize
  updateTimeline gui oauth curlTimeline
  -- ������饤������Υ��󥿡��Х뤴�Ȥ˹���
  timeoutAdd (updateTimeline gui oauth curlTimeline) 30000
  -- "tweet"�ܥ���ǥĥ�����
  curlTweet <- initialize
  onClicked (tweetButton gui) (tweet gui oauth curlTweet curlTimeline)
  -- ǧ���ѥ�����ɥ��õ�
  widgetHideAll (accessTokenGetWin gui)

main :: FilePath -> IO ()
main gladePath = do
  -- GTK+�����ƥ�����
  initGUI
  -- ¾�Υ���åɤ����ˤ������褦�ˤ���
  timeoutAddFull (yield >> return True) priorityDefaultIdle 100
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
  let oauth = OAuth consumerKey consumerSecret "" "" "" ""

  -- Access Token����������ᥤ�󥦥���ɥ���ɽ��
  restoreAccessToken gui oauth `catch` \_ -> authorization gui oauth

  -- �ᥤ��롼��
  mainGUI

