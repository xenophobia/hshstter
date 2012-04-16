{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module HshstterMain where

import OAuth
import TweetJSON

import Data.List
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Prelude hiding (catch)
import Data.Typeable
import Control.Exception
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

-- ����ɽ����������(From: (screen_name))�ˤ���
initInformation :: GUI -> OAuth -> IO ()
initInformation gui oauth = labelSetText (information gui) ("From: " ++ (OAuth.screen_name oauth))

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
                                  `catch` \(e::SomeException) -> do
                                    -- ǧ�ڤ˼��Ԥ�����ƻ��
                                    entrySetText (pinEntry gui) ""
                                    labelSetText (hint gui) "Sorry, Failed to authorize your account. Please try again."

-- Access Token�򿷵��˼���
getNewAccessToken :: GUI -> OAuth -> Parameter -> Parameter -> IO ()
getNewAccessToken gui oauth requestToken requestTokenSecret = do
  -- PIN���� -> oauth_verifier�ѥ�᡼���Ȥ���«��
  verifier <- ("oauth_varifier",) <$> entryGetText (pinEntry gui)
  -- Access Token����
  accessTokenParameters <- parseParameter <$> oauthRequest oauth accessTokenURL (snd requestTokenSecret) [requestToken, verifier]
  accessToken <- getParameter accessTokenParameters "oauth_token"
  accessTokenSecret <- getParameter accessTokenParameters "oauth_token_secret"
  my_user_id <- getParameter accessTokenParameters "user_id"
  my_screen_name <- getParameter accessTokenParameters "screen_name"
  accessTokenData@(accessToken:accessTokenSecret:my_user_id:my_screen_name:[])
      <- mapM (fmap snd . getParameter accessTokenParameters) ["oauth_token", "oauth_token_secret", "user_id", "screen_name"]
  -- Access Token�ݻ��ե�����access.ini��Access Token�ڤӥ桼������򥻡���
  runResourceT $ CL.sourceList ((:[]) . intercalate "\n" $ accessTokenData) $= CL.map pack $$ CB.sinkFile "access.ini"
  -- Access Token�����ꤷ��OAuth������ˡ��ᥤ��롼�����Ƥ�
  mainRoutine gui $ OAuth (consumerKey oauth) (consumerSecret oauth) accessToken accessTokenSecret my_user_id my_screen_name

-- Access Token���ɤ߹���
restoreAccessToken :: GUI -> OAuth -> IO ()
restoreAccessToken gui oauth = do
  -- Access Token�ɤ߹���
  (accessToken:accessTokenSecret:my_user_id:my_screen_name:[]) <- runResourceT $ CB.sourceFile "./access.ini" $= CB.lines $= CL.map unpack $$ CL.take 4
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
  res <- apiRequest curl oauth "home_timeline" GET [] `catch` \(e::SomeException) -> return "error"
  tweets <- getTimeline res `catch` \(e::SomeException) -> return []
  unless (null tweets) $ do
    buffer <- textViewGetBuffer (timeline gui)
    let tl = foldl (\s -> \t -> s ++ (show t) ++ "\n") "" tweets
    textBufferSetText buffer tl
  reset curl
  return True

data TweetErrorType = EmptyTweet | CharactorExceeded | APIError deriving (Show, Typeable)
data TweetError = TweetError TweetErrorType deriving (Show, Typeable)
instance Exception TweetError

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
            then throw (TweetError EmptyTweet)
            else if lengthOfTweet > 140 then throw (TweetError CharactorExceeded)
            else do
              apiRequest curlTweet oauth "update" POST [("status", encodeString tweetText)] `catch` \(_::SomeException) -> throw (TweetError APIError)
              -- �ĥ�������������ꥻ�å�
              entrySetText (tweetEntry gui) ""
              -- �����٥�����
              initInformation gui oauth
        tweetErrorHandle (TweetError err) =
            let errMessage = case err of
                               EmptyTweet -> "Please input some messages."
                               CharactorExceeded -> "You cannot transmit a tweet exceeding 140 characters."
                               APIError -> "API problem, please try again."
            in
              labelSetText (information gui) $ "Error: " ++ errMessage


-- �ᥤ�󥦥���ɥ�ɽ����������饤��ɽ�������������޺�ư���ĥ����ȥܥ���˥ϥ�ɥ������ʡ�ǧ���ѥ�����ɥ��õ��
mainRoutine :: GUI -> OAuth -> IO ()
mainRoutine gui oauth = do
  -- �����٥�����
  initInformation gui oauth
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

  -- Consumer Key / Consumer Secret�ɤ߹���
  (consumerKey:consumerSecret:[]) <- runResourceT $ CB.sourceFile "./config.ini" $= CB.lines $= CL.map unpack $$ CL.take 2
  let oauth = OAuth consumerKey consumerSecret "" "" "" ""

  -- Access Token����������ᥤ�󥦥���ɥ���ɽ��
  restoreAccessToken gui oauth `catch` \(e::SomeException) -> authorization gui oauth

  -- �ᥤ��롼��
  mainGUI

