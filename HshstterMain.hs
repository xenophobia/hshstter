{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module HshstterMain where

import OAuth
import TweetJSON
import HshstterConnectWithTwitter
import THUtility
import qualified GUILibrary as GUI

import Data.List
import Data.Maybe
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Prelude hiding (catch)
import Data.Typeable
import Data.IORef
import Control.Exception
import Control.Monad
import Control.Applicative
import Codec.Binary.UTF8.String (decodeString, encodeString)
import Graphics.UI.Gtk hiding (add)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Glade
import Control.Concurrent

type TimelineName = String
data TimelineBody = TimelineBody {body :: IORef String}

-- Timeline�Υǡ�����
data Timeline = Timeline {
      timelineName :: !TimelineName,
      timelineWidth :: !(IORef Int),
      timelineHeight :: !(IORef Int),
      timelineField :: !DrawingArea,
      timelineWindow :: !DrawWindow,
      timelineBody :: !TimelineBody,
      position :: !GUI.Coordinate
}

-- GUI�ǡ�����
data GUI = GUI {
      mainWin :: !Window,
      information :: !Label,
      tweetEntry :: !Entry,
      tweetButton :: !Button,
      timelineWin :: !ScrolledWindow,
      timeline :: !(IORef [Timeline]),
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
  -- Request Token����
  (requestToken, requestTokenSecret) <- getRequestTokenParameter oauth
  -- ǧ�ڥڡ�����URL��ɽ��
  entrySetText (authorizationURL gui) (authorizeURL requestToken)
  -- ǧ���ѥܥ���򥯥�å���ǧ�ڤ��ߤ�
  onClicked (authorizationButton gui) $ flip catch handler $ getNewAccessToken gui oauth requestToken requestTokenSecret
  return ()
      where -- ���顼�ϥ�ɥ顧ǧ�ڤ˼��Ԥ�����ƻ��
        handler = \(e::SomeException) -> entrySetText (pinEntry gui) "" >> labelSetText (hint gui) "Sorry, Failed to authorize your account. Please try again."

-- Access Token�򿷵��˼���
getNewAccessToken :: GUI -> OAuth -> Parameter -> Parameter -> IO ()
getNewAccessToken gui oauth requestToken requestTokenSecret = do
  -- PIN���� -> oauth_verifier�ѥ�᡼���Ȥ���«��
  verifier <- ("oauth_varifier",) <$> entryGetText (pinEntry gui)
  -- Access Token����
  accessTokenData@[accessToken, accessTokenSecret, my_user_id, my_screen_name] <- getAccessTokenString oauth (snd requestTokenSecret) [requestToken, verifier]
  -- Access Token�ݻ��ե�����access.ini��Access Token�ڤӥ桼������򥻡���
  runResourceT $ CL.sourceList accessTokenData $= CL.map (++"\n") $= CL.map pack $$ CB.sinkFile "access.ini"
  -- Access Token�����ꤷ��OAuth������ˡ��ᥤ��롼�����Ƥ�
  mainRoutine gui =<< newOAuth (consumerKey oauth) (consumerSecret oauth) accessToken accessTokenSecret my_user_id my_screen_name

-- Access Token���ɤ߹���
restoreAccessToken :: GUI -> OAuth -> IO ()
restoreAccessToken gui oauth = do
  -- Access Token�ɤ߹���
  [accessToken, accessTokenSecret, my_user_id, my_screen_name] <- runResourceT $ CB.sourceFile "./access.ini" $= CB.lines $= CL.map unpack $$ CL.take 4
  -- Access Token�����ꤷ��OAuth������ˡ��ᥤ��롼�����Ƥ�
  mainRoutine gui =<< newOAuth (consumerKey oauth) (consumerSecret oauth) accessToken accessTokenSecret my_user_id my_screen_name

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

  -- ������饤��ɽ���������
  guiTimelineWin <- xmlGetWidget xml castToScrolledWindow "timelineWin"
  guiTimeline <- newIORef []

  guiAccessTokenGetWin <- xmlGetWidget xml castToWindow "accessTokenGetWin"  -- Access Token����������ɥ������
  guiHint <- xmlGetWidget xml castToLabel "hint"  -- ǧ���ѥ�å����������
  guiAuthorizationButton <- xmlGetWidget xml castToButton "authorizationButton"  -- Authorization�ܥ�������
  guiCancelButton <- xmlGetWidget xml castToButton "cancelButton"  -- Cancel�ܥ�������
  guiPinEntry <- xmlGetWidget xml castToEntry "pinEntry"  -- PIN�����������
  guiAuthorizationURL <- xmlGetWidget xml castToEntry "authorizationURL"  -- ǧ����URLɽ��

  return $ GUI guiMainWin guiInformation guiTweetEntry guiTweetButton guiTimelineWin guiTimeline
                      guiAccessTokenGetWin guiHint guiAuthorizationButton guiCancelButton guiPinEntry guiAuthorizationURL

selectTimeline :: TimelineName -> [Timeline] -> Timeline
selectTimeline tlName = head . filter ((==tlName) . timelineName)

-- ������饤����ɲ�
addTimeline :: GUI -> TimelineName -> IO ()
addTimeline gui timelineName = do
  let pos = (0, 0)
  field <- drawingAreaNew -- ������饤��ɽ���ΰ�����
  widgetModifyBg field StateNormal (Color 65535 65535 65535) -- �طʤ���˥��å�
  scrolledWindowAddWithViewport (timelineWin gui) field -- ������ɥ���Ž���դ�
  drawWin <- widgetGetDrawWindow field
  (body, width, height) <- $(mapMT 3 [|newIORef|]) ("", 300, 0)
  -- �����襤�٥�Ȥ��ɲ�
  onExpose field $ \_ -> do
    (tlBody, tlWidth, tlHeight) <- $(mapMT 3 [|readIORef|]) (body, width, height)
    GUI.drawString field drawWin (0, 0, 0) pos tlWidth tlBody
    widgetSetSizeRequest field tlWidth tlHeight
    return True
  modifyIORef (timeline gui) (Timeline timelineName width height field drawWin (TimelineBody body) pos:)
  widgetShowAll (timelineWin gui) -- ɽ���򹹿�

-- ������饤��򹹿�
updateTimeline :: GUI -> OAuth -> Timeline -> IO Bool
updateTimeline gui oauth tl = do
  flip catch getTimelineErrorHandle $ do
    -- ������饤�󤫤�ǿ��Υĥ����Ȥ����
    tweets <- getTimelineData oauth (timelineName tl) `catch` \(e::SomeException) -> putStrLn "error" >> return []
    let (x, y) = position tl
        tlText = foldl (\s -> \t -> s ++ (show t) ++ "\n") "" tweets
    writeIORef (body . timelineBody $ tl) tlText
    writeIORef (timelineHeight $ tl) ((*25) . length . lines $ tlText)
    -- ������
    (width, height) <- $(mapMT 2 [|readIORef|]) (timelineWidth tl, timelineHeight tl)
    drawWindowClearAreaExpose (timelineWindow tl) x y width height
  return True
      where -- ���顼�ϥ�ɥ��ñ��̵���
        getTimelineErrorHandle = \(e::SomeException) -> return ()

-- tweet����
tweet :: GUI -> OAuth -> IO ()
tweet gui oauth = do
  -- tweet�ܥ����̵����
  widgetSetSensitivity (tweetButton gui) False
  -- tweet entry����tweet���Ƥ���Ф�
  tweetText <- entryGetText (tweetEntry gui)
  flip catch tweetErrorHandle $ do
    -- tweet����
    sendTweet oauth tweetText
    -- tweet��������ꥻ�å�
    entrySetText (tweetEntry gui) ""
    -- �����٥�����
    initInformation gui oauth
  -- home timeline�򹹿�
  updateTimeline gui oauth . selectTimeline "home_timeline" =<< readIORef (timeline gui)
  -- tweet�ܥ����ͭ����
  widgetSetSensitivity (tweetButton gui) True
      where
        -- ���顼�ϥ�ɥ�
        tweetErrorHandle (TweetError err) = labelSetText (information gui) $ "Error: " ++ errMessage err
        errMessage EmptyTweet = "Please input some messages."
        errMessage CharactorExceeded = "You cannot transmit a tweet exceeding 140 characters."
        errMessage APIError = "API problem, please try again."

-- �ᥤ�󥦥���ɥ�ɽ����������饤��ɽ�������������޺�ư���ĥ����ȥܥ���˥ϥ�ɥ������ʡ�ǧ���ѥ�����ɥ��õ��
mainRoutine :: GUI -> OAuth -> IO ()
mainRoutine gui oauth = do
  -- �����٥�����
  initInformation gui oauth
  -- �ᥤ�󥦥���ɥ�ɽ��
  widgetShowAll (mainWin gui)
  -- Home Timeline���ɲ�
  addTimeline gui "home_timeline"
  hometl <- selectTimeline "home_timeline" <$> readIORef (timeline gui)
  -- ������饤�󹹿�
  updateTimeline gui oauth hometl
  -- ������饤������Υ��󥿡��Х뤴�Ȥ˹���
  timeoutAdd (updateTimeline gui oauth hometl) 30000
  -- "tweet"�ܥ���ǥĥ�����
  onClicked (tweetButton gui) (tweet gui oauth)
  -- ǧ���ѥ�����ɥ��õ�
  widgetHideAll (accessTokenGetWin gui)

main :: FilePath -> IO ()
main gladePath = do
  -- GTK+�����ƥ�����
  initGUI
  -- ¾�Υ���åɤ����ˤ������褦�ˤ���
  timeoutAddFull (Control.Concurrent.yield >> return True) priorityDefaultIdle 100
  -- .glade�ե���������
  gui <- loadGlade gladePath
  -- x/Cansel�ܥ���ǥ�����ɥ���õ�
  onDestroy (mainWin gui) mainQuit
  onDestroy (accessTokenGetWin gui) mainQuit
  onClicked (cancelButton gui) mainQuit
  -- Consumer Key / Consumer Secret�ɤ߹���
  (consumerKey:consumerSecret:[]) <- runResourceT $ CB.sourceFile "./config.ini" $= CB.lines $= CL.map unpack $$ CL.take 2
  oauth <- newOAuth consumerKey consumerSecret "" "" "" ""
  -- Access Token����������ᥤ�󥦥���ɥ���ɽ��
  restoreAccessToken gui oauth `catch` \(e::SomeException) -> authorization gui oauth
  -- �ᥤ��롼�׵�ư
  mainGUI