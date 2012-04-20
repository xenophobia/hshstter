{-# LANGUAGE TupleSections, ScopedTypeVariables, DeriveDataTypeable #-}
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
import Control.Arrow
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
      timelineHeight :: !(IORef Int),
      timelineField :: !DrawingArea,
      timelineWindow :: !DrawWindow,
      timelineBody :: !TimelineBody,
      position :: !GUI.Coordinate
}

type TimelineSet = IORef [Timeline]

-- GUI�ǡ�����
data GUI = GUI {
      mainWin :: !Window,
      information :: !Label,
      tweetEntry :: !Entry,
      tweetButton :: !Button,
      timelineWin :: !ScrolledWindow,
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
  -- Request Token����
  (requestToken, requestTokenSecret) <- getRequestTokenParameter oauth
  -- ǧ�ڤ��Ե�
  entrySetText (authorizationURL gui) (authorizeURL requestToken)
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

-- .glade�ե������XML�����ˤ���GUI���������åȥ��ɡ�GUI�����Ѵ�
loadGUI :: FilePath -> IO GUI
loadGUI = $(castToGUI ''GUI) <=< xmlNewIO
    where xmlNewIO gladePath = xmlNew gladePath >>= \maybeXML -> case maybeXML of {Just xml -> return xml; Nothing -> fail "XML format error."}

selectTimeline :: TimelineName -> TimelineSet -> IO Timeline
selectTimeline tlName timelineset = head . filter ((==tlName) . timelineName) <$> readIORef timelineset

-- ������饤����ɲ�
addTimeline :: GUI -> TimelineSet -> TimelineName -> IO ()
addTimeline gui timelineset timelineName = do
  let pos = (0, 0)
  field <- drawingAreaNew -- ������饤��ɽ���ΰ�����
  widgetModifyBg field StateNormal (Color 65535 65535 65535) -- �طʤ���˥��å�
  scrolledWindowAddWithViewport (timelineWin gui) field -- ������ɥ���Ž���դ�
  drawWin <- widgetGetDrawWindow field
  (body, height) <- $(mapMT 2 [|newIORef|]) ("", 0)
  -- �����襤�٥�Ȥ��ɲ�
  onExpose field $ \_ -> do
    tlWidth <- fst . first (flip (-) 30) <$> windowGetSize (mainWin gui)
    (tlBody, tlHeight) <- $(mapMT 2 [|readIORef|]) (body, height)
    GUI.drawString field drawWin (0, 0, 0) pos tlWidth tlBody
    widgetSetSizeRequest field tlWidth tlHeight
    return True
  modifyIORef timelineset (Timeline timelineName height field drawWin (TimelineBody body) pos:)
  widgetShowAll (timelineWin gui) -- ɽ���򹹿�

-- ������饤��򹹿�
updateTimeline :: GUI -> OAuth -> Timeline -> IO ()
updateTimeline gui oauth tl =
  flip catch getTimelineErrorHandle $ do
    -- ������饤�󤫤�ǿ��Υĥ����Ȥ����
    tweets <- getTimelineData oauth (timelineName tl) `catch` \(e::SomeException) -> putStrLn "error" >> return []
    let (x, y) = position tl
        tlText = foldl (\s -> \t -> s ++ (show t) ++ "\n") "" tweets
    writeIORef (body . timelineBody $ tl) tlText
    writeIORef (timelineHeight $ tl) ((*25) . length . lines $ tlText)
    -- ������
    widgetQueueDraw (timelineField tl)
      where -- ���顼�ϥ�ɥ��ñ��̵���
        getTimelineErrorHandle = \(e::SomeException) -> return ()

-- tweet����
tweet :: GUI -> OAuth -> MVar String -> IO ()
tweet gui oauth sendText = do
  -- tweet�ܥ����̵����
  widgetSetSensitivity (tweetButton gui) False
  tweetText <- entryGetText (tweetEntry gui)
  -- tweet����
  putMVar sendText tweetText
  entrySetText (tweetEntry gui) ""
  labelSetText (information gui) ("From: " ++ (OAuth.screen_name oauth))
  -- tweet�ܥ����ͭ����
  widgetSetSensitivity (tweetButton gui) True

-- �ᥤ�󥦥���ɥ�ɽ����������饤��ɽ�������������޺�ư���ĥ����ȥܥ���˥ϥ�ɥ������ʡ�ǧ���ѥ�����ɥ��õ��
mainRoutine :: GUI -> OAuth -> IO ()
mainRoutine gui oauth = do
  timelineset <- newIORef []
  labelSetText (information gui) ("From: " ++ (OAuth.screen_name oauth))
  -- �ᥤ�󥦥���ɥ�ɽ��
  widgetShowAll (mainWin gui)
  -- Home Timeline���ɲ�
  addTimeline gui timelineset "home_timeline"
  hometl <- selectTimeline "home_timeline" timelineset
  -- �̿�����ˤϤ����ѿ��Υ�å�����ɬ�פ����롣
  connectionLock <- newMVar ()
  updateTimeline gui oauth hometl
  -- ������饤������Υ��󥿡��Х뤴�Ȥ˹���
  forkIO $ do
    (flip timeoutAdd) 3000 $ do
      takeMVar connectionLock
      updateTimeline gui oauth hometl
      putMVar connectionLock ()
      return True
    return ()
  -- "tweet"�ܥ���ǥĥ�����
  sendText <- newEmptyMVar
  onClicked (tweetButton gui) (tweet gui oauth sendText)
  forkIO $ forever $ do
    tweetText <- takeMVar sendText
    takeMVar connectionLock
    (flip catch) tweetErrorHandle (sendTweet oauth tweetText >> return ())
    putMVar connectionLock ()
  -- ǧ���ѥ�����ɥ��õ�
  widgetHideAll (accessTokenGetWin gui)
    where -- ���顼�ϥ�ɥ�
      tweetErrorHandle (TweetError err) = labelSetText (information gui) $ "Error: " ++ errMessage err
      errMessage EmptyTweet = "Please input some messages."
      errMessage CharactorExceeded = "You cannot transmit a tweet exceeding 140 characters."
      errMessage APIError = "API problem, please try again."

main :: FilePath -> IO ()
main gladePath = do
  -- GTK+�����ƥ�����
  initGUI
  -- ¾�Υ���åɤ����ˤ������褦�ˤ���
  timeoutAddFull (Control.Concurrent.yield >> return True) priorityDefaultIdle 100
  -- .glade�ե����뤫��GUI���������åȷ������
  gui <- loadGUI gladePath
  -- x/Cansel�ܥ���ǥ�����ɥ���õ�
  onDestroy (mainWin gui) mainQuit
  onDestroy (accessTokenGetWin gui) mainQuit
  onClicked (cancelButton gui) mainQuit
  -- Consumer Key / Consumer Secret�ɤ߹���
  [consumerKey, consumerSecret] <- runResourceT $ CB.sourceFile "./config.ini" $= CB.lines $= CL.map unpack $$ CL.take 2
  oauth <- newOAuth consumerKey consumerSecret "" "" "" ""
  -- Access Token����������ᥤ�󥦥���ɥ���ɽ��
  restoreAccessToken gui oauth `catch` \(e::SomeException) -> authorization gui oauth
  -- �ᥤ��롼�׵�ư
  mainGUI