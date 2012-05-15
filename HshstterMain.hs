{-# LANGUAGE TupleSections, ScopedTypeVariables, DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module HshstterMain where

import OAuth
import TweetJSON
import HshstterConnectWithTwitter
import Utility
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
import qualified Data.HashTable as Hash

type IconTable = Hash.HashTable String String
type TimelineName = String
data TimelineBody = TimelineBody {body :: IORef [Tweet]}

-- Timeline�Υǡ�����
data Timeline = Timeline {
      timelineName :: !TimelineName,
      timelineField :: !DrawingArea,
      timelineWindow :: !DrawWindow,
      timelineBody :: !TimelineBody,
      sinceId :: !(IORef String)
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
authorization gui oauth = const () <$> do
  -- ǧ���ѥ�����ɥ�ɽ��
  widgetShowAll (accessTokenGetWin gui)
  -- Request Token����
  (requestToken, requestTokenSecret) <- getRequestTokenParameter oauth
  -- ǧ�ڤ��Ե�
  entrySetText (authorizationURL gui) (authorizeURL requestToken)
  onClicked (authorizationButton gui) $ flip catch handler $ getNewAccessToken gui oauth requestToken requestTokenSecret
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

selectTimeline :: TimelineSet -> TimelineName -> IO Timeline
selectTimeline timelineset tlName = head . filter ((==tlName) . timelineName) <$> readIORef timelineset

-- �����������
getIcon :: IconTable -> Tweet -> IO GUI.Icon
getIcon iconTable twt = do
  icon <- Hash.lookup iconTable (name twt)
  pixbufNewFromFile =<< case icon of {Nothing -> getAndRegisterIcon; Just ic -> return ic}
      where getAndRegisterIcon = do
              ic <- downroadIcon (profile_image_url twt)
              Hash.insert iconTable (name twt) ic
              return ic

-- ������饤����ɲ�
addTimeline :: GUI -> OAuth -> TimelineSet -> TimelineName -> MVar () -> IO ()
addTimeline gui oauth timelineset timelineName connectionLock = do
  iconTable <- Hash.new (==) Hash.hashString
  field <- drawingAreaNew -- ������饤��ɽ���ΰ�����
  widgetModifyBg field StateNormal (Color 65535 65535 65535) -- �طʤ���˥��å�
  scrolledWindowAddWithViewport (timelineWin gui) field -- ������ɥ���Ž���դ�
  drawWin <- widgetGetDrawWindow field
  (body, sinceid) <- $(mapMT 2 [|newIORef|]) ([], "1")
  -- �����襤�٥�Ȥ��ɲ�
  onExpose field $ \_ -> (const True) <$> do
    tlWidth <- (flip (-) 30) . fst <$> windowGetSize (mainWin gui)
    tlBody <- readIORef body
    tlHeight <- (\ l i m -> foldM m i l) tlBody 0 $ \i twt -> do
      icon <- getIcon iconTable twt -- �������������Pixbuf����
      GUI.drawTweet field drawWin icon (0, 0, 0) tlWidth i twt
    widgetSetSizeRequest field tlWidth tlHeight
  let tl = Timeline timelineName field drawWin (TimelineBody body) sinceid
  -- ������饤������Υ��󥿡��Х뤴�Ȥ˹���
  forkIO $ (const ()) <$> do
    (flip timeoutAdd) 30000 $ (const True) <$> do -- ������饤�󹹿�����åɵ�ư
      takeMVar connectionLock
      updateTimeline gui oauth tl
      putMVar connectionLock ()
  modifyIORef timelineset (tl:)
  widgetShowAll (timelineWin gui) -- ɽ���򹹿�

-- ������饤��򹹿�
updateTimeline :: GUI -> OAuth -> Timeline -> IO ()
updateTimeline gui oauth tl = do
  -- ������饤�󤫤�ǿ��Υĥ����Ȥ����
  sinceid <- readIORef $ sinceId tl
  tweets <- getTimelineData oauth [("since_id", sinceid)] (timelineName tl) `catch` \(e::SomeException) -> (const []) <$> putStrLn "error"
  unless (null tweets) $ writeIORef (sinceId tl) (tweet_id . head $ tweets)
  modifyIORef (body . timelineBody $ tl) (tweets++)
  -- ������
  widgetQueueDraw (timelineField tl)

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

  -- �̿�����ˤϤ����ѿ��Υ�å�����ɬ�פ����롣
  connectionLock <- newMVar ()

  -- Home Timeline���ɲ�
  addTimeline gui oauth timelineset "home_timeline" connectionLock
  hometl <- selectTimeline timelineset "home_timeline"
  updateTimeline gui oauth hometl

  -- "tweet"�ܥ���ǥĥ�����
  sendText <- newEmptyMVar
  onClicked (tweetButton gui) (tweet gui oauth sendText)
  forkIO $ forever $ do -- tweet��������åɵ�ư
    tweetText <- takeMVar sendText
    takeMVar connectionLock
    (flip catch) tweetErrorHandle (const () <$> sendTweet oauth tweetText)
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