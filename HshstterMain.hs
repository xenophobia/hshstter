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
      authorizationField :: !Fixed
    }

-- Access Token�������Ƥ��ʤ��ä���硢OAuthǧ�ڤ�桼���˹ԤʤäƤ�餦
authorization :: GUI -> OAuth -> IO ()
authorization gui oauth = const () <$> do
  -- ǧ���ѥ�����ɥ�ɽ��
  widgetShowAll (accessTokenGetWin gui)
  -- Request Token����
  (requestToken, requestTokenSecret) <- getRequestTokenParameter oauth
  -- ǧ�ڤ��Ե�
  authorizationLink <- linkButtonNewWithLabel (authorizeURL requestToken) "Authorization Page"
  fixedPut (authorizationField gui) authorizationLink (150, 70)
  widgetShowAll $ authorizationField gui
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

-- ���������Pixbuf�����
getIconImage :: IconTable -> Tweet -> IO GUI.Icon
getIconImage iconTable twt = do
  icon <- Hash.lookup iconTable (profile_image_url twt)
  pixbufNewFromFile =<< case icon of {Nothing -> getAndRegisterIcon; Just ic -> return ic}
      where getAndRegisterIcon = do
              ic <- getIcon (profile_image_url twt)
              Hash.insert iconTable (profile_image_url twt) ic
              return ic

-- ������饤����ɲ�
addTimeline :: GUI -> OAuth -> TimelineSet -> MVar (String, [Parameter]) -> MVar String -> MVar String -> TimelineName -> MVar () -> IO ()
addTimeline gui oauth timelineset sendText retweetTweet favoriteTweet timelineName connectionLock = do
  iconTable <- Hash.new (==) Hash.hashString
  field <- drawingAreaNew -- ������饤��ɽ���ΰ�����
  widgetModifyBg field StateNormal (Color 65535 65535 65535) -- �طʤ���˥��å�
  scrolledWindowAddWithViewport (timelineWin gui) field -- ������ɥ���Ž���դ�
  drawWin <- widgetGetDrawWindow field
  (body, sinceid) <- $(mapMT 2 [|newIORef|]) ([], "1")

  -- ���٥�ȸ��Ρ�RT/favorite�������᥽�åɤ򥻥å�
  widgetAddEvents field [ButtonPressMask]
  field `on` buttonPressEvent $ tryEvent $ const () <$> do -- �ĥ����Ȥ�����å����줿
    n <- (`div` GUI.tweetAreaHeight) . floor . snd <$> eventCoordinates
    button <- eventButton
    tweets <- liftIO $ readIORef body
    when (button == RightButton) $ liftIO $ do
      let thisTweet = tweets!!n
          thisTweetId = tweet_id thisTweet
          thisTweetUserId = TweetJSON.screen_name thisTweet
      dialogWidth <- fst <$> windowGetSize (mainWin gui)
      dialog <- dialogNew
      set dialog [windowTitle := "About this tweet", windowDefaultWidth := dialogWidth, windowDefaultHeight := dialogHeight]
      tweetArea <- drawingAreaNew
      widgetSetSizeRequest tweetArea dialogWidth dialogHeight
      widgetModifyBg tweetArea StateNormal (Color 65535 65535 65535) -- �طʤ���˥��å�
      (flip containerAdd) tweetArea =<< dialogGetUpper dialog
      retweetButton <- dialogAddButton dialog "Retweet" (ResponseUser idRetweet)
      favoriteButton <- dialogAddButton dialog "Favorite" (ResponseUser idFavorite)
      replyButton <- dialogAddButton dialog "Reply" (ResponseUser idReply)
      cancel <- dialogAddButton dialog "Cancel" ResponseCancel
      widgetShowAll dialog
      tweetAreaDrawWin <- widgetGetDrawWindow tweetArea
      onExpose tweetArea $ \_ -> (const True) <$> do
        icon <- getIconImage iconTable thisTweet
        GUI.drawTweet tweetArea tweetAreaDrawWin icon (0, 0, 0) dialogWidth 0 thisTweet
      widgetShowAll dialog
      responseDialog <- dialogRun dialog
      case responseDialog of
        ResponseUser identifer | identifer == idRetweet -> putMVar retweetTweet thisTweetId >> widgetDestroy dialog
                               | identifer == idFavorite -> putMVar favoriteTweet thisTweetId >> widgetDestroy dialog
                               | identifer == idReply -> widgetDestroy dialog >> replyTo gui sendText thisTweetUserId thisTweetId
        ResponseCancel -> widgetDestroy dialog
        ResponseDeleteEvent -> widgetDestroy dialog

  -- �����襤�٥�Ȥ��ɲ�
  onExpose field $ \_ -> (const True) <$> do
    tlWidth <- (flip (-) 30) . fst <$> windowGetSize (mainWin gui)
    tlBody <- readIORef body
    tlHeight <- (\ l i m -> foldM m i l) tlBody 0 $ \i twt -> do
      icon <- getIconImage iconTable twt -- �������������Pixbuf����
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
      where
        dialogHeight = 70
        idRetweet = 0
        idFavorite = 1
        idReply = 2

-- ������饤��򹹿�
updateTimeline :: GUI -> OAuth -> Timeline -> IO ()
updateTimeline gui oauth tl = do
  -- ������饤�󤫤�ǿ��Υĥ����Ȥ����
  sinceid <- readIORef $ sinceId tl
  tweets <- getTimelineData oauth [("since_id", sinceid), ("count", "200")] (timelineName tl) `catch` \(e::SomeException) -> (const []) <$> putStrLn "Timeline Update Error."
  unless (null tweets) $ writeIORef (sinceId tl) (tweet_id . head $ tweets)
  modifyIORef (body . timelineBody $ tl) (tweets++)
  -- ������
  widgetQueueDraw (timelineField tl)

-- tweet����
tweet :: GUI -> OAuth -> MVar (String, [Parameter]) -> IO ()
tweet gui oauth sendText = do
  -- tweet�ܥ����̵����
  widgetSetSensitivity (tweetButton gui) False
  tweetText <- entryGetText (tweetEntry gui)
  -- tweet����
  putMVar sendText (tweetText, [])
  entrySetText (tweetEntry gui) ""
  labelSetText (information gui) ("From: " ++ (OAuth.screen_name oauth))
  -- tweet�ܥ����ͭ����
  widgetSetSensitivity (tweetButton gui) True

replyTo :: GUI -> MVar (String, [Parameter]) -> String -> ID -> IO ()
replyTo gui sendText username replyToId = do
  replyWindow <- dialogNew
  set replyWindow [windowTitle := "Reply To", windowDefaultWidth := replyWindowWidth , windowDefaultHeight := replyWindowHeight]
  replyToLabel <- labelNew Nothing
  labelSetMarkup replyToLabel ("<b>" ++ ("@" ++ username) ++ "</b>")
  set replyToLabel [miscXalign := 0]
  replyEntry <- textViewNew
  replyWindowUpper <- dialogGetUpper replyWindow
  boxPackStart replyWindowUpper replyToLabel PackNatural 0
  boxPackStart replyWindowUpper replyEntry PackGrow 0
  replyButton <- dialogAddButton replyWindow "Send" (ResponseYes)
  cancel <- dialogAddButton replyWindow "Cancel" ResponseCancel
  widgetShowAll replyWindow
  responseDialog <- dialogRun replyWindow
  case responseDialog of
    ResponseYes -> do
      replyText <- GUI.textViewGetText replyEntry True
      -- reply����
      putMVar sendText ("@" ++ username ++ " " ++ replyText, [("in_reply_to_status_id", replyToId)])
    ResponseCancel -> return ()
    ResponseDeleteEvent -> return ()
  widgetDestroy replyWindow
    where
      replyWindowHeight = 300
      replyWindowWidth = 400

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
  sendText <- newEmptyMVar
  retweetTweet <- newEmptyMVar
  favoriteTweet <- newEmptyMVar
  addTimeline gui oauth timelineset sendText retweetTweet favoriteTweet "home_timeline" connectionLock
  hometl <- selectTimeline timelineset "home_timeline"
  updateTimeline gui oauth hometl

  -- "tweet"�ܥ���ǥĥ�����
  onClicked (tweetButton gui) (tweet gui oauth sendText)
  forkIO $ forever $ do -- tweet��������åɵ�ư
    (tweetText, params) <- takeMVar sendText
    takeMVar connectionLock
    (flip catch) tweetErrorHandle $ sendTweet oauth tweetText params
    putMVar connectionLock ()

  -- ��ĥ�����
  forkIO $ forever $ do -- retweet��������åɵ�ư
    retweetID <- takeMVar retweetTweet
    takeMVar connectionLock
    (flip catch) tweetErrorHandle $ retweet oauth retweetID
    putMVar connectionLock ()

  -- �դ���
  forkIO $ forever $ do -- �դ��ܥ���åɵ�ư
    favoriteID <- takeMVar favoriteTweet
    takeMVar connectionLock
    (flip catch) tweetErrorHandle $ favorite oauth favoriteID
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