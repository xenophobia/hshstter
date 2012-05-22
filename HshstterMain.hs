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

-- Timelineのデータ型
data Timeline = Timeline {
      timelineName :: !TimelineName,
      timelineField :: !DrawingArea,
      timelineWindow :: !DrawWindow,
      timelineBody :: !TimelineBody,
      sinceId :: !(IORef String)
}

type TimelineSet = IORef [Timeline]

-- GUIデータ型
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

-- Access Tokenを所持していなかった場合、OAuth認証をユーザに行なってもらう
authorization :: GUI -> OAuth -> IO ()
authorization gui oauth = const () <$> do
  -- 認証用ウインドウ表示
  widgetShowAll (accessTokenGetWin gui)
  -- Request Token取得
  (requestToken, requestTokenSecret) <- getRequestTokenParameter oauth
  -- 認証を待機
  authorizationLink <- linkButtonNewWithLabel (authorizeURL requestToken) "Authorization Page"
  fixedPut (authorizationField gui) authorizationLink (150, 70)
  widgetShowAll $ authorizationField gui
  onClicked (authorizationButton gui) $ flip catch handler $ getNewAccessToken gui oauth requestToken requestTokenSecret
      where -- エラーハンドラ：認証に失敗したら再試行
        handler = \(e::SomeException) -> entrySetText (pinEntry gui) "" >> labelSetText (hint gui) "Sorry, Failed to authorize your account. Please try again."

-- Access Tokenを新規に取得
getNewAccessToken :: GUI -> OAuth -> Parameter -> Parameter -> IO ()
getNewAccessToken gui oauth requestToken requestTokenSecret = do
  -- PIN入力 -> oauth_verifierパラメータとして束縛
  verifier <- ("oauth_varifier",) <$> entryGetText (pinEntry gui)
  -- Access Token取得
  accessTokenData@[accessToken, accessTokenSecret, my_user_id, my_screen_name] <- getAccessTokenString oauth (snd requestTokenSecret) [requestToken, verifier]
  -- Access Token保持ファイルaccess.iniにAccess Token及びユーザ情報をセーブ
  runResourceT $ CL.sourceList accessTokenData $= CL.map (++"\n") $= CL.map pack $$ CB.sinkFile "access.ini"
  -- Access Tokenを設定したOAuthを引数に、メインルーチンを呼ぶ
  mainRoutine gui =<< newOAuth (consumerKey oauth) (consumerSecret oauth) accessToken accessTokenSecret my_user_id my_screen_name

-- Access Tokenを読み込む
restoreAccessToken :: GUI -> OAuth -> IO ()
restoreAccessToken gui oauth = do
  -- Access Token読み込み
  [accessToken, accessTokenSecret, my_user_id, my_screen_name] <- runResourceT $ CB.sourceFile "./access.ini" $= CB.lines $= CL.map unpack $$ CL.take 4
  -- Access Tokenを設定したOAuthを引数に、メインルーチンを呼ぶ
  mainRoutine gui =<< newOAuth (consumerKey oauth) (consumerSecret oauth) accessToken accessTokenSecret my_user_id my_screen_name

-- .gladeファイル（XML形式）からGUIウィジェットロード・GUI型に変換
loadGUI :: FilePath -> IO GUI
loadGUI = $(castToGUI ''GUI) <=< xmlNewIO
    where xmlNewIO gladePath = xmlNew gladePath >>= \maybeXML -> case maybeXML of {Just xml -> return xml; Nothing -> fail "XML format error."}

selectTimeline :: TimelineSet -> TimelineName -> IO Timeline
selectTimeline timelineset tlName = head . filter ((==tlName) . timelineName) <$> readIORef timelineset

-- アイコンのPixbufを取得
getIconImage :: IconTable -> Tweet -> IO GUI.Icon
getIconImage iconTable twt = do
  icon <- Hash.lookup iconTable (profile_image_url twt)
  pixbufNewFromFile =<< case icon of {Nothing -> getAndRegisterIcon; Just ic -> return ic}
      where getAndRegisterIcon = do
              ic <- getIcon (profile_image_url twt)
              Hash.insert iconTable (profile_image_url twt) ic
              return ic

-- タイムラインを追加
addTimeline :: GUI -> OAuth -> TimelineSet -> MVar (String, [Parameter]) -> MVar String -> MVar String -> TimelineName -> MVar () -> IO ()
addTimeline gui oauth timelineset sendText retweetTweet favoriteTweet timelineName connectionLock = do
  iconTable <- Hash.new (==) Hash.hashString
  field <- drawingAreaNew -- タイムライン表示領域を作成
  widgetModifyBg field StateNormal (Color 65535 65535 65535) -- 背景を白にセット
  scrolledWindowAddWithViewport (timelineWin gui) field -- ウインドウに貼り付け
  drawWin <- widgetGetDrawWindow field
  (body, sinceid) <- $(mapMT 2 [|newIORef|]) ([], "1")

  -- イベント検知・RT/favoriteの送信メソッドをセット
  widgetAddEvents field [ButtonPressMask]
  field `on` buttonPressEvent $ tryEvent $ const () <$> do -- ツイートがクリックされた
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
      widgetModifyBg tweetArea StateNormal (Color 65535 65535 65535) -- 背景を白にセット
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

  -- 再描画イベントに追加
  onExpose field $ \_ -> (const True) <$> do
    tlWidth <- (flip (-) 30) . fst <$> windowGetSize (mainWin gui)
    tlBody <- readIORef body
    tlHeight <- (\ l i m -> foldM m i l) tlBody 0 $ \i twt -> do
      icon <- getIconImage iconTable twt -- アイコン画像のPixbuf取得
      GUI.drawTweet field drawWin icon (0, 0, 0) tlWidth i twt
    widgetSetSizeRequest field tlWidth tlHeight
  let tl = Timeline timelineName field drawWin (TimelineBody body) sinceid
  -- タイムラインを一定のインターバルごとに更新
  forkIO $ (const ()) <$> do
    (flip timeoutAdd) 30000 $ (const True) <$> do -- タイムライン更新スレッド起動
      takeMVar connectionLock
      updateTimeline gui oauth tl
      putMVar connectionLock ()
  modifyIORef timelineset (tl:)
  widgetShowAll (timelineWin gui) -- 表示を更新
      where
        dialogHeight = 70
        idRetweet = 0
        idFavorite = 1
        idReply = 2

-- タイムラインを更新
updateTimeline :: GUI -> OAuth -> Timeline -> IO ()
updateTimeline gui oauth tl = do
  -- タイムラインから最新のツイートを取得
  sinceid <- readIORef $ sinceId tl
  tweets <- getTimelineData oauth [("since_id", sinceid), ("count", "200")] (timelineName tl) `catch` \(e::SomeException) -> (const []) <$> putStrLn "Timeline Update Error."
  unless (null tweets) $ writeIORef (sinceId tl) (tweet_id . head $ tweets)
  modifyIORef (body . timelineBody $ tl) (tweets++)
  -- 再描画
  widgetQueueDraw (timelineField tl)

-- tweetする
tweet :: GUI -> OAuth -> MVar (String, [Parameter]) -> IO ()
tweet gui oauth sendText = do
  -- tweetボタンを無効化
  widgetSetSensitivity (tweetButton gui) False
  tweetText <- entryGetText (tweetEntry gui)
  -- tweet送信
  putMVar sendText (tweetText, [])
  entrySetText (tweetEntry gui) ""
  labelSetText (information gui) ("From: " ++ (OAuth.screen_name oauth))
  -- tweetボタンを有効化
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
      -- reply送信
      putMVar sendText ("@" ++ username ++ " " ++ replyText, [("in_reply_to_status_id", replyToId)])
    ResponseCancel -> return ()
    ResponseDeleteEvent -> return ()
  widgetDestroy replyWindow
    where
      replyWindowHeight = 300
      replyWindowWidth = 400

-- メインウインドウ表示・タイムライン表示・更新タイマ作動・ツイートボタンにハンドラを設定（・認証用ウインドウ消去）
mainRoutine :: GUI -> OAuth -> IO ()
mainRoutine gui oauth = do
  timelineset <- newIORef []
  labelSetText (information gui) ("From: " ++ (OAuth.screen_name oauth))
  -- メインウインドウ表示
  widgetShowAll (mainWin gui)

  -- 通信するにはこの変数のロックを取る必要がある。
  connectionLock <- newMVar ()

  -- Home Timelineを追加
  sendText <- newEmptyMVar
  retweetTweet <- newEmptyMVar
  favoriteTweet <- newEmptyMVar
  addTimeline gui oauth timelineset sendText retweetTweet favoriteTweet "home_timeline" connectionLock
  hometl <- selectTimeline timelineset "home_timeline"
  updateTimeline gui oauth hometl

  -- "tweet"ボタンでツイート
  onClicked (tweetButton gui) (tweet gui oauth sendText)
  forkIO $ forever $ do -- tweet送信スレッド起動
    (tweetText, params) <- takeMVar sendText
    takeMVar connectionLock
    (flip catch) tweetErrorHandle $ sendTweet oauth tweetText params
    putMVar connectionLock ()

  -- リツイート
  forkIO $ forever $ do -- retweet送信スレッド起動
    retweetID <- takeMVar retweetTweet
    takeMVar connectionLock
    (flip catch) tweetErrorHandle $ retweet oauth retweetID
    putMVar connectionLock ()

  -- ふぁぼ
  forkIO $ forever $ do -- ふぁぼスレッド起動
    favoriteID <- takeMVar favoriteTweet
    takeMVar connectionLock
    (flip catch) tweetErrorHandle $ favorite oauth favoriteID
    putMVar connectionLock ()

  -- 認証用ウインドウ消去
  widgetHideAll (accessTokenGetWin gui)
    where -- エラーハンドラ
      tweetErrorHandle (TweetError err) = labelSetText (information gui) $ "Error: " ++ errMessage err
      errMessage EmptyTweet = "Please input some messages."
      errMessage CharactorExceeded = "You cannot transmit a tweet exceeding 140 characters."
      errMessage APIError = "API problem, please try again."

main :: FilePath -> IO ()
main gladePath = do
  -- GTK+システム初期化
  initGUI
  -- 他のスレッドが頻繁に走れるようにする
  timeoutAddFull (Control.Concurrent.yield >> return True) priorityDefaultIdle 100
  -- .gladeファイルからGUIウィジェット群をロード
  gui <- loadGUI gladePath
  -- x/Canselボタンでウインドウを消去
  onDestroy (mainWin gui) mainQuit
  onDestroy (accessTokenGetWin gui) mainQuit
  onClicked (cancelButton gui) mainQuit
  -- Consumer Key / Consumer Secret読み込み
  [consumerKey, consumerSecret] <- runResourceT $ CB.sourceFile "./config.ini" $= CB.lines $= CL.map unpack $$ CL.take 2
  oauth <- newOAuth consumerKey consumerSecret "" "" "" ""
  -- Access Tokenを取得し、メインウインドウを表示
  restoreAccessToken gui oauth `catch` \(e::SomeException) -> authorization gui oauth
  -- メインループ起動
  mainGUI