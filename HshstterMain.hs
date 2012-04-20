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

-- Timelineのデータ型
data Timeline = Timeline {
      timelineName :: !TimelineName,
      timelineHeight :: !(IORef Int),
      timelineField :: !DrawingArea,
      timelineWindow :: !DrawWindow,
      timelineBody :: !TimelineBody,
      position :: !GUI.Coordinate
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
      authorizationURL :: !Entry
    }

-- Access Tokenを所持していなかった場合、OAuth認証をユーザに行なってもらう
authorization :: GUI -> OAuth -> IO ()
authorization gui oauth = do
  -- 認証用ウインドウ表示
  widgetShowAll (accessTokenGetWin gui)
  -- Request Token取得
  (requestToken, requestTokenSecret) <- getRequestTokenParameter oauth
  -- 認証を待機
  entrySetText (authorizationURL gui) (authorizeURL requestToken)
  onClicked (authorizationButton gui) $ flip catch handler $ getNewAccessToken gui oauth requestToken requestTokenSecret
  return ()
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

selectTimeline :: TimelineName -> TimelineSet -> IO Timeline
selectTimeline tlName timelineset = head . filter ((==tlName) . timelineName) <$> readIORef timelineset

-- タイムラインを追加
addTimeline :: GUI -> TimelineSet -> TimelineName -> IO ()
addTimeline gui timelineset timelineName = do
  let pos = (0, 0)
  field <- drawingAreaNew -- タイムライン表示領域を作成
  widgetModifyBg field StateNormal (Color 65535 65535 65535) -- 背景を白にセット
  scrolledWindowAddWithViewport (timelineWin gui) field -- ウインドウに貼り付け
  drawWin <- widgetGetDrawWindow field
  (body, height) <- $(mapMT 2 [|newIORef|]) ("", 0)
  -- 再描画イベントに追加
  onExpose field $ \_ -> do
    tlWidth <- fst . first (flip (-) 30) <$> windowGetSize (mainWin gui)
    (tlBody, tlHeight) <- $(mapMT 2 [|readIORef|]) (body, height)
    GUI.drawString field drawWin (0, 0, 0) pos tlWidth tlBody
    widgetSetSizeRequest field tlWidth tlHeight
    return True
  modifyIORef timelineset (Timeline timelineName height field drawWin (TimelineBody body) pos:)
  widgetShowAll (timelineWin gui) -- 表示を更新

-- タイムラインを更新
updateTimeline :: GUI -> OAuth -> Timeline -> IO ()
updateTimeline gui oauth tl =
  flip catch getTimelineErrorHandle $ do
    -- タイムラインから最新のツイートを取得
    tweets <- getTimelineData oauth (timelineName tl) `catch` \(e::SomeException) -> putStrLn "error" >> return []
    let (x, y) = position tl
        tlText = foldl (\s -> \t -> s ++ (show t) ++ "\n") "" tweets
    writeIORef (body . timelineBody $ tl) tlText
    writeIORef (timelineHeight $ tl) ((*25) . length . lines $ tlText)
    -- 再描画
    widgetQueueDraw (timelineField tl)
      where -- エラーハンドラ（単に無視）
        getTimelineErrorHandle = \(e::SomeException) -> return ()

-- tweetする
tweet :: GUI -> OAuth -> MVar String -> IO ()
tweet gui oauth sendText = do
  -- tweetボタンを無効化
  widgetSetSensitivity (tweetButton gui) False
  tweetText <- entryGetText (tweetEntry gui)
  -- tweet送信
  putMVar sendText tweetText
  entrySetText (tweetEntry gui) ""
  labelSetText (information gui) ("From: " ++ (OAuth.screen_name oauth))
  -- tweetボタンを有効化
  widgetSetSensitivity (tweetButton gui) True

-- メインウインドウ表示・タイムライン表示・更新タイマ作動・ツイートボタンにハンドラを設定（・認証用ウインドウ消去）
mainRoutine :: GUI -> OAuth -> IO ()
mainRoutine gui oauth = do
  timelineset <- newIORef []
  labelSetText (information gui) ("From: " ++ (OAuth.screen_name oauth))
  -- メインウインドウ表示
  widgetShowAll (mainWin gui)
  -- Home Timelineを追加
  addTimeline gui timelineset "home_timeline"
  hometl <- selectTimeline "home_timeline" timelineset
  -- 通信するにはこの変数のロックを取る必要がある。
  connectionLock <- newMVar ()
  updateTimeline gui oauth hometl
  -- タイムラインを一定のインターバルごとに更新
  forkIO $ do
    (flip timeoutAdd) 3000 $ do
      takeMVar connectionLock
      updateTimeline gui oauth hometl
      putMVar connectionLock ()
      return True
    return ()
  -- "tweet"ボタンでツイート
  sendText <- newEmptyMVar
  onClicked (tweetButton gui) (tweet gui oauth sendText)
  forkIO $ forever $ do
    tweetText <- takeMVar sendText
    takeMVar connectionLock
    (flip catch) tweetErrorHandle (sendTweet oauth tweetText >> return ())
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