{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module HshstterMain where

import OAuth
import TweetJSON
import HshstterConnectWithTwitter
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

import System.Random

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
      timelineWidth :: !(IORef Int),
      timelineHeight :: !(IORef Int),
      timelineField :: !DrawingArea,
      timelineWindow :: !DrawWindow,
      timelineBody :: !TimelineBody,
      position :: !GUI.Coordinate
}

-- GUIデータ型
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

-- 情報表示を初期状態(From: (screen_name))にする
initInformation :: GUI -> OAuth -> IO ()
initInformation gui oauth = labelSetText (information gui) ("From: " ++ (OAuth.screen_name oauth))

-- Access Tokenを所持していなかった場合、OAuth認証をユーザに行なってもらう
authorization :: GUI -> OAuth -> IO ()
authorization gui oauth = do
  -- 認証用ウインドウ表示
  widgetShowAll (accessTokenGetWin gui)
  -- Request Token取得
  (requestToken, requestTokenSecret) <- getRequestTokenParameter oauth
  -- 認証ページのURLを表示
  entrySetText (authorizationURL gui) (authorizeURL requestToken)
  -- 認証用ボタンをクリックで認証を試みる
  onClicked (authorizationButton gui) $ tryGetNewAccessToken gui oauth requestToken requestTokenSecret
  return ()
      where
        tryGetNewAccessToken gui oauth requestToken requestTokenSecret =
            getNewAccessToken gui oauth requestToken requestTokenSecret
                                  `catch` \(e::SomeException) -> do
                                    -- 認証に失敗したら再試行
                                    entrySetText (pinEntry gui) ""
                                    labelSetText (hint gui) "Sorry, Failed to authorize your account. Please try again."

-- Access Tokenを新規に取得
getNewAccessToken :: GUI -> OAuth -> Parameter -> Parameter -> IO ()
getNewAccessToken gui oauth requestToken requestTokenSecret = do
  -- PIN入力 -> oauth_verifierパラメータとして束縛
  verifier <- ("oauth_varifier",) <$> entryGetText (pinEntry gui)
  -- Access Token取得
  accessTokenData@(accessToken:accessTokenSecret:my_user_id:my_screen_name:[]) <- getAccessTokenString oauth (snd requestTokenSecret) [requestToken, verifier]
  -- Access Token保持ファイルaccess.iniにAccess Token及びユーザ情報をセーブ
  runResourceT $ CL.sourceList accessTokenData $= CL.map (++"\n") $= CL.map pack $$ CB.sinkFile "access.ini"
  -- Access Tokenを設定したOAuthを引数に、メインルーチンを呼ぶ
  mainRoutine gui =<< newOAuth (consumerKey oauth) (consumerSecret oauth) accessToken accessTokenSecret my_user_id my_screen_name

-- Access Tokenを読み込む
restoreAccessToken :: GUI -> OAuth -> IO ()
restoreAccessToken gui oauth = do
  -- Access Token読み込み
  (accessToken:accessTokenSecret:my_user_id:my_screen_name:[]) <- runResourceT $ CB.sourceFile "./access.ini" $= CB.lines $= CL.map unpack $$ CL.take 4
  -- Access Tokenを設定したOAuthを引数に、メインルーチンを呼ぶ
  mainRoutine gui =<< newOAuth (consumerKey oauth) (consumerSecret oauth) accessToken accessTokenSecret my_user_id my_screen_name

xmlNewIO :: FilePath -> IO GladeXML
xmlNewIO gladePath = do
  maybeXML <- xmlNew gladePath
  case maybeXML of
    Nothing -> fail "XML format error."
    Just xml -> return xml

-- .gladeファイル（XML形式）をロード・GUI型に変換
loadGlade :: FilePath -> IO GUI
loadGlade gladePath = do
  -- XMLをロード
  xml <- xmlNewIO gladePath

  guiMainWin <- xmlGetWidget xml castToWindow "mainWin"  -- メインウインドウをロード
  guiInformation <- xmlGetWidget xml castToLabel "information"  -- 情報
  guiTweetEntry <- xmlGetWidget xml castToEntry "tweetEntry"  -- ツイート入力部をロード
  guiTweetButton <- xmlGetWidget xml castToButton "tweetButton"  -- ツイートボタンをロード

  -- タイムライン表示部をロード
  guiTimelineWin <- xmlGetWidget xml castToScrolledWindow "timelineWindow"
  guiTimeline <- newIORef []

  guiAccessTokenWin <- xmlGetWidget xml castToWindow "accessTokenGetWin"  -- Access Token取得ウインドウをロード
  guiHint <- xmlGetWidget xml castToLabel "hint"  -- 認証用メッセージをロード
  guiAuthorizationButton <- xmlGetWidget xml castToButton "authorizationButton"  -- Authorizationボタンをロード
  guiCancelButton <- xmlGetWidget xml castToButton "cancelButton"  -- Cancelボタンをロード
  guiPinEntry <- xmlGetWidget xml castToEntry "pinEntry"  -- PIN入力部をロード
  guiAuthorizationURL <- xmlGetWidget xml castToEntry "authorizationURL"  -- 認証用URL表示

  return $ GUI guiMainWin guiInformation guiTweetEntry guiTweetButton guiTimelineWin guiTimeline
                      guiAccessTokenWin guiHint guiAuthorizationButton guiCancelButton guiPinEntry guiAuthorizationURL

selectTimeline :: TimelineName -> [Timeline] -> Timeline
selectTimeline tlName = head . filter ((==tlName) . timelineName)

-- タイムラインを追加
addTimeline :: GUI -> TimelineName -> IO ()
addTimeline gui timelineName = do
  let pos = (0, 0)
  field <- drawingAreaNew -- タイムライン表示領域を作成
  widgetModifyBg field StateNormal (Color 65535 65535 65535) -- 背景を白にセット
  scrolledWindowAddWithViewport (timelineWin gui) field -- ウインドウに貼り付け
  drawWin <- widgetGetDrawWindow field
  body <- newIORef ""
  width <- newIORef 300
  height <- newIORef 0
  -- 再描画イベントに追加
  onExpose field $ \_ -> do
    tlBody <- readIORef body
    tlWidth <- readIORef width
    tlHeight <- readIORef height
    GUI.drawString field drawWin (0, 0, 0) pos tlWidth tlBody
    widgetSetSizeRequest field tlWidth tlHeight
    return True
  modifyIORef (timeline gui) (Timeline timelineName width height field drawWin (TimelineBody body) pos:)
  widgetShowAll (timelineWin gui) -- 表示を更新

-- タイムラインのフィールドにデータを描画
drawTimelineData :: Timeline -> OAuth -> IO ()
drawTimelineData tl oauth = do
  -- タイムラインから最新のツイートを取得
  tweets <- getTimelineData oauth (timelineName tl) `catch` \(e::SomeException) -> putStrLn "error" >> return []
  let (x, y) = position tl
      tlText = foldl (\s -> \t -> s ++ (show t) ++ "\n") "" tweets
  -- 描画イベントを発生させる
  writeIORef (body . timelineBody $ tl) tlText
  writeIORef (timelineHeight $ tl) ((*25) . length . lines $ tlText)
  drawWindowClearAreaExpose (timelineWindow tl) x y 300 ((*25) . length . lines $ tlText)
  return ()

-- タイムラインを更新
updateTimeline :: GUI -> OAuth -> Timeline -> IO Bool
updateTimeline gui oauth tl = do
  flip catch getTimelineErrorHandle $ do
    -- タイムラインのデータを描画
    drawTimelineData tl oauth
  return True
      where
        -- エラーハンドラ（単に無視）
        getTimelineErrorHandle = \(e::SomeException) -> return ()

-- tweetする
tweet :: GUI -> OAuth -> IO ()
tweet gui oauth = do
  -- tweetボタンを無効化
  widgetSetSensitivity (tweetButton gui) False
  -- tweet entryからtweet内容を取り出す
  tweetText <- entryGetText (tweetEntry gui)

  flip catch tweetErrorHandle $ do
    -- tweet送信
    sendTweet oauth tweetText
    -- tweet入力部をリセット
    entrySetText (tweetEntry gui) ""
    -- 情報ラベル初期化
    initInformation gui oauth

  -- home timelineを更新
  updateTimeline gui oauth . selectTimeline "home_timeline" =<< readIORef (timeline gui)
  -- tweetボタンを有効化
  widgetSetSensitivity (tweetButton gui) True
      where
        -- エラーハンドラ
        tweetErrorHandle (TweetError err) = labelSetText (information gui) $ "Error: " ++ errMessage err
        errMessage EmptyTweet = "Please input some messages."
        errMessage CharactorExceeded = "You cannot transmit a tweet exceeding 140 characters."
        errMessage APIError = "API problem, please try again."

-- メインウインドウ表示・タイムライン表示・更新タイマ作動・ツイートボタンにハンドラを設定（・認証用ウインドウ消去）
mainRoutine :: GUI -> OAuth -> IO ()
mainRoutine gui oauth = do
  -- 情報ラベル初期化
  initInformation gui oauth
  -- メインウインドウ表示
  widgetShowAll (mainWin gui)
  -- Home Timelineを追加
  addTimeline gui "home_timeline"
  
  hometl <- selectTimeline "home_timeline" <$> readIORef (timeline gui)
  -- タイムライン更新
  updateTimeline gui oauth hometl
  -- タイムラインを一定のインターバルごとに更新
  timeoutAdd (updateTimeline gui oauth hometl) 30000
  -- "tweet"ボタンでツイート
  onClicked (tweetButton gui) (tweet gui oauth)
  -- 認証用ウインドウ消去
  widgetHideAll (accessTokenGetWin gui)

main :: FilePath -> IO ()
main gladePath = do
  -- GTK+システム初期化
  initGUI
  -- 他のスレッドが頻繁に走れるようにする
  timeoutAddFull (Control.Concurrent.yield >> return True) priorityDefaultIdle 100
  -- .gladeファイルをロード
  gui <- loadGlade gladePath  -- ウインドウを表示
  -- x/Canselボタンでウインドウを消去
  onDestroy (mainWin gui) mainQuit
  onDestroy (accessTokenGetWin gui) mainQuit
  onClicked (cancelButton gui) mainQuit

  -- Consumer Key / Consumer Secret読み込み
  (consumerKey:consumerSecret:[]) <- runResourceT $ CB.sourceFile "./config.ini" $= CB.lines $= CL.map unpack $$ CL.take 2
  oauth <- newOAuth consumerKey consumerSecret "" "" "" ""

  -- Access Tokenを取得し、メインウインドウを表示
  restoreAccessToken gui oauth `catch` \(e::SomeException) -> authorization gui oauth

  -- メインループ
  mainGUI