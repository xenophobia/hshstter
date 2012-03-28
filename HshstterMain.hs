{-# LANGUAGE TupleSections #-}

module HshstterMain where

import OAuth
import TweetJSON

import Network.HTTP
import System.IO
import Control.Monad
import Control.Applicative
import Codec.Binary.UTF8.String (decodeString, encodeString)

import Graphics.UI.Gtk hiding (add)
import Graphics.UI.Gtk.Glade
import Control.Concurrent

-- GUIデータ型
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

-- Access Tokenを所持していなかった場合、OAuth認証をユーザに行なってもらう
authorization :: GUI -> OAuth -> IO ()
authorization gui oauth = do
  -- 認証用ウインドウ表示
  widgetShowAll (accessTokenGetWin gui)
  -- リクエストトークン取得
  requestTokenParameters <- parseParameter <$> oauthRequest oauth requestTokenURL "" []
  requestToken <- getParameter requestTokenParameters "oauth_token"
  requestTokenSecret <- getParameter requestTokenParameters "oauth_token_secret"
  -- 認証ページのURLを提示
  entrySetText (authorizationURL gui) (authorizeURL ++ "?" ++ urlEncodeVars [requestToken])
  onClicked (authorizationButton gui) $ tryGetNewAccessToken gui oauth requestToken requestTokenSecret
  return ()
      where
        tryGetNewAccessToken gui oauth requestToken requestTokenSecret =
            getNewAccessToken gui oauth requestToken requestTokenSecret
                                  `catch` \_ -> do
                                    -- 認証に失敗したら再試行
                                    entrySetText (pinEntry gui) ""
                                    labelSetText (hint gui) "Sorry, Failed to authorize your account. Please try again."

-- Access Tokenを新規に取得
getNewAccessToken :: GUI -> OAuth -> Parameter -> Parameter -> IO ()
getNewAccessToken gui oauth requestToken requestTokenSecret = do
  -- PIN入力 -> oauth_verifierパラメータとして束縛
  verifier <- ("oauth_varifier",) <$> entryGetText (pinEntry gui)
  -- Access Token取得
  accessTokenParameters <- parseParameter <$> oauthRequest oauth accessTokenURL (snd requestTokenSecret) [requestToken, verifier]
  accessToken <- getParameter accessTokenParameters "oauth_token"
  accessTokenSecret <- getParameter accessTokenParameters "oauth_token_secret"
  -- Access Token保持ファイルaccess.iniにAccess Tokenをセーブ
  fout <- openFile "./access.ini" WriteMode
  hPutStrLn fout (snd accessToken)
  hPutStrLn fout (snd accessTokenSecret)
  hClose fout
  -- Access Tokenを設定したOAuthを引数に、メインルーチンを呼ぶ
  mainRoutine gui $ OAuth (consumerKey oauth) (consumerSecret oauth) (snd accessToken) (snd accessTokenSecret)

-- Access Tokenを読み込む
restoreAccessToken :: GUI -> OAuth -> IO ()
restoreAccessToken gui oauth = do
  -- Access Token読み込み
  fin <- openFile "./access.ini" ReadMode
  accessToken <- hGetLine fin
  accessTokenSecret <- hGetLine fin
  hClose fin
  -- Access Tokenを設定したOAuthを引数に、メインルーチンを呼ぶ
  mainRoutine gui $ OAuth (consumerKey oauth) (consumerSecret oauth) accessToken accessTokenSecret

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

  -- メインウインドウをロード
  guiMainWin <- xmlGetWidget xml castToWindow "mainWin"
  -- ツイート入力部をロード
  guiTweetEntry <- xmlGetWidget xml castToEntry "tweetEntry"
  -- ツイートボタンをロード
  guiTweetButton <- xmlGetWidget xml castToButton "tweetButton"
  -- タイムライン表示部をロード
  guiTimeline <- xmlGetWidget xml castToTextView "timeline"
  -- スクロールバーをロード
  guiTimelineWindow <- xmlGetWidget xml castToScrolledWindow "timelineWindow"
  -- Access Token取得ウインドウをロード
  guiAccessTokenWin <- xmlGetWidget xml castToWindow "accessTokenGetWin"
  -- 認証用メッセージをロード
  guiHint <- xmlGetWidget xml castToLabel "hint"
  -- Authorizationボタンをロード
  guiAuthorizationButton <- xmlGetWidget xml castToButton "authorizationButton"
  -- Authorization Cancelボタンをロード
  guiCancelButton <- xmlGetWidget xml castToButton "cancelButton"
  -- PIN入力部をロード
  guiPinEntry <- xmlGetWidget xml castToEntry "pinEntry"
  -- 認証用URL表示
  guiAuthorizationURL <- xmlGetWidget xml castToEntry "authorizationURL"
  
  return $ GUI guiMainWin guiTweetEntry guiTweetButton guiTimeline guiTimelineWindow
                      guiAccessTokenWin guiHint guiAuthorizationButton guiCancelButton guiPinEntry guiAuthorizationURL

-- タイムラインを表示
showTimeline :: GUI -> OAuth -> IO Bool
showTimeline gui oauth = do
  -- タイムラインから最新のツイートを取得
  res <- apiRequest oauth "home_timeline" GET [] `catch` \_ -> return "error"
  tweets <- getTimeline res `catch` \_ -> return []
  unless (null tweets) $ do
    buffer <- textViewGetBuffer (timeline gui)
    let tl = foldl (\s -> \t -> s ++ (show t) ++ "\n") "" tweets
    textBufferSetText buffer tl
  return True

-- ツイートする
tweet :: GUI -> OAuth -> IO ()
tweet gui oauth = do
  tweetText <- entryGetText (tweetEntry gui)
  apiRequest oauth "update" POST [("status", encodeString tweetText)] `catch` \_ -> return ""
  -- ツイート入力部をリセット
  entrySetText (tweetEntry gui) ""
  -- タイムラインを更新
  showTimeline gui oauth
  return ()

-- メインウインドウ表示・タイムライン表示・更新タイマ作動・ツイートボタンにハンドラを設定（・認証用ウインドウ消去）
mainRoutine :: GUI -> OAuth -> IO ()
mainRoutine gui oauth = do
  -- メインウインドウ表示
  widgetShowAll (mainWin gui)
  -- タイムライン更新
  showTimeline gui oauth
  -- タイムラインを一定のインターバルごとに更新
  timeoutAdd (showTimeline gui oauth) 30000
  -- "tweet"ボタンでツイート
  onClicked (tweetButton gui) (tweet gui oauth)
  -- 認証用ウインドウ消去
  widgetHideAll (accessTokenGetWin gui)

main :: FilePath -> IO ()
main gladePath = do
  -- GTK+システム初期化
  initGUI
  -- 他のスレッドが頻繁に走れるようにする
  timeoutAddFull (yield >> return True) priorityDefaultIdle 100
  -- .gladeファイルをロード
  gui <- loadGlade gladePath  -- ウインドウを表示
  -- x/Canselボタンでウインドウを消去
  onDestroy (mainWin gui) mainQuit
  onDestroy (accessTokenGetWin gui) mainQuit
  onClicked (cancelButton gui) mainQuit

  -- OAuth関連
  -- Consumer Key / Consumer Secret読み込み
  fin <- openFile "./config.ini" ReadMode
  consumerKey <- hGetLine fin
  consumerSecret <- hGetLine fin
  hClose fin
  let oauth = OAuth consumerKey consumerSecret "" ""

  -- Access Tokenを取得し、メインウインドウを表示
  restoreAccessToken gui oauth `catch` \_ -> authorization gui oauth

  -- メインループ
  mainGUI

