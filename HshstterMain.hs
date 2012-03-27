{-# LANGUAGE TupleSections #-}

module HshstterMain where

import OAuth
import TweetJSON

import Network.HTTP
import Network.URI
import Text.JSON
import Data.Maybe
import Data.List
import System.IO
import System.Random
import System.Time
import Control.Arrow
import Control.Applicative
import Data.Digest.Pure.SHA
import qualified System.IO.UTF8 as U8
import Codec.Binary.UTF8.String (decodeString, encodeString)
import Data.Char
import qualified Codec.Binary.Base64 as B64
import qualified Data.ByteString.Lazy as L

import Graphics.UI.Gtk hiding (add, disconnect)
import Graphics.UI.Gtk.Glade
import Control.Concurrent

-- GUIデータ型
data GUI = GUI {
      mainWin :: !Window,
      tweetEntry :: !Entry,
      tweetButton :: !Button,
      timeline :: !TextView,
      timelineWindow :: !ScrolledWindow,
      accessTokenGetWin :: !Window
    }

-- アクセストークンを新規に取得
getNewAccessToken :: GUI -> String -> String -> IO (Parameter, Parameter)
getNewAccessToken gui consumerKey consumerSecret = do
  putStrLn "Access Token is not found."
  oauth_ <- newOAuth consumerKey consumerSecret "" ""
  -- リクエストトークン発行要求リクエスト生成
  requestForGetRequestToken <- oauthRequest oauth_ requestTokenURL "" []
  -- リクエストトークン取得
  requestTokenParameters <- (fmap $ parseParameter . rspBody) . simpleHTTPIO $ requestForGetRequestToken
  requestToken <- getParameter requestTokenParameters "oauth_token"
  requestTokenSecret <- getParameter requestTokenParameters "oauth_token_secret"

  -- 認証ページのアドレス表示
  putStrLn $ authorizeURL ++ "?" ++ urlEncodeVars [requestToken]

  -- PIN入力 -> oauth_verifierパラメータとして束縛
  verifier <- ("oauth_varifier",) <$> getLine
  -- アクセストークン発行要求リクエスト生成
  requestForGetAccessToken <- oauthRequest oauth_ accessTokenURL (snd requestTokenSecret) [requestToken, verifier]
  -- アクセストークン取得
  accessTokenParameters <- (fmap $ parseParameter . rspBody) . simpleHTTPIO $ requestForGetAccessToken
  accessToken <- getParameter accessTokenParameters "oauth_token"
  accessTokenSecret <- getParameter accessTokenParameters "oauth_token_secret"
  -- アクセストークン保持ファイルaccess.iniにアクセストークンをセーブ
  fout <- openFile "./access.ini" WriteMode
  hPutStrLn fout (snd accessToken)
  hPutStrLn fout (snd accessTokenSecret)
  hClose fout
  return (accessToken, accessTokenSecret)
  
-- アクセストークンを読み込む
restoreAccessToken :: IO (Parameter, Parameter)
restoreAccessToken = do
  -- アクセストークン読み込み
  fin <- openFile "./access.ini" ReadMode
  accessToken <- hGetLine fin
  accessTokenSecret <- hGetLine fin
  hClose fin
  putStrLn "Access Token successfully restored."
  return (("oauth_token", accessToken), ("oauth_token_secret", accessTokenSecret))

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
  -- アクセストークン取得ウインドウをロード
  guiAccessTokenWin <- xmlGetWidget xml castToWindow "accessTokenGetWin"
  -- ツイート入力部をロード
  guiTweetEntry <- xmlGetWidget xml castToEntry "tweetEntry"
  -- ツイートボタンをロード
  guiTweetButton <- xmlGetWidget xml castToButton "tweetButton"
  -- タイムライン表示部をロード
  guiTimeline <- xmlGetWidget xml castToTextView "timeline"
  -- スクロールバーをロード
  guiTimelineWindow <- xmlGetWidget xml castToScrolledWindow "timelineWindow"
  
  return $ GUI guiMainWin guiTweetEntry guiTweetButton guiTimeline guiTimelineWindow guiAccessTokenWin

-- タイムラインを表示
showTimeline :: GUI -> OAuth -> IO Bool
showTimeline gui oauth = do
  -- タイムラインから最新のツイートを取得
  newestTweet <- apiRequest oauth "home_timeline" GET []
  res <- simpleHTTPIO newestTweet
  let tryJSON = case decode (rspBody res) of
                  Ok a -> a
                  Error _ -> JSNull
  tweets <- (getTimeline tryJSON)
  -- text buffer生成
  buffer <- textViewGetBuffer (timeline gui)
  let tl = foldl (\s -> \t -> s ++ (show t) ++ "\n") "" tweets
  textBufferSetText buffer tl
  return True

-- ツイートする
tweet :: GUI -> OAuth -> IO ()
tweet gui oauth = do
  tweetText <- entryGetText (tweetEntry gui)
  tweet <- apiRequest oauth "update" POST [("status", encodeString tweetText)]
  res <- simpleHTTPIO tweet
  -- ツイート入力部をリセット
  entrySetText (tweetEntry gui) ""
  -- タイムラインを更新
  showTimeline gui oauth
  return ()

main :: FilePath -> IO ()
main gladePath = do

  -- GTK+システム初期化
  initGUI

  -- 他のスレッドが頻繁に走れるようにする
  timeoutAddFull (yield >> return True)
                 priorityDefaultIdle 100

  -- .gladeファイルをロード
  gui <- loadGlade gladePath  -- ウインドウを表示
  widgetShowAll . mainWin $ gui
  

  -- xボタンでウインドウを消去
  onDestroy (mainWin gui) mainQuit
  
  -- OAuth関連
  -- Consumer Key / Consumer Secret読み込み
  fin <- openFile "./config.ini" ReadMode
  consumerKey <- hGetLine fin
  consumerSecret <- hGetLine fin
  hClose fin
  -- アクセストークン取得
  (accessToken, accessTokenSecret) <- restoreAccessToken `catch` \_ -> getNewAccessToken gui consumerKey consumerSecret
  oauth <- newOAuth consumerKey consumerSecret (snd accessToken) (snd accessTokenSecret)

  -- タイムライン表示
  showTimeline gui oauth
  timeoutAdd (showTimeline gui oauth) 30000

  -- "tweet"ボタンでツイート
  onClicked (tweetButton gui) (tweet gui oauth)

  -- メインループ
  mainGUI

