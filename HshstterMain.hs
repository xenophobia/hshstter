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

-- GUIデータ型
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

-- 情報表示を初期状態(From: (screen_name))にする
initInformation :: GUI -> OAuth -> IO ()
initInformation gui oauth = labelSetText (information gui) ("From: " ++ (OAuth.screen_name oauth))

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
  accessTokenParameters <- parseParameter <$> oauthRequest oauth accessTokenURL (snd requestTokenSecret) [requestToken, verifier]
  accessToken <- getParameter accessTokenParameters "oauth_token"
  accessTokenSecret <- getParameter accessTokenParameters "oauth_token_secret"
  my_user_id <- getParameter accessTokenParameters "user_id"
  my_screen_name <- getParameter accessTokenParameters "screen_name"
  accessTokenData@(accessToken:accessTokenSecret:my_user_id:my_screen_name:[])
      <- mapM (fmap snd . getParameter accessTokenParameters) ["oauth_token", "oauth_token_secret", "user_id", "screen_name"]
  -- Access Token保持ファイルaccess.iniにAccess Token及びユーザ情報をセーブ
  runResourceT $ CL.sourceList ((:[]) . intercalate "\n" $ accessTokenData) $= CL.map pack $$ CB.sinkFile "access.ini"
  -- Access Tokenを設定したOAuthを引数に、メインルーチンを呼ぶ
  mainRoutine gui $ OAuth (consumerKey oauth) (consumerSecret oauth) accessToken accessTokenSecret my_user_id my_screen_name

-- Access Tokenを読み込む
restoreAccessToken :: GUI -> OAuth -> IO ()
restoreAccessToken gui oauth = do
  -- Access Token読み込み
  (accessToken:accessTokenSecret:my_user_id:my_screen_name:[]) <- runResourceT $ CB.sourceFile "./access.ini" $= CB.lines $= CL.map unpack $$ CL.take 4
  -- Access Tokenを設定したOAuthを引数に、メインルーチンを呼ぶ
  mainRoutine gui $ OAuth (consumerKey oauth) (consumerSecret oauth) accessToken accessTokenSecret my_user_id my_screen_name

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
  guiTimeline <- xmlGetWidget xml castToTextView "timeline"  -- タイムライン表示部をロード
  guiTimelineWindow <- xmlGetWidget xml castToScrolledWindow "timelineWindow"  -- スクロールバーをロード
  guiAccessTokenWin <- xmlGetWidget xml castToWindow "accessTokenGetWin"  -- Access Token取得ウインドウをロード
  guiHint <- xmlGetWidget xml castToLabel "hint"  -- 認証用メッセージをロード
  guiAuthorizationButton <- xmlGetWidget xml castToButton "authorizationButton"  -- Authorizationボタンをロード
  guiCancelButton <- xmlGetWidget xml castToButton "cancelButton"  -- Cancelボタンをロード
  guiPinEntry <- xmlGetWidget xml castToEntry "pinEntry"  -- PIN入力部をロード
  guiAuthorizationURL <- xmlGetWidget xml castToEntry "authorizationURL"  -- 認証用URL表示

  return $ GUI guiMainWin guiInformation guiTweetEntry guiTweetButton guiTimeline guiTimelineWindow
                      guiAccessTokenWin guiHint guiAuthorizationButton guiCancelButton guiPinEntry guiAuthorizationURL

-- タイムラインを更新
updateTimeline :: GUI -> OAuth -> Curl -> IO Bool
updateTimeline gui oauth curl = do
  -- タイムラインから最新のツイートを取得
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

-- ツイートする
tweet :: GUI -> OAuth -> Curl -> Curl -> IO ()
tweet gui oauth curlTweet curlTimeline = do
  -- tweet ボタンを無効化
  widgetSetSensitivity (tweetButton gui) False
  -- ツイート送信
  catch sendTweet tweetErrorHandle
  -- タイムラインを更新
  updateTimeline gui oauth curlTimeline
  reset curlTweet
  -- tweet ボタンを有効化
  widgetSetSensitivity (tweetButton gui) True
      where
        sendTweet = do -- ツイート
          tweetText <- entryGetText (tweetEntry gui)
          let lengthOfTweet = length tweetText
          if lengthOfTweet == 0
            then throw (TweetError EmptyTweet)
            else if lengthOfTweet > 140 then throw (TweetError CharactorExceeded)
            else do
              apiRequest curlTweet oauth "update" POST [("status", encodeString tweetText)] `catch` \(_::SomeException) -> throw (TweetError APIError)
              -- ツイート入力部をリセット
              entrySetText (tweetEntry gui) ""
              -- 情報ラベル初期化
              initInformation gui oauth
        tweetErrorHandle (TweetError err) =
            let errMessage = case err of
                               EmptyTweet -> "Please input some messages."
                               CharactorExceeded -> "You cannot transmit a tweet exceeding 140 characters."
                               APIError -> "API problem, please try again."
            in
              labelSetText (information gui) $ "Error: " ++ errMessage


-- メインウインドウ表示・タイムライン表示・更新タイマ作動・ツイートボタンにハンドラを設定（・認証用ウインドウ消去）
mainRoutine :: GUI -> OAuth -> IO ()
mainRoutine gui oauth = do
  -- 情報ラベル初期化
  initInformation gui oauth
  -- メインウインドウ表示
  widgetShowAll (mainWin gui)
  -- タイムライン更新
  curlTimeline <- initialize
  updateTimeline gui oauth curlTimeline
  -- タイムラインを一定のインターバルごとに更新
  timeoutAdd (updateTimeline gui oauth curlTimeline) 30000
  -- "tweet"ボタンでツイート
  curlTweet <- initialize
  onClicked (tweetButton gui) (tweet gui oauth curlTweet curlTimeline)
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

  -- Consumer Key / Consumer Secret読み込み
  (consumerKey:consumerSecret:[]) <- runResourceT $ CB.sourceFile "./config.ini" $= CB.lines $= CL.map unpack $$ CL.take 2
  let oauth = OAuth consumerKey consumerSecret "" "" "" ""

  -- Access Tokenを取得し、メインウインドウを表示
  restoreAccessToken gui oauth `catch` \(e::SomeException) -> authorization gui oauth

  -- メインループ
  mainGUI

