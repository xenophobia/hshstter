module TweetJSON where

import Control.Monad
import Text.JSON
import Data.Ratio

-- ツイート
data Tweet = Tweet {
      tweet_id :: !String, -- ID
      name :: !String, -- ユーザ名
      screen_name :: !String, -- ユーザID
      source :: !String, -- クライアント
      created_at :: !String, -- ツイートされた時刻
      profile_image_url :: !String, -- プロフィール画像
      text :: !String -- ツイート内容
}

instance Show Tweet where
    show tweet =
        screen_name tweet ++ "(" ++ name tweet ++ ") [tweeted at " ++ created_at tweet ++ "]\n" ++ (map (\c -> if c == '\n' then ' ' else c) . text $ tweet) ++ "\n"

-- オブジェクト名指定で対応するJSONオブジェクトの値を取得
findJSObject :: Monad m => JSObject a -> String -> m a
findJSObject jsobject objectName =
    case dropWhile ((/= objectName) . fst) (fromJSObject jsobject) of
      [] -> fail $ "findJSObject: Not_found(\"" ++ objectName ++ "\")"
      object:_ -> return . snd $ object

ofJSBool :: Monad m => JSValue -> m Bool
ofJSBool (JSBool b) = return b
ofJSBool _ = fail "ofJSBool: Not JSBool"

ofJSString :: Monad m => JSValue -> m String
ofJSString (JSString str) = return $ fromJSString str
ofJSString _ = fail "ofJSBool: Not JSString"

ofJSObject :: Monad m => JSValue -> m (JSObject JSValue)
ofJSObject (JSObject jsobject) = return jsobject
ofJSObject _ = fail "ofJSBool: Not JSObject"

ofJSRational :: Monad m => JSValue -> m String
ofJSRational (JSRational _ ratio) = return (show . numerator $ ratio)
ofJSRational _ = fail "ofJSBool: Not JSRational"

-- tweetを取得
getTweet :: Monad m => JSObject JSValue -> m Tweet
getTweet jsobject = do
  tweet_id          <- ofJSRational =<< findJSObject jsobject "id"
  user              <- ofJSObject   =<< findJSObject jsobject "user"
  name              <- ofJSString   =<< findJSObject user     "name"
  screen_name       <- ofJSString   =<< findJSObject user     "screen_name"
  source            <- ofJSString   =<< findJSObject jsobject "source"
  created_at        <- ofJSString   =<< findJSObject jsobject "created_at"
  profile_image_url <- ofJSString   =<< findJSObject user     "profile_image_url"
  text              <- ofJSString =<< findJSObject jsobject "text"
  return $ Tweet tweet_id name screen_name source created_at profile_image_url text

-- tweetのリストを取得
getTweetList :: Monad m => String -> m [Tweet]
getTweetList rsp = case decode rsp of
                     Ok (JSArray tweets) -> mapM (ofJSObject >=> getTweet) tweets
                     _ -> fail "getTimeline: JSON format error"