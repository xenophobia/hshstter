module TweetJSON where

import Control.Monad
import Text.JSON

-- ツイート
data Tweet = Tweet {
      name :: !String, -- ユーザ名
      screen_name :: !String, -- ユーザID
      retweeted :: !Bool, -- リツイートされたか
      created_at :: !String, -- ツイートされた時刻
      profile_image_url :: !String, -- プロフィール画像
      text :: !String -- ツイート内容
}

instance Show Tweet where
    show tweet =
        let retweet_mark = if retweeted tweet then "[Retweet]" else "" in
        retweet_mark ++ screen_name tweet ++ "(" ++ name tweet ++ ") [tweeted at " ++ created_at tweet ++ "]\n" ++ text tweet ++ "\n"

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

-- ツイートを取得
getTweet :: Monad m => JSObject JSValue -> m Tweet
getTweet jsobject = do
  user              <- ofJSObject =<< findJSObject jsobject "user"
  name              <- ofJSString =<< findJSObject user     "name"
  screen_name       <- ofJSString =<< findJSObject user     "screen_name"
  retweeted         <- ofJSBool   =<< findJSObject jsobject "retweeted"
  created_at        <- ofJSString =<< findJSObject jsobject "created_at"
  profile_image_url <- ofJSString =<< findJSObject user     "profile_image_url"
  text              <- ofJSString =<< findJSObject jsobject "text"
  return $ Tweet name screen_name retweeted created_at profile_image_url text

-- タイムラインをTweet型のリストとして取得
getTimeline :: Monad m => JSValue -> m [Tweet]
getTimeline (JSArray tweets) = mapM (ofJSObject >=> getTweet) tweets
getTimeline _ = fail "getTimeline: JSON format error"

showOfTweet :: JSValue -> String
showOfTweet JSNull = ""
showOfTweet (JSBool b) = show b
showOfTweet (JSRational b r) = show b ++ ": " ++ show r
showOfTweet (JSString str) = fromJSString str
showOfTweet (JSArray tweets) = foldl (\s -> \(i, t) -> s ++ "[" ++ show i ++ "]\n" ++ showOfTweet t ++ "\n") "" (zip [0..] tweets)
showOfTweet (JSObject jsobject) =
    let jsobjectString = fromJSObject jsobject in
    foldl (\s -> \(name, content) -> s ++ name ++ " = \n" ++ showOfTweet content ++ "\n") "" jsobjectString