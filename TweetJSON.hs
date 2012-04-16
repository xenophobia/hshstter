module TweetJSON where

import Control.Monad
import Text.JSON

-- �ĥ�����
data Tweet = Tweet {
      name :: !String, -- �桼��̾
      screen_name :: !String, -- �桼��ID
      retweeted :: !Bool, -- ��ĥ����Ȥ��줿��
      created_at :: !String, -- �ĥ����Ȥ��줿����
      profile_image_url :: !String, -- �ץ�ե��������
      text :: !String -- �ĥ���������
}

instance Show Tweet where
    show tweet =
        screen_name tweet ++ "(" ++ name tweet ++ ") [tweeted at " ++ created_at tweet ++ "]\n" ++ text tweet ++ "\n"

-- ���֥�������̾������б�����JSON���֥������Ȥ��ͤ����
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

-- tweet�����
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

-- tweet�Υꥹ�Ȥ����
getTweetList :: Monad m => String -> m [Tweet]
getTweetList rsp = case decode rsp of
                     Ok (JSArray tweets) -> mapM (ofJSObject >=> getTweet) tweets
                     _ -> fail "getTimeline: JSON format error"