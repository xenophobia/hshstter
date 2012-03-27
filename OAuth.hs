module OAuth  where

import Network.HTTP
import Network.URI
import Data.Maybe
import Data.List
import System.IO
import System.Random
import System.Time
import Control.Arrow
import Control.Applicative
import Data.IORef
import Data.Digest.Pure.SHA
import Codec.Binary.UTF8.String (encodeString)
import Data.Char
import qualified Codec.Binary.Base64 as B64
import qualified Data.ByteString.Lazy as L

-- OAuth型
data OAuth = OAuth {
      consumerKey :: !(IORef String),
      consumerSecret :: !(IORef String),
      accessToken :: !(IORef String),
      accessTokenSecret :: !(IORef String)
    }

-- Oauth型を生成
newOAuth :: String -> String -> String -> String -> IO OAuth
newOAuth oauthConsumerKey_ oauthConsumerSecret_ oauthAccessToken_ oauthAccessTokenSecret_ = do
  oauthConsumerKey <- newIORef oauthConsumerKey_
  oauthConsumerSecret <- newIORef oauthConsumerSecret_
  oauthAccessToken <- newIORef oauthAccessToken_
  oauthAccessTokenSecret <- newIORef oauthAccessTokenSecret_
  return $ OAuth oauthConsumerKey oauthConsumerSecret oauthAccessToken oauthAccessTokenSecret

-- 各フィールドに書き込み
setConsumerKey :: OAuth -> String -> IO ()
setConsumerKey oauth set = writeIORef (consumerKey oauth) set
setConsumerSecret :: OAuth -> String -> IO ()
setConsumerSecret oauth set = writeIORef (consumerSecret oauth) set
setAccessToken :: OAuth -> String -> IO ()
setAccessToken oauth set = writeIORef (accessToken oauth) set
setAccessTokenSecret :: OAuth -> String -> IO ()
setAccessTokenSecret oauth set = writeIORef (accessTokenSecret oauth) set

-- 各フィールドの読み込み
getConsumerKey :: OAuth -> IO String
getConsumerKey oauth = readIORef (consumerKey oauth)
getConsumerSecret :: OAuth -> IO String
getConsumerSecret oauth = readIORef (consumerSecret oauth)
getAccessToken :: OAuth -> IO String
getAccessToken oauth = readIORef (accessToken oauth)
getAccessTokenSecret :: OAuth -> IO String
getAccessTokenSecret oauth = readIORef (accessTokenSecret oauth)

-- パラメータ型
type Parameter = (String, String)

-- パラメータをパース
parseParameter :: String -> [Parameter]
parseParameter = map splitByEqual . splitByAnd
    where
      splitByAnd str = case span (/= '&') str of
                         (x, "") -> [x]
                         (x, _:xs) -> x : splitByAnd xs
      splitByEqual = second tail . span (/= '=')

-- パラメータのリストから特定のパラメータを取得
getParameter :: Monad m => [Parameter] -> String -> m Parameter
getParameter parameters parameterName =
    case find ((== parameterName) . fst) parameters of
      Just parameter -> return parameter
      Nothing -> fail "Parameter not found."

-- パラメータを '=' で結合し、 ',' 区切りで並べる
urlEncodeParams :: [Parameter] -> String
urlEncodeParams parameters = intercalate ", " . map (\(x, y) -> urlEncode x ++ "=" ++ doubleQuote (urlEncode y)) $ parameters
    where
      doubleQuote str = "\"" ++ str ++ "\""

-- リクエストの署名を生成
genSignature :: String -> String -> RequestMethod -> String -> [Parameter] -> String
genSignature consumerSecret tokenSecret method uri parameters =
    let parameters' = urlEncode . urlEncodeVars . sort $ parameters -- パラメータのソート・結合・エンコード
        signatureKey = L.pack . map (fromIntegral . ord) $ urlEncode consumerSecret ++ "&" ++ urlEncode tokenSecret -- 署名キー生成
        signatureBaseString = L.pack . map (fromIntegral . ord) $ urlEncode (show method) ++ "&" ++ urlEncode uri ++ "&" ++ parameters' -- 署名対象文字列生成
    in
        B64.encode . L.unpack . bytestringDigest $ hmacSha1 signatureKey signatureBaseString -- HMAC-SHA1アルゴリズムでダイジェスト値を生成・Base64でエンコード

-- リクエストトークン取得URL
requestTokenURL = "http://api.twitter.com/oauth/request_token"
authorizeURL = "http://api.twitter.com/oauth/authorize"
-- アクセストークン取得URL
accessTokenURL = "http://api.twitter.com/oauth/access_token"

-- APIリクエストURL
apiRequestURL :: String -> String
apiRequestURL api = "http://api.twitter.com/1/statuses/" ++ api ++ ".json"

-- OAuth Request 生成
oauthRequest :: OAuth -> String -> String -> [Parameter] -> IO Request_String
oauthRequest oauth url token parameter = do
  key <- getConsumerKey oauth -- Consumer key
  secret <- getConsumerSecret oauth -- Consumer Secret
  let uri = fromJust . parseURI $ url -- URI
  timestamp <- show . (\(TOD i _) -> i) <$> getClockTime -- タイムスタンプ取得
  nonce <- show <$> randomRIO (0, maxBound::Int) -- 乱数取得
  let authorizationParameters_ = parameter ++ [
                                          ("oauth_consumer_key", key),
                                          ("oauth_nonce", nonce),
                                          ("oauth_timestamp", timestamp),
                                          ("oauth_signature_method", "HMAC-SHA1"),
                                          ("oauth_version", "1.0")
                                         ] -- 各種基本パラメータをセット
      signature = genSignature secret token POST url authorizationParameters_ -- 署名生成
      authorizationParameters = authorizationParameters_++[("oauth_signature", signature)] -- 署名をパラメータに加える
      authorizationHeader = mkHeader HdrAuthorization . ("OAuth "++) . urlEncodeParams $ authorizationParameters -- Authorizationヘッダ生成
  -- Request を構成
  return $ Request {
               rqURI = uri,
               rqMethod = POST,
               rqHeaders = [authorizationHeader],
               rqBody = ""
             }

-- APIリクエスト
apiRequest :: OAuth -> String -> RequestMethod -> [Parameter] -> IO Request_String
apiRequest oauth api method args = do
  key <- getConsumerKey oauth -- Consumer key
  token <- getAccessToken oauth -- AccessToken
  secret_Consumer <- getConsumerSecret oauth -- Consumer Secret
  secret_AccessToken <- getAccessTokenSecret oauth -- AccessToken Secret
  let url = apiRequestURL api
      uri = fromJust . parseURI $ if method == POST then url else url ++ "?" ++ urlEncodeVars args  -- URI
  timestamp <- show . (\(TOD i _) -> i) <$> getClockTime -- タイムスタンプ取得
  nonce <- show <$> randomRIO (0, maxBound::Int) -- 乱数取得
  let authorizationParameters_ = [
       ("oauth_token", token),
       ("oauth_consumer_key", key),
       ("oauth_nonce", nonce),
       ("oauth_timestamp", timestamp),
       ("oauth_signature_method", "HMAC-SHA1"),
       ("oauth_version", "1.0")] -- 各種基本パラメータをセット
      signature = genSignature secret_Consumer secret_AccessToken method url (args ++ authorizationParameters_) -- 署名生成
      authorizationParameters = authorizationParameters_++[("oauth_signature", signature)] -- 署名をパラメータに加える
      authorizationHeader = mkHeader HdrAuthorization . ("OAuth "++) . urlEncodeParams $ authorizationParameters -- Authorizationヘッダ生成
      contentLengthHeader = mkHeader HdrContentLength (show . length . urlEncodeVars $ args)
  -- Request を構成
  return $ Request {
               rqURI = uri,
               rqMethod = method,
               rqHeaders = if method == POST then [authorizationHeader, contentLengthHeader] else [authorizationHeader] ,
               rqBody = if method == POST then urlEncodeVars args else ""
             }

-- simpleHTTP のIO版
simpleHTTPIO :: HStream a => Request a -> IO (Response a)
simpleHTTPIO req = do
  res <- simpleHTTP req
  case res of
    Right res' -> if rspCode res' == (2, 0, 0) then return res' else fail.show $ res'
    Left err -> fail.show $ err


  