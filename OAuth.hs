module OAuth  where

import Network.HTTP
import Network.URI
import Network.Curl
import Data.Maybe
import Data.List
import System.IO
import System.Random
import System.Time
import Control.Monad
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
      consumerKey :: !String,
      consumerSecret :: !String,
      accessToken :: !String,
      accessTokenSecret :: !String,
      user_id :: !String,
      screen_name :: !String,
      curl :: !Curl
    }

newOAuth :: String -> String -> String -> String -> String -> String -> IO OAuth
newOAuth oauthConsumerKey oauthConsumerSecret oauthAccessToken oauthAccessTokenSecret oauthUser_id oauthScreen_name =
    (OAuth oauthConsumerKey oauthConsumerSecret oauthAccessToken oauthAccessTokenSecret oauthUser_id oauthScreen_name) <$> initialize

getCurl :: OAuth -> IO Curl
getCurl oauth = do
  let oauthCurl = curl oauth
  reset oauthCurl
  return oauthCurl

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
    where doubleQuote str = "\"" ++ str ++ "\""

-- リクエストの署名を生成
genSignature :: String -> String -> RequestMethod -> String -> [Parameter] -> String
genSignature consumerSecret tokenSecret method uri parameters =
    let parameters' = urlEncode . urlEncodeVars . sort $ parameters -- パラメータのソート・結合・エンコード
        signatureKey = L.pack . map (fromIntegral . ord) $ urlEncode consumerSecret ++ "&" ++ urlEncode tokenSecret -- 署名キー生成
        signatureBaseString = L.pack . map (fromIntegral . ord) $ urlEncode (show method) ++ "&" ++ urlEncode uri ++ "&" ++ parameters' -- 署名対象文字列生成
    in
        B64.encode . L.unpack . bytestringDigest $ hmacSha1 signatureKey signatureBaseString -- HMAC-SHA1アルゴリズムでダイジェスト値を生成・Base64でエンコード

-- リクエストトークン取得URL
requestTokenURL = "https://api.twitter.com/oauth/request_token"
-- 認証ページURL
authorizeURL :: Parameter -> String
authorizeURL requestToken = "https://api.twitter.com/oauth/authorize" ++ "?" ++ urlEncodeVars [requestToken]
-- アクセストークン取得URL
accessTokenURL = "https://api.twitter.com/oauth/access_token"
-- APIリクエストURL
apiRequestURL :: String -> String
apiRequestURL api = "https://api.twitter.com" ++ api ++ ".json"

-- OAuth Request 生成
oauthRequest :: OAuth -> String -> String -> [Parameter] -> IO String
oauthRequest oauth url token parameter = do
  timestamp <- show . (\(TOD i _) -> i) <$> getClockTime -- タイムスタンプ取得
  nonce <- show <$> randomRIO (0, maxBound::Int) -- 乱数取得
  let authorizationParameters_ = parameter ++ [
                                          ("oauth_consumer_key", consumerKey oauth),
                                          ("oauth_nonce", nonce),
                                          ("oauth_timestamp", timestamp),
                                          ("oauth_signature_method", "HMAC-SHA1"),
                                          ("oauth_version", "1.0")
                                         ] -- 各種基本パラメータをセット
      signature = genSignature (consumerSecret oauth) token POST url authorizationParameters_ -- 署名生成
      authorizationParameters = authorizationParameters_++[("oauth_signature", signature)] -- 署名をパラメータに加える
      authorizationHeader = ("Authorization: OAuth "++) . urlEncodeParams $ authorizationParameters -- Authorizationヘッダ生成
      contentLengthHeader = "Content-Length: 0"
  -- Curlインスタンス初期化・オプションをセット
  curl <- initialize
  setopts curl [CurlHttpHeaders [authorizationHeader, contentLengthHeader],
                CurlPostFieldSize 0,
                CurlPost True]
  -- Requestを送信
  respBody <$> (do_curl_ curl url [] :: IO (CurlResponse_ [(String, String)] String))

-- APIリクエスト
apiRequest :: OAuth -> String -> RequestMethod -> [Parameter] -> IO String
apiRequest oauth api method args = do
  let url = apiRequestURL api
      accessurl = if method == POST then url else url ++ "?" ++ urlEncodeVars args  -- URI
  timestamp <- show . (\(TOD i _) -> i) <$> getClockTime -- タイムスタンプ取得
  nonce <- show <$> randomRIO (0, maxBound::Int) -- 乱数取得
  let authorizationParameters_ = [
       ("oauth_token", accessToken oauth),
       ("oauth_consumer_key", consumerKey oauth),
       ("oauth_nonce", nonce),
       ("oauth_timestamp", timestamp),
       ("oauth_signature_method", "HMAC-SHA1"),
       ("oauth_version", "1.0")] -- 各種基本パラメータをセット
      signature = genSignature (consumerSecret oauth) (accessTokenSecret oauth) method url (args ++ authorizationParameters_) -- 署名生成
      authorizationParameters = authorizationParameters_++[("oauth_signature", signature)] -- 署名をパラメータに加える
      authorizationHeader = ("Authorization: OAuth "++) . urlEncodeParams $ authorizationParameters -- Authorizationヘッダ生成
      postFields = map (\(x, y) -> urlEncode x ++ "=" ++ urlEncode y) args
      contentLength = length . urlEncodeVars $ args
      contentLengthHeader = "Content-Length: " ++ show contentLength
      htmlHeaders = if method==POST then [authorizationHeader, contentLengthHeader] else [authorizationHeader]
  -- Curlオブジェクトを初期化
  curl <- getCurl oauth
  -- オプションをセット
  setopts curl [CurlHttpHeaders htmlHeaders]
  when (method == POST) $ setopts curl [CurlPostFieldSize (fromIntegral contentLength),
                                        CurlPost True,
                                        CurlPostFields postFields]
  -- Requestを送信
  respBody <$> (do_curl_ curl accessurl [] :: IO (CurlResponse_ [(String, String)] String))
