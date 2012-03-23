{-# LANGUAGE TupleSections #-}

-- module OAuth  where

import Network.HTTP
import Network.URI
import Data.Maybe
import Data.List
import System.IO
import System.Random
import System.Time
import Control.Arrow
import Control.Applicative
import Data.Digest.Pure.SHA
import Codec.Binary.UTF8.String (encodeString)
import Data.Char
import qualified Codec.Binary.Base64 as B64
import qualified Data.ByteString.Lazy as L

-- OAuth型
data OAuth = OAuth {
      consumerKey :: String,
      consumerSecret :: String,
      accessToken :: String,
      accessTokenSecret :: String
    }
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
    let key = consumerKey oauth -- Consumer key
        secret = consumerSecret oauth -- Consumer Secret
        uri = fromJust . parseURI $ url -- URI
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
    let key = consumerKey oauth -- Consumer key
        token = accessToken oauth -- AccessToken
        secret_Consumer = consumerSecret oauth -- Consumer Secret
        secret_AccessToken = accessTokenSecret oauth -- AccessToken Secret
        url = apiRequestURL api
        uri = fromJust . parseURI $ url -- ++ "?" ++ urlEncodeVars args  -- URI
    timestamp <- show . (\(TOD i _) -> i) <$> getClockTime -- タイムスタンプ取得
    nonce <- show <$> randomRIO (0, maxBound::Int) -- 乱数取得
    let authorizationParameters_ = [
                                    ("oauth_token", token),
                                    ("oauth_consumer_key", key),
                                    ("oauth_nonce", nonce),
                                    ("oauth_timestamp", timestamp),
                                    ("oauth_signature_method", "HMAC-SHA1"),
                                    ("oauth_version", "1.0")
                                   ] -- 各種基本パラメータをセット
        signature = genSignature secret_Consumer secret_AccessToken method url (args ++ authorizationParameters_) -- 署名生成
        authorizationParameters = authorizationParameters_++[("oauth_signature", signature)] -- 署名をパラメータに加える
        authorizationHeader = mkHeader HdrAuthorization . ("OAuth "++) . urlEncodeParams $ authorizationParameters -- Authorizationヘッダ生成
        contentLengthHeader = mkHeader HdrContentLength (show . length . urlEncodeVars $ args)
    -- Request を構成
    return $ Request {
                 rqURI = uri,
                 rqMethod = method,
                 rqHeaders = [authorizationHeader, contentLengthHeader],
                 rqBody = if method == POST then urlEncodeVars args else ""
               }

-- simpleHTTP のIO版
simpleHTTPIO :: HStream a => Request a -> IO (Response a)
simpleHTTPIO req = do
  res <- simpleHTTP req
  case res of
    Right res' -> if rspCode res' == (2, 0, 0) then return res' else fail.show $ res'
    Left err -> fail.show $ err

-- ツイートする(Ctrl+Cで抜ける)
main_loop :: OAuth -> IO ()
main_loop oauth = do
  content <- getLine
  tweet <- apiRequest oauth "update" POST [("status", encodeString content)]
  res <- simpleHTTPIO tweet
  main_loop oauth

getAccessToken :: String -> String -> IO (Parameter, Parameter)
getAccessToken consumerKey consumerSecret = do
  putStrLn "Access Token is not found."
  let oauth_ = OAuth consumerKey consumerSecret "" ""
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
  

restoreAccessToken :: IO (Parameter, Parameter)
restoreAccessToken = do
  -- アクセストークン読み込み
  fin <- openFile "./access.ini" ReadMode
  accessToken <- hGetLine fin
  accessTokenSecret <- hGetLine fin
  hClose fin
  putStrLn "Access Token successfully restored."
  return (("oauth_token", accessToken), ("oauth_token_secret", accessTokenSecret))

main :: IO ()
main = do
  -- Consumer Key / Consumer Secret読み込み
  fin <- openFile "./config.ini" ReadMode
  consumerKey <- hGetLine fin
  consumerSecret <- hGetLine fin
  hClose fin
  -- アクセストークン取得
  (accessToken, accessTokenSecret) <- restoreAccessToken `catch` \_ -> getAccessToken consumerKey consumerSecret
  let oauth = OAuth consumerKey consumerSecret (snd accessToken) (snd accessTokenSecret)
  main_loop oauth
  