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

-- OAuth��
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

-- �ѥ�᡼����
type Parameter = (String, String)

-- �ѥ�᡼����ѡ���
parseParameter :: String -> [Parameter]
parseParameter = map splitByEqual . splitByAnd
    where
      splitByAnd str = case span (/= '&') str of
                         (x, "") -> [x]
                         (x, _:xs) -> x : splitByAnd xs
      splitByEqual = second tail . span (/= '=')

-- �ѥ�᡼���Υꥹ�Ȥ�������Υѥ�᡼�������
getParameter :: Monad m => [Parameter] -> String -> m Parameter
getParameter parameters parameterName =
    case find ((== parameterName) . fst) parameters of
      Just parameter -> return parameter
      Nothing -> fail "Parameter not found."

-- �ѥ�᡼���� '=' �Ƿ�礷�� ',' ���ڤ���¤٤�
urlEncodeParams :: [Parameter] -> String
urlEncodeParams parameters = intercalate ", " . map (\(x, y) -> urlEncode x ++ "=" ++ doubleQuote (urlEncode y)) $ parameters
    where doubleQuote str = "\"" ++ str ++ "\""

-- �ꥯ�����Ȥν�̾������
genSignature :: String -> String -> RequestMethod -> String -> [Parameter] -> String
genSignature consumerSecret tokenSecret method uri parameters =
    let parameters' = urlEncode . urlEncodeVars . sort $ parameters -- �ѥ�᡼���Υ����ȡ���硦���󥳡���
        signatureKey = L.pack . map (fromIntegral . ord) $ urlEncode consumerSecret ++ "&" ++ urlEncode tokenSecret -- ��̾��������
        signatureBaseString = L.pack . map (fromIntegral . ord) $ urlEncode (show method) ++ "&" ++ urlEncode uri ++ "&" ++ parameters' -- ��̾�о�ʸ��������
    in
        B64.encode . L.unpack . bytestringDigest $ hmacSha1 signatureKey signatureBaseString -- HMAC-SHA1���르�ꥺ��ǥ������������ͤ�������Base64�ǥ��󥳡���

-- �ꥯ�����ȥȡ��������URL
requestTokenURL = "https://api.twitter.com/oauth/request_token"
-- ǧ�ڥڡ���URL
authorizeURL :: Parameter -> String
authorizeURL requestToken = "https://api.twitter.com/oauth/authorize" ++ "?" ++ urlEncodeVars [requestToken]
-- ���������ȡ��������URL
accessTokenURL = "https://api.twitter.com/oauth/access_token"
-- API�ꥯ������URL
apiRequestURL :: String -> String
apiRequestURL api = "https://api.twitter.com" ++ api ++ ".json"

-- OAuth Request ����
oauthRequest :: OAuth -> String -> String -> [Parameter] -> IO String
oauthRequest oauth url token parameter = do
  timestamp <- show . (\(TOD i _) -> i) <$> getClockTime -- �����ॹ����׼���
  nonce <- show <$> randomRIO (0, maxBound::Int) -- �������
  let authorizationParameters_ = parameter ++ [
                                          ("oauth_consumer_key", consumerKey oauth),
                                          ("oauth_nonce", nonce),
                                          ("oauth_timestamp", timestamp),
                                          ("oauth_signature_method", "HMAC-SHA1"),
                                          ("oauth_version", "1.0")
                                         ] -- �Ƽ���ܥѥ�᡼���򥻥å�
      signature = genSignature (consumerSecret oauth) token POST url authorizationParameters_ -- ��̾����
      authorizationParameters = authorizationParameters_++[("oauth_signature", signature)] -- ��̾��ѥ�᡼���˲ä���
      authorizationHeader = ("Authorization: OAuth "++) . urlEncodeParams $ authorizationParameters -- Authorization�إå�����
      contentLengthHeader = "Content-Length: 0"
  -- Curl���󥹥��󥹽���������ץ����򥻥å�
  curl <- initialize
  setopts curl [CurlHttpHeaders [authorizationHeader, contentLengthHeader],
                CurlPostFieldSize 0,
                CurlPost True]
  -- Request������
  respBody <$> (do_curl_ curl url [] :: IO (CurlResponse_ [(String, String)] String))

-- API�ꥯ������
apiRequest :: OAuth -> String -> RequestMethod -> [Parameter] -> IO String
apiRequest oauth api method args = do
  let url = apiRequestURL api
      accessurl = if method == POST then url else url ++ "?" ++ urlEncodeVars args  -- URI
  timestamp <- show . (\(TOD i _) -> i) <$> getClockTime -- �����ॹ����׼���
  nonce <- show <$> randomRIO (0, maxBound::Int) -- �������
  let authorizationParameters_ = [
       ("oauth_token", accessToken oauth),
       ("oauth_consumer_key", consumerKey oauth),
       ("oauth_nonce", nonce),
       ("oauth_timestamp", timestamp),
       ("oauth_signature_method", "HMAC-SHA1"),
       ("oauth_version", "1.0")] -- �Ƽ���ܥѥ�᡼���򥻥å�
      signature = genSignature (consumerSecret oauth) (accessTokenSecret oauth) method url (args ++ authorizationParameters_) -- ��̾����
      authorizationParameters = authorizationParameters_++[("oauth_signature", signature)] -- ��̾��ѥ�᡼���˲ä���
      authorizationHeader = ("Authorization: OAuth "++) . urlEncodeParams $ authorizationParameters -- Authorization�إå�����
      postFields = map (\(x, y) -> urlEncode x ++ "=" ++ urlEncode y) args
      contentLength = length . urlEncodeVars $ args
      contentLengthHeader = "Content-Length: " ++ show contentLength
      htmlHeaders = if method==POST then [authorizationHeader, contentLengthHeader] else [authorizationHeader]
  -- Curl���֥������Ȥ�����
  curl <- getCurl oauth
  -- ���ץ����򥻥å�
  setopts curl [CurlHttpHeaders htmlHeaders]
  when (method == POST) $ setopts curl [CurlPostFieldSize (fromIntegral contentLength),
                                        CurlPost True,
                                        CurlPostFields postFields]
  -- Request������
  respBody <$> (do_curl_ curl accessurl [] :: IO (CurlResponse_ [(String, String)] String))
