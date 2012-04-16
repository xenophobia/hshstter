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

-- OAuth��
data OAuth = OAuth {
      consumerKey :: !String,
      consumerSecret :: !String,
      accessToken :: !String,
      accessTokenSecret :: !String
    }

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
    where
      doubleQuote str = "\"" ++ str ++ "\""

-- �ꥯ�����Ȥν�̾������
genSignature :: String -> String -> RequestMethod -> String -> [Parameter] -> String
genSignature consumerSecret tokenSecret method uri parameters =
    let parameters' = urlEncode . urlEncodeVars . sort $ parameters -- �ѥ�᡼���Υ����ȡ���硦���󥳡���
        signatureKey = L.pack . map (fromIntegral . ord) $ urlEncode consumerSecret ++ "&" ++ urlEncode tokenSecret -- ��̾��������
        signatureBaseString = L.pack . map (fromIntegral . ord) $ urlEncode (show method) ++ "&" ++ urlEncode uri ++ "&" ++ parameters' -- ��̾�о�ʸ��������
    in
        B64.encode . L.unpack . bytestringDigest $ hmacSha1 signatureKey signatureBaseString -- HMAC-SHA1���르�ꥺ��ǥ������������ͤ�������Base64�ǥ��󥳡���

-- �ꥯ�����ȥȡ��������URL
requestTokenURL = "http://api.twitter.com/oauth/request_token"
-- ǧ�ڥڡ���URL
authorizeURL = "http://api.twitter.com/oauth/authorize"
-- ���������ȡ��������URL
accessTokenURL = "http://api.twitter.com/oauth/access_token"
-- API�ꥯ������URL
apiRequestURL :: String -> String
apiRequestURL api = "http://api.twitter.com/1/statuses/" ++ api ++ ".json"

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
      authorizationHeader = mkHeader HdrAuthorization . ("OAuth "++) . urlEncodeParams $ authorizationParameters -- Authorization�إå�����
  -- Request������
  rspBody <$> (simpleHTTPIO $ Request {
                                  rqURI = fromJust . parseURI $ url,
                                  rqMethod = POST,
                                  rqHeaders = [authorizationHeader],
                                  rqBody = ""
                                })

-- API�ꥯ������
apiRequest :: OAuth -> String -> RequestMethod -> [Parameter] -> IO String
apiRequest oauth api method args = do
  let url = apiRequestURL api
      uri = fromJust . parseURI $ if method == POST then url else url ++ "?" ++ urlEncodeVars args  -- URI
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
      authorizationHeader = mkHeader HdrAuthorization . ("OAuth "++) . urlEncodeParams $ authorizationParameters -- Authorization�إå�����
      contentLengthHeader = mkHeader HdrContentLength (show . length . urlEncodeVars $ args)
  -- Request������
  rspBody <$> (simpleHTTPIO $ Request {
                                  rqURI = uri,
                                  rqMethod = method,
                                  rqHeaders = if method == POST then [authorizationHeader, contentLengthHeader] else [authorizationHeader] ,
                                  rqBody = if method == POST then urlEncodeVars args else ""
                                })

-- simpleHTTP ��IO��
simpleHTTPIO :: HStream a => Request a -> IO (Response a)
simpleHTTPIO req = do
  res <- simpleHTTP req
  case res of
    Right res' -> if rspCode res' == (2, 0, 0) then return res' else fail.show $ res'
    Left err -> fail.show $ err