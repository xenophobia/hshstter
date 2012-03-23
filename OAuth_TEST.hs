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

-- OAuth��
data OAuth = OAuth {
      consumerKey :: String,
      consumerSecret :: String,
      accessToken :: String,
      accessTokenSecret :: String
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
authorizeURL = "http://api.twitter.com/oauth/authorize"
-- ���������ȡ��������URL
accessTokenURL = "http://api.twitter.com/oauth/access_token"

-- API�ꥯ������URL
apiRequestURL :: String -> String
apiRequestURL api = "http://api.twitter.com/1/statuses/" ++ api ++ ".json"

-- OAuth Request ����
oauthRequest :: OAuth -> String -> String -> [Parameter] -> IO Request_String
oauthRequest oauth url token parameter = do
    let key = consumerKey oauth -- Consumer key
        secret = consumerSecret oauth -- Consumer Secret
        uri = fromJust . parseURI $ url -- URI
    timestamp <- show . (\(TOD i _) -> i) <$> getClockTime -- �����ॹ����׼���
    nonce <- show <$> randomRIO (0, maxBound::Int) -- �������
    let authorizationParameters_ = parameter ++ [
                                ("oauth_consumer_key", key),
                                ("oauth_nonce", nonce),
                                ("oauth_timestamp", timestamp),
                                ("oauth_signature_method", "HMAC-SHA1"),
                                ("oauth_version", "1.0")
                               ] -- �Ƽ���ܥѥ�᡼���򥻥å�
        signature = genSignature secret token POST url authorizationParameters_ -- ��̾����
        authorizationParameters = authorizationParameters_++[("oauth_signature", signature)] -- ��̾��ѥ�᡼���˲ä���
        authorizationHeader = mkHeader HdrAuthorization . ("OAuth "++) . urlEncodeParams $ authorizationParameters -- Authorization�إå�����
    -- Request ����
    return $ Request {
                 rqURI = uri,
                 rqMethod = POST,
                 rqHeaders = [authorizationHeader],
                 rqBody = ""
               }

-- API�ꥯ������
apiRequest :: OAuth -> String -> RequestMethod -> [Parameter] -> IO Request_String
apiRequest oauth api method args = do
    let key = consumerKey oauth -- Consumer key
        token = accessToken oauth -- AccessToken
        secret_Consumer = consumerSecret oauth -- Consumer Secret
        secret_AccessToken = accessTokenSecret oauth -- AccessToken Secret
        url = apiRequestURL api
        uri = fromJust . parseURI $ url -- ++ "?" ++ urlEncodeVars args  -- URI
    timestamp <- show . (\(TOD i _) -> i) <$> getClockTime -- �����ॹ����׼���
    nonce <- show <$> randomRIO (0, maxBound::Int) -- �������
    let authorizationParameters_ = [
                                    ("oauth_token", token),
                                    ("oauth_consumer_key", key),
                                    ("oauth_nonce", nonce),
                                    ("oauth_timestamp", timestamp),
                                    ("oauth_signature_method", "HMAC-SHA1"),
                                    ("oauth_version", "1.0")
                                   ] -- �Ƽ���ܥѥ�᡼���򥻥å�
        signature = genSignature secret_Consumer secret_AccessToken method url (args ++ authorizationParameters_) -- ��̾����
        authorizationParameters = authorizationParameters_++[("oauth_signature", signature)] -- ��̾��ѥ�᡼���˲ä���
        authorizationHeader = mkHeader HdrAuthorization . ("OAuth "++) . urlEncodeParams $ authorizationParameters -- Authorization�إå�����
        contentLengthHeader = mkHeader HdrContentLength (show . length . urlEncodeVars $ args)
    -- Request ����
    return $ Request {
                 rqURI = uri,
                 rqMethod = method,
                 rqHeaders = [authorizationHeader, contentLengthHeader],
                 rqBody = if method == POST then urlEncodeVars args else ""
               }

-- simpleHTTP ��IO��
simpleHTTPIO :: HStream a => Request a -> IO (Response a)
simpleHTTPIO req = do
  res <- simpleHTTP req
  case res of
    Right res' -> if rspCode res' == (2, 0, 0) then return res' else fail.show $ res'
    Left err -> fail.show $ err

-- �ĥ����Ȥ���(Ctrl+C��ȴ����)
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
  -- �ꥯ�����ȥȡ�����ȯ���׵�ꥯ����������
  requestForGetRequestToken <- oauthRequest oauth_ requestTokenURL "" []
  -- �ꥯ�����ȥȡ��������
  requestTokenParameters <- (fmap $ parseParameter . rspBody) . simpleHTTPIO $ requestForGetRequestToken
  requestToken <- getParameter requestTokenParameters "oauth_token"
  requestTokenSecret <- getParameter requestTokenParameters "oauth_token_secret"
  -- ǧ�ڥڡ����Υ��ɥ쥹ɽ��
  putStrLn $ authorizeURL ++ "?" ++ urlEncodeVars [requestToken]
  -- PIN���� -> oauth_verifier�ѥ�᡼���Ȥ���«��
  verifier <- ("oauth_varifier",) <$> getLine
  -- ���������ȡ�����ȯ���׵�ꥯ����������
  requestForGetAccessToken <- oauthRequest oauth_ accessTokenURL (snd requestTokenSecret) [requestToken, verifier]
  -- ���������ȡ��������
  accessTokenParameters <- (fmap $ parseParameter . rspBody) . simpleHTTPIO $ requestForGetAccessToken
  accessToken <- getParameter accessTokenParameters "oauth_token"
  accessTokenSecret <- getParameter accessTokenParameters "oauth_token_secret"
  -- ���������ȡ������ݻ��ե�����access.ini�˥��������ȡ�����򥻡���
  fout <- openFile "./access.ini" WriteMode
  hPutStrLn fout (snd accessToken)
  hPutStrLn fout (snd accessTokenSecret)
  hClose fout
  return (accessToken, accessTokenSecret)
  

restoreAccessToken :: IO (Parameter, Parameter)
restoreAccessToken = do
  -- ���������ȡ������ɤ߹���
  fin <- openFile "./access.ini" ReadMode
  accessToken <- hGetLine fin
  accessTokenSecret <- hGetLine fin
  hClose fin
  putStrLn "Access Token successfully restored."
  return (("oauth_token", accessToken), ("oauth_token_secret", accessTokenSecret))

main :: IO ()
main = do
  -- Consumer Key / Consumer Secret�ɤ߹���
  fin <- openFile "./config.ini" ReadMode
  consumerKey <- hGetLine fin
  consumerSecret <- hGetLine fin
  hClose fin
  -- ���������ȡ��������
  (accessToken, accessTokenSecret) <- restoreAccessToken `catch` \_ -> getAccessToken consumerKey consumerSecret
  let oauth = OAuth consumerKey consumerSecret (snd accessToken) (snd accessTokenSecret)
  main_loop oauth
  