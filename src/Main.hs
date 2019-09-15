{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Console.GetOpt
import System.Environment (getArgs)

import Network.Wreq (get, getWith, defaults, auth, oauth2Bearer, responseBody)
import Control.Lens
import Control.Concurrent

import Data.Aeson
import Data.Aeson.Lens (key, nth, _String, values, _Integer)

import Data.Text (Text, stripSuffix, unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Char (isSpace)

import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM

data Flags = Flags
  { token :: Text
  , domain :: Text
  , record :: Text
  } deriving Show

defaultFlags = Flags
  { token = ""
  , domain = ""
  , record = ""
  }

options :: [OptDescr (Flags -> Flags)]
options =
  [ Option ['t'] ["token"]  (ReqArg (\t opts -> opts { token = pack t })  "TOKEN")  "DigitalOcean API token"
  , Option ['r'] ["record"] (ReqArg (\r opts -> opts { record = pack r }) "RECORD") "Name of A record"
  , Option ['d'] ["domain"] (ReqArg (\d opts -> opts { domain = pack d }) "DOMAIN") "Name of domain"
  ]

ddnsOpts :: [String] -> IO (Flags, [String])
ddnsOpts argv =
  case getOpt Permute options argv of
         (o,n,[]  ) -> return (foldl (flip id) defaultFlags o, n)
         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
   where header = "Usage: ddns -t <api_token> -r <record_name> -d <domain_name>"

main :: IO ()
main = do
    args <- getArgs
    (flags, _) <- ddnsOpts args
    let t = token flags
        r = record flags
        d = domain flags
    loop t r d

loop :: Text -> Text -> Text -> IO ()
loop t r d = do
  dynIp <- publicIp
  record <- recordByName r d t
  print record
  print dynIp
  threadDelay 5000000
  loop t r d

data DnsRecord = DnsRecord { recordId :: Integer
                           , ip :: Text
                           } deriving Show

recordByName :: Text -> Text -> Text -> IO (Maybe DnsRecord)
recordByName n d t = do
  let opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 t)
  resp <- getWith opts $ "https://api.digitalocean.com/v2/domains/" ++ unpack d ++ "/records"
  let body = resp ^. responseBody
  return $ filterDnsRecords n body


filterDnsRecords :: Text -> BL.ByteString -> Maybe DnsRecord
filterDnsRecords n r = do
  let record = r ^.. key "domain_records" . values . filtered (has (key "name"._String.only n))
      ip =  record ^? traverse . key "data" . _String
      id = record ^? traverse . key "id" . _Integer
  return DnsRecord <*> id <*> ip


publicIp :: IO (Maybe Text)
publicIp = do
  resp <- get "http://checkip.amazonaws.com/"
  let body = resp ^. responseBody
  return $ stripSuffix "\n" (toStrict . decodeUtf8 $ body)
