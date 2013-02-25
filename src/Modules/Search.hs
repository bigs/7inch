module Modules.Search where

import Network.HTTP
import Network.HTTP.Base
import Network.URI
import Data.Aeson
import Control.Monad (mzero)
import Control.Applicative ((<*>), (<$>))
import Data.Attoparsec hiding (try)
import System.Environment (getEnv)
import Control.Exception (try)
import System.IO.Error hiding (try)
import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM (atomically)

import Text.Parsers.IRC
import Network.IRC.SevenInch
import Network.Socket
import System.IO
import Network.BSD
import Text.Regex.Posix ((=~))
import Text.Regex

import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson.Types as T
import qualified Data.Text as TXT

data Definition = Definition {
  textProns :: [String],
  sourceDictionary :: String,
  exampleUses :: [String],
  relatedWords :: [String],
  labels :: [String],
  citations :: [String],
  word :: String,
  text :: String,
  defSequence :: String,
  score :: Float,
  partOfSpeech :: String,
  attributionText :: String
} deriving (Eq, Show)

instance FromJSON Definition where
  parseJSON (Object v) = Definition <$> v .: TXT.pack "textProns"
                                    <*> v .: TXT.pack "sourceDictionary"
                                    <*> v .: TXT.pack "exampleUses"
                                    <*> v .: TXT.pack "relatedWords"
                                    <*> v .: TXT.pack "labels"
                                    <*> v .: TXT.pack "citations"
                                    <*> v .: TXT.pack "word"
                                    <*> v .: TXT.pack "text"
                                    <*> v .: TXT.pack "sequence"
                                    <*> v .: TXT.pack "score"
                                    <*> v .: TXT.pack "partOfSpeech"
                                    <*> v .: TXT.pack "attributionText"
  parseJSON _ = mzero

openURL :: Request String -> IO (String)
openURL req = getResponseBody =<< simpleHTTP req

readEnv :: String -> IO (Either IOError String)
readEnv var = try $ getEnv var

apiKey :: IO (String)
apiKey = do
  var <- readEnv "WORDNIK_KEY"
  case var of
    Left _ -> return ""
    Right key -> return key

wordnikUrl :: String -> URI
wordnikUrl q = URI "http:"
                   (Just $ URIAuth "" "api.wordnik.com" "")
                   ("/v4/word.json/" ++ q ++ "/definitions")
                   ""
                   "" 

wordnikRequest :: String -> IO (Request String)
wordnikRequest q = do
  let uri = wordnikUrl q
  key <- apiKey
  return $ Request uri GET [Header (HdrCustom "api_key") key] ""

parseDefinition :: String -> Maybe [Definition]
parseDefinition s = let bs = BS.pack s
  in case parse json bs of
      (Data.Attoparsec.Done _ r) -> T.parseMaybe parseJSON r :: Maybe [Definition]
      _                          -> Nothing

lookupWord :: String -> IO (Maybe String)
lookupWord q = do
  req <- wordnikRequest q
  resp <- openURL req
  case parseDefinition resp of
    Just (x:xs) -> return $ Just $ text x
    _           -> return Nothing

isSearchCommand :: IrcMsg -> Bool
isSearchCommand (PubMsg _ _ _ msg) = (msg =~ "^!d(efine)? [a-zA-Z-]+$") :: Bool
isSearchCommand _ = False

searchHandler :: TChan String -> IrcMsg -> SocketHandler -> IO ()
searchHandler chan (PubMsg msgType _ c msg) cb = do
  let reg = mkRegex "^!d(efine)? ([a-zA-Z-]+)$"
  case (matchRegex reg msg) of
    Just [_, word] -> sendWordResponse (atomically . sendCmd chan msgType) c word >> cb
    Nothing -> cb

sendWordResponse :: ([String] -> IO ()) -> Channel -> String -> IO ()
sendWordResponse cmd c word = do
  definition <- lookupWord word
  let response = case definition of
                    Just def -> word ++ ": " ++ def
                    Nothing  -> "Error: Invalid response"
  cmd [channelToString c, response]

searchCommand = (isSearchCommand, searchHandler)

