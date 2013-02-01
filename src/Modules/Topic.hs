module Modules.Topic (initializeTopicChange) where

import Text.Parsers.IRC
import Network.IRC.SevenInch
import Network.Socket
import Network.BSD
import Text.Regex.Posix
import System.IO
import Control.Concurrent.MVar
import qualified Data.Map as M

isTopicChange :: IrcMsg -> Bool
isTopicChange (TopicMsg _ _ _) = True
isTopicChange _ = False

topicSwap :: Channel ->
             String ->
             M.Map Channel String ->
             IO (M.Map Channel String, Maybe String)

topicSwap chan newTopic topicMap = do
  return (M.insert chan newTopic topicMap, M.lookup chan topicMap)

topicChangeHandler :: MVar (M.Map Channel String) ->
                      Handle ->
                      IrcMsg ->
                      SocketHandler ->
                      IO ()

topicChangeHandler topicMapRef h (TopicMsg (IrcUser nick _ _) chan newTopic) cb = do
  oldTopic <- modifyMVar topicMapRef (topicSwap chan newTopic)
  case oldTopic of
    Just topic -> sendCmd h PRIVMSG [channelToString chan, "Topic was: " ++ topic] >> cb
    Nothing -> cb

initializeTopicChange :: IO (MsgHandler)
initializeTopicChange = do
  topicMapRef <- newMVar (M.empty :: M.Map Channel String)
  return (isTopicChange, topicChangeHandler topicMapRef)

