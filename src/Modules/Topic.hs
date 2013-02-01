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

topicSwapResponse :: MVar (M.Map Channel String) ->
                     M.Map Channel String ->
                     Handle ->
                     (Channel, String, String) ->
                     IO ()

topicSwapResponse topicMapRef topicMap h (chan, oldTopic, newTopic) = do
  putMVar topicMapRef $ M.insert chan newTopic topicMap
  let msg = "Topic was: " ++ oldTopic
  sendCmd h PRIVMSG [channelToString chan, msg]

topicChangeHandler :: MVar (M.Map Channel String) ->
                      Handle ->
                      IrcMsg ->
                      SocketHandler ->
                      IO ()

topicChangeHandler topicMapRef h (TopicMsg (IrcUser nick _ _) chan newTopic) cb = do
  topicMap <- takeMVar topicMapRef
  case M.lookup chan topicMap of
    Just oldTopic -> topicSwapResponse topicMapRef topicMap h (chan, oldTopic, newTopic) >> cb
    Nothing -> putMVar topicMapRef (M.insert chan newTopic topicMap) >> cb

initializeTopicChange :: IO (MsgHandler)
initializeTopicChange = do
  topicMapRef <- newMVar (M.empty :: M.Map Channel String)
  return (isTopicChange, topicChangeHandler topicMapRef)

