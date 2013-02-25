module Modules.Default (echoCommand, quitCommand, autoVoiceCommand) where

import Text.Parsers.IRC
import Network.IRC.SevenInch
import Network.Socket
import Network.BSD
import Text.Regex.Posix
import System.IO
import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM (STM, atomically)

echoHandler :: TChan String -> IrcMsg -> SocketHandler -> IO ()
echoHandler chan (PubMsg c _ channel msg) cb = do
  let line = drop 6 msg
  atomically $ sendCmd chan PRIVMSG [channelToString channel, line]
  cb

isMsgMatchingRegex :: String -> IrcMsg -> Bool
isMsgMatchingRegex regex (PubMsg _ _ _ msg) = msg =~ regex :: Bool
isMsgMatchingRegex regex (PrivMsg _ _ _ msg) = msg =~ regex :: Bool
isMsgMatchingRegex _ _ = False

isEcho = isMsgMatchingRegex "^!echo .+"
isQuit = isMsgMatchingRegex "^!quit"

quitHandler :: TChan String -> IrcMsg -> SocketHandler -> IO ()
quitHandler chan _ cb = do
  atomically $ sendCmd chan QUIT []
  cb

echoCommand = (isEcho, echoHandler)
quitCommand = (isQuit, quitHandler)

isAutoVoice :: IrcMsg -> Bool
isAutoVoice (JoinPartMsg JOIN _ _) = True
isAutoVoice _ = False

autoVoiceHandler :: TChan String -> IrcMsg -> SocketHandler -> IO ()
autoVoiceHandler chan (JoinPartMsg _ (IrcUser nick _ _) room) cb = do
  let roomStr = channelToString room
  atomically $ sendCmd chan MODE [roomStr, "+v", nick]
  cb

autoVoiceCommand = (isAutoVoice, autoVoiceHandler)

