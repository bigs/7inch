module Modules.Default (echoCommand, quitCommand, autoVoiceCommand) where

import Text.Parsers.IRC
import Network.IRC.SevenInch
import Network.Socket
import Network.BSD
import Text.Regex.Posix
import System.IO

echoHandler :: Handle -> IrcMsg -> SocketHandler -> IO ()
echoHandler h (PubMsg c _ channel msg) cb = do
  let line = drop 6 msg
  sendCmd h PRIVMSG [channelToString channel, line]
  cb

isMsgMatchingRegex :: String -> IrcMsg -> Bool
isMsgMatchingRegex regex (PubMsg _ _ _ msg) = msg =~ regex :: Bool
isMsgMatchingRegex regex (PrivMsg _ _ _ msg) = msg =~ regex :: Bool
isMsgMatchingRegex _ _ = False

isEcho = isMsgMatchingRegex "^!echo .+"
isQuit = isMsgMatchingRegex "^!quit"

quitHandler :: Handle -> IrcMsg -> SocketHandler -> IO ()
quitHandler h _ cb = do
  sendCmd h QUIT []
  cb

echoCommand = (isEcho, echoHandler)
quitCommand = (isQuit, quitHandler)

isAutoVoice :: IrcMsg -> Bool
isAutoVoice (JoinPartMsg JOIN _ _) = True
isAutoVoice _ = False

autoVoiceHandler :: Handle -> IrcMsg -> SocketHandler -> IO ()
autoVoiceHandler h (JoinPartMsg _ (IrcUser nick _ _) room) cb = do
  let roomStr = channelToString room
  sendCmd h MODE [roomStr, "+v", nick]
  cb

autoVoiceCommand = (isAutoVoice, autoVoiceHandler)

