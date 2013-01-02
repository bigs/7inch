module Modules.Default (echoCommand, quitCommand, autoVoiceCommand) where

import Text.Parsers.IRC
import Network.IRC.SevenInch
import Network.Socket
import Network.BSD
import Text.Regex.Posix
import System.IO

echoHandler :: Handle -> IrcMsg -> IO ()
echoHandler h (PubMsg c _ channel msg) = do
  let line = drop 6 msg
  sendCmd h PRIVMSG [channelToString channel, line]

isMsgMatchingRegex :: String -> IrcMsg -> Bool
isMsgMatchingRegex regex (PubMsg _ _ _ msg) = msg =~ regex :: Bool
isMsgMatchingRegex regex (PrivMsg _ _ _ msg) = msg =~ regex :: Bool
isMsgMatchingRegex _ _ = False

isEcho = isMsgMatchingRegex "^!echo .+"
isQuit = isMsgMatchingRegex "^!quit"

quitHandler :: Handle -> IrcMsg -> IO ()
quitHandler h _ = do
  sendCmd h QUIT []

echoCommand = (isEcho, echoHandler)
quitCommand = (isQuit, quitHandler)

isAutoVoice :: IrcMsg -> Bool
isAutoVoice (JoinPartMsg JOIN _ _) = True
isAutoVoice _ = False

autoVoiceHandler :: Handle -> IrcMsg -> IO ()
autoVoiceHandler h (JoinPartMsg _ (IrcUser nick _ _) room) = do
  let roomStr = channelToString room
  sendCmd h MODE [roomStr, "+v", nick]

autoVoiceCommand = (isAutoVoice, autoVoiceHandler)

