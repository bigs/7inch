module Main where

import Text.Parsers.IRC
import Network.Socket
import Network.BSD
import System.IO
import System.Environment
import Control.Concurrent
import Data.List (isPrefixOf)
import Text.Regex.Posix
import Network.IRC.SevenInch

-- Config stuff
channels = ["#test", "#room"]
botNick = ["clbt"]
botUser = ["clbt", "clbt", "clbt", "clbt"]

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

commands = [(isEcho, echoHandler), (isQuit, quitHandler)]
  
main :: IO ()
main = do
  [server, port] <- getArgs
  putStrLn $ "Connecting to " ++ server ++ ":" ++ port
  h <- initSocket server port
  initializeIrc h (botNick, botUser) channels commands

