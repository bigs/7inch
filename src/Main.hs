module Main where

import Text.Parsers.IRC
import Network.Socket
import Network.BSD
import System.IO
import System.Environment
import System.Posix.Process
import Data.List (isPrefixOf)
import Text.Regex.Posix
import Network.IRC.SevenInch
import Modules.Default
import Modules.CapsQuotes
import Modules.Search

-- Config stuff
channels = ["#room", "#test"]
botNick = ["clbt"]
botUser = ["clbt", "clbt", "clbt", "clbt"]
server = "irc.faceroar.com"
port = "6667"

commands = [echoCommand, quitCommand, autoVoiceCommand, searchCommand]
  
main :: IO ()
main = do
  putStrLn $ "Connecting to " ++ server ++ ":" ++ port
  h <- initSocket server port
  initializeIrc h (botNick, botUser) channels commands

