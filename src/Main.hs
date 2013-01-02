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
import Modules.Default
import Modules.CapsQuotes

-- Config stuff
channels = ["#test", "#room"]
botNick = ["clbt"]
botUser = ["clbt", "clbt", "clbt", "clbt"]

commands = [echoCommand, quitCommand, autoVoiceCommand]
  
main :: IO ()
main = do
  [server, port] <- getArgs
  putStrLn $ "Connecting to " ++ server ++ ":" ++ port
  h <- initSocket server port
  capsQuotes <- initializeCapsQuotes
  let _commands = capsQuotes : commands
  initializeIrc h (botNick, botUser) channels _commands

