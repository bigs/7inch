module Main where

import Network.Socket
import Network.BSD
import System.IO
import System.Environment
import Network.IRC.SevenInch
import Modules.Default
import Modules.CapsQuotes
import Modules.Search
import Modules.Topic
import Modules.Invite
import Modules.Boss

-- Config stuff
channels = ["#room", "#test"]
botNick = ["clbt"]
botUser = ["clbt", "clbt", "clbt", "clbt"]
server = "irc.faceroar.com"
port = "6667"

commands = [echoCommand, quitCommand, autoVoiceCommand, searchCommand, inviteCommand, bossCommand]
  
main :: IO ()
main = do
  h <- initSocket server port
  topicCommand <- initializeTopicChange
  let _commands = topicCommand : commands
  initializeIrc h (botNick, botUser) channels _commands

