module Text.Parsers.IRC where

import Text.ParserCombinators.Parsec
import Data.List (intercalate)

data Command = PRIVMSG |
               USER |
               NICK |
               JOIN |
               PART |
               TOPIC |
               NOTICE |
               PING |
               PONG |
               ERROR |
               KICK |
               QUIT |
               MODE deriving (Show, Eq)

-- Irc User struct   : Nick   Ident  Host
data IrcUser = IrcUser String String String deriving (Show, Eq)
data Channel = Channel String deriving (Show, Eq)

userToString :: IrcUser -> String
userToString (IrcUser n i h) = n ++ "!" ++ i ++ "@" ++ h

channelToString :: Channel -> String
channelToString (Channel c) = "#" ++ c

data IrcMsg =
  PrivMsg Command IrcUser IrcUser String |
  PubMsg Command IrcUser Channel String |
  JoinPartMsg Command IrcUser Channel |
  ServerMsg String Int String String |
  PingMsg String |
  ErrorMsg String |
  AuthNoticeMsg String |
  ModeMsg IrcUser Channel String [String] |
  KickMsg IrcUser Channel String String |
  TopicMsg IrcUser Channel String
  deriving (Show)

commandToString :: Command -> [String] -> String
commandToString PRIVMSG [to, msg] = "PRIVMSG " ++ to ++ " :" ++ msg
commandToString NOTICE [to, msg] = "NOTICE " ++ to ++ " :" ++ msg
commandToString USER args = "USER " ++ (intercalate " " args)
commandToString NICK [nick] = "NICK " ++ nick
commandToString JOIN rooms = intercalate "\r\n" roomString
  where roomString = ["JOIN " ++ r | r <- rooms]
commandToString PART rooms = intercalate "\r\n" roomString
  where roomString = ["PART " ++ r | r <- rooms]
commandToString TOPIC [room, topic] = "TOPIC " ++ room ++ " :" ++ topic
commandToString PONG [response] = "PONG :" ++ response
commandToString QUIT [msg] = "QUIT :" ++ msg
commandToString QUIT [] = "QUIT"
commandToString KICK [room, user, reason] = "KICK " ++ room ++ " " ++ user ++ " :" ++ reason
commandToString MODE (room:mode:users) = "MODE " ++ room ++ " " ++ mode ++ intercalate " " users
commandToString _ _ = ""

commandString :: Command -> Parser Command
commandString command = try $ do
  string $ show command
  return command

userString :: Parser IrcUser
userString = try $ do
  nick <- manyTill (noneOf " !") $ char '!'
  ident <- manyTill (noneOf " @") $ char '@'
  host <- manyTill anyChar $ try space
  return $ IrcUser nick ident host

channelStringNonTerm :: Parser Channel
channelStringNonTerm = try $ do
  char '#'
  chan <- manyTill (noneOf " ") $ try space
  return $ Channel chan

channelStringTerm :: Parser Channel
channelStringTerm = try $ do
  char '#'
  chan <- many (noneOf " ")
  return $ Channel chan

channelString :: Parser Channel
channelString = channelStringNonTerm <|> channelStringTerm

toFromMsgPriv :: Command -> Parser IrcMsg
toFromMsgPriv command = try $ do
  char ':'
  from <- userString
  commandString command
  space
  to <- userString
  char ':'
  msg <- many anyChar
  return $ PrivMsg command from to msg

toFromMsgPub :: Command -> Parser IrcMsg
toFromMsgPub command = try $ do
  char ':'
  from <- userString
  commandString command
  space
  to <- channelString
  char ':'
  msg <- many anyChar
  return $ PubMsg command from to msg

kickMsg :: Parser IrcMsg
kickMsg = try $ do
  char ':'
  user <- userString
  commandString KICK
  space
  chan <- channelString
  kicked <- manyTill anyChar $ try space
  char ':'
  msg <- many anyChar
  return $ KickMsg user chan kicked msg

topicMsg :: Parser IrcMsg
topicMsg = try $ do
  char ':'
  user <- userString
  commandString TOPIC
  space
  chan <- channelString
  char ':'
  msg <- many anyChar
  return $ TopicMsg user chan msg

errorMsg :: Parser IrcMsg
errorMsg = try $ do
  string "ERROR :"
  msg <- many anyChar
  return $ ErrorMsg msg

modeMsg :: Parser IrcMsg
modeMsg = try $ do
  char ':'
  user <- userString
  commandString MODE
  space
  chan <- channelString
  status <- manyTill anyChar $ try space
  users <- many $ try $ manyTill anyChar $ try space
  finalUser <- many anyChar
  return $ ModeMsg user chan status (users ++ [finalUser])

serverMsg :: Parser IrcMsg
serverMsg = try $ do
  char ':'
  server <- manyTill (noneOf " ") space
  code <- manyTill digit space
  user <- manyTill (noneOf " ") space
  msg <- try (do { char ':'; many anyChar }) <|> many anyChar
  let codeNum = read code :: Int
  return $ ServerMsg server codeNum user msg

pingMsg :: Parser IrcMsg
pingMsg = try $ do
  commandString PING
  space
  char ':'
  ping <- many anyChar 
  return $ PingMsg ping

authNotice :: Parser IrcMsg
authNotice = try $ do
  commandString NOTICE
  space
  string "AUTH :"
  msg <- many anyChar
  return $ AuthNoticeMsg msg

joinPartMsg :: Command -> Parser IrcMsg
joinPartMsg command = try $ do
  char ':'
  nick <- userString
  c <- commandString command 
  space
  char ':'
  channel <- channelString
  return $ JoinPartMsg c nick channel

ircStmt :: Parser IrcMsg
ircStmt = toFromMsgPriv PRIVMSG <|>
          toFromMsgPriv NOTICE <|>
          toFromMsgPub PRIVMSG <|>
          toFromMsgPub NOTICE <|>
          joinPartMsg JOIN <|>
          joinPartMsg PART <|>
          serverMsg <|>
          pingMsg <|>
          errorMsg <|>
          modeMsg <|>
          kickMsg <|>
          topicMsg <|>
          authNotice

parseIrcMsg = parse ircStmt "IRC"

