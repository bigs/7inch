module Text.Parsers.IRC where

import Text.ParserCombinators.Parsec

data Command = PRIVMSG | USER | NICK | JOIN | PART | TOPIC | NOTICE | PING | ERROR deriving (Show, Eq)

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
  AuthNotice String
  deriving (Show)

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

errorMsg :: Parser IrcMsg
errorMsg = try $ do
  string "ERROR :"
  msg <- many anyChar
  return $ ErrorMsg msg

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
  return $ AuthNotice msg

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
          authNotice

parseIrcMsg = parse ircStmt "IRC"

