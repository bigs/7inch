module Text.Parsers.IRC (Command, IrcMsg, IrcUser, parseIrcMsg) where

import Text.ParserCombinators.Parsec

data Command = PRIVMSG | USER | NICK | JOIN | PART | TOPIC | NOTICE deriving (Show, Eq)

commandToString :: Command -> String
commandToString c = case c of
  PRIVMSG -> "PRIVMSG"
  USER    -> "USER"
  NICK    -> "NICK"
  JOIN    -> "JOIN"
  PART    -> "PART"
  TOPIC   -> "TOPIC"
  NOTICE  -> "NOTICE"

-- Irc User struct   : Nick   Ident  Host
data IrcUser = IrcUser String String String deriving (Show, Eq)
data Channel = Channel String deriving (Show, Eq)

userToString :: IrcUser -> String
userToString (IrcUser n i h) = n ++ "!" ++ i ++ "@" ++ h

data IrcMsg =
  PrivMsg Command IrcUser IrcUser String |
  PubMsg Command IrcUser Channel String |
  JoinPartMsg Command IrcUser String |
  ServerMsg String Int String String |
  AuthNotice String
  deriving (Show)

msgString :: IrcMsg -> String
msgString msg = msgString' msg ++ "\r\n"

msgString' :: IrcMsg -> String
msgString' msg = case msg of
  PrivMsg t from to msg -> ":" ++ userToString from ++ " " ++ commandToString t ++ " " ++ userToString to ++ " :" ++ msg
  JoinPartMsg t nick channel -> ":" ++ userToString nick ++ " " ++ commandToString t ++ " #" ++ channel

commandString :: Command -> Parser Command
commandString command = try $ do
  string $ commandToString command
  return command

userString :: Parser IrcUser
userString = try $ do
  nick <- manyTill (noneOf " !") $ char '!'
  ident <- manyTill (noneOf " @") $ char '@'
  host <- manyTill anyChar $ try space
  return $ IrcUser nick ident host

channelString :: Parser Channel
channelString = try $ do
  char '#'
  chan <- manyTill (noneOf " ") $ try space
  return $ Channel chan

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

serverMsg :: Parser IrcMsg
serverMsg = try $ do
  char ':'
  server <- manyTill (noneOf " ") space
  code <- manyTill digit space
  user <- manyTill (noneOf " ") space
  msg <- try (do { char ':'; many anyChar }) <|> many anyChar
  let codeNum = read code :: Int
  return $ ServerMsg server codeNum user msg

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
  char '#'
  channel <- many anyChar
  return $ JoinPartMsg c nick channel

ircStmt :: Parser IrcMsg
ircStmt = toFromMsgPriv PRIVMSG <|>
          toFromMsgPriv NOTICE <|>
          toFromMsgPub PRIVMSG <|>
          toFromMsgPub NOTICE <|>
          joinPartMsg JOIN <|>
          joinPartMsg PART <|>
          serverMsg <|>
          authNotice

parseIrcMsg = parse ircStmt "IRC"

