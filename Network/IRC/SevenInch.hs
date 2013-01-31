module Network.IRC.SevenInch where

import Text.Parsers.IRC
import Network.Socket
import Network.BSD
import System.IO
import System.Environment
import Control.Concurrent
import Data.List (isPrefixOf)
import Text.Regex.Posix

-- Types
-- ========================
-- 
type MsgHandler = ((IrcMsg -> Bool), (Handle -> IrcMsg -> SocketHandler -> IO ()))

type SocketHandler = IO ()

-- Socket functions
-- ========================
--
initSocket :: String -> String -> IO (Handle)
initSocket server port = do
  addrInfos <- getAddrInfo Nothing (Just server) (Just port)
  let addrInfo = head addrInfos
  sock <- socket (addrFamily addrInfo) Stream defaultProtocol
  setSocketOption sock KeepAlive 1
  connect sock (addrAddress addrInfo)
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h LineBuffering
  return h

-- Sends a command
sendCmd :: Handle -> Command -> [String] -> IO ()
sendCmd h c args = do
  let cmdStr = commandToString c args
  hPutStr h $ cmdStr ++ "\r\n"
  hFlush h

-- Ping handler
respondToPing :: Handle -> String -> IO ()
respondToPing h msg = sendCmd h PONG [msg] 

selectHandler :: IrcMsg ->
                 MsgHandler ->
                 Maybe (Handle -> IrcMsg -> SocketHandler -> IO ())
selectHandler msg (f, g) = if f msg then Just g else Nothing

dispatchCommand :: Handle -> [MsgHandler] -> IrcMsg -> SocketHandler -> IO ()
dispatchCommand _ [] _ cb = cb
dispatchCommand h (c:cs) msg cb = do
  case selectHandler msg c of
    Just handler -> handler h msg recurse
    Nothing -> recurse
  where recurse = dispatchCommand h cs msg cb

socketHandler :: Handle -> [MsgHandler] -> IO ()
socketHandler h commands = do
  dead <- hIsEOF h
  if dead then putStrLn "Quitting..." else do
    line <- hGetLine h
    let stripped = take (length line - 1) line
    let msg = parseIrcMsg stripped
    case msg of
      Left e -> putStrLn (show e) >> putStrLn stripped >> recurse
      Right (PingMsg ping) -> respondToPing h ping >> recurse
      Right ircMsg -> dispatchCommand h commands ircMsg recurse
  where recurse = socketHandler h commands

initializeIrc :: Handle -> ([String], [String]) -> [String] -> [MsgHandler] -> IO ()
initializeIrc h (nick, user) chans commands = do
  sendCmd h NICK nick
  sendCmd h USER user
  waitForReady h chans commands

waitForReady :: Handle -> [String] -> [MsgHandler] -> IO ()
waitForReady h chans commands = do
  dead <- hIsEOF h
  if dead then putStrLn "Quitting..." else do
    line <- hGetLine h
    let stripped = take (length line - 1) line
    let msg = parseIrcMsg stripped
    case msg of
      Right (ServerMsg _ 255 _ _) -> joinChans >> socketHandler h commands
      _ -> waitForReady h chans commands
  where joinChans = sendCmd h JOIN chans

