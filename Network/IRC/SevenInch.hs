module Network.IRC.SevenInch where

import Text.Parsers.IRC
import Network.Socket
import Network.BSD
import System.IO
import Text.Regex.Posix
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Concurrent

-- Types
-- ========================
-- 
type MsgHandler = ((IrcMsg -> Bool), (TChan String -> IrcMsg -> SocketHandler -> IO ()))
type CommandWriter = Command -> [String] -> STM ()

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
sendCmd :: TChan String -> Command -> [String] -> STM ()
sendCmd chan c args = do
  let cmdStr = commandToString c args
  writeTChan chan $ cmdStr ++ "\r\n"

writeLoop :: Handle -> TChan String -> IO ()
writeLoop h chan = forever $ do
  msg <- atomically $ readTChan chan
  hPutStr h msg
  hFlush h

-- Ping handler
respondToPing :: TChan String -> String -> IO ()
respondToPing chan msg = atomically $ sendCmd chan PONG [msg] 

selectHandler :: IrcMsg ->
                 MsgHandler ->
                 Maybe (TChan String -> IrcMsg -> SocketHandler -> IO ())
selectHandler msg (f, g) = if f msg then Just g else Nothing

dispatchCommand :: TChan String -> [MsgHandler] -> IrcMsg -> SocketHandler -> IO ()
dispatchCommand _ [] _ cb = cb
dispatchCommand chan (c:cs) msg cb = do
  case selectHandler msg c of
    Just handler -> handler chan msg recurse
    Nothing -> recurse
  where recurse = dispatchCommand chan cs msg cb

socketHandler :: Handle -> TChan String -> [MsgHandler] -> IO ()
socketHandler h chan commands = do
  dead <- hIsEOF h
  if dead then hClose h else do
    line <- hGetLine h
    let stripped = take (length line - 1) line
    let msg = parseIrcMsg stripped
    case msg of
      Left e -> putStrLn (show e) >> putStrLn stripped >> recurse
      Right (PingMsg ping) -> respondToPing chan ping >> recurse
      Right ircMsg -> dispatchCommand chan commands ircMsg recurse
  where recurse = socketHandler h chan commands

initializeIrc :: Handle -> ([String], [String]) -> [String] -> [MsgHandler] -> IO ()
initializeIrc h (nick, user) chans commands = do
  chan <- atomically $ newTChan :: IO (TChan String)
  threadId <- forkIO $ writeLoop h chan
  atomically $ sendCmd chan NICK nick
  atomically $ sendCmd chan USER user
  waitForReady h chan chans commands

initializeReadyIrc :: Handle -> TChan String -> [String] -> [MsgHandler] -> IO ()
initializeReadyIrc h chan channels commands = do
  atomically $ sendCmd chan JOIN channels
  socketHandler h chan commands

waitForReady :: Handle -> TChan String -> [String] -> [MsgHandler] -> IO ()
waitForReady h chan chans commands = do
  dead <- hIsEOF h
  if dead then hClose h else do
    line <- hGetLine h
    let stripped = take (length line - 1) line
    let msg = parseIrcMsg stripped
    case msg of
      Right (ServerMsg _ 255 _ _) -> initializeReadyIrc h chan chans commands
      _ -> waitForReady h chan chans commands

