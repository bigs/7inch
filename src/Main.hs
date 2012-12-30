module Main where

import Text.Parsers.IRC
import Network.Socket
import Network.BSD
import System.IO
import System.Environment
import Control.Monad.Reader (forever)
import Control.Concurrent
import Data.List (isPrefixOf)
import Text.Regex.Posix

loop :: IO ()
loop = do
  putStrLn "Line:"
  line <- getLine
  putStrLn $ show $ parseIrcMsg line
  loop

initSocket :: String -> String -> IO (Handle)
initSocket server port = do
  -- Gather address info
  addrInfos <- getAddrInfo Nothing (Just server) (Just port)
  let addrInfo = head addrInfos
  sock <- socket (addrFamily addrInfo) Stream defaultProtocol
  setSocketOption sock KeepAlive 1
  -- Connect and create handle 
  connect sock (addrAddress addrInfo)
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h LineBuffering
  return h

respondToPing :: Handle -> String -> IO ()
respondToPing h msg = do
  putStrLn "Responding to ping..."
  hPutStr h $ "PONG :" ++ msg ++ "\r\n"
  hFlush h


--PubMsg Command IrcUser Channel String

sendCmd :: Handle -> String -> IO ()
sendCmd h cmd = do
  hPutStr h $ cmd ++ "\r\n"

sendMsg :: Handle -> Command -> Channel -> String -> IO ()
sendMsg h c ch msg = do
  let line = (show c) ++ " " ++ channelToString ch ++ " :" ++ msg
  sendCmd h line

selectHandler :: IrcMsg -> [((IrcMsg -> Bool), (Handle -> IrcMsg -> IO ()))] -> Maybe (Handle -> IrcMsg -> IO ())
selectHandler _ [] = Nothing
selectHandler msg ((f, g):xs) = if f msg then Just g else selectHandler msg xs

echoHandler :: Handle -> IrcMsg -> IO ()
echoHandler h (PubMsg c _ channel msg) = do
  let line = drop 6 msg
  sendMsg h c channel line

isMsgMatchingRegex :: String -> IrcMsg -> Bool
isMsgMatchingRegex regex (PubMsg _ _ _ msg) = msg =~ regex :: Bool
isMsgMatchingRegex regex (PrivMsg _ _ _ msg) = msg =~ regex :: Bool
isMsgMatchingRegex _ _ = False

isEcho = isMsgMatchingRegex "^!echo .+"

isQuit = isMsgMatchingRegex "^!quit"

quitHandler :: Handle -> IrcMsg -> IO ()
quitHandler h _ = do
  sendCmd h "QUIT"

commands = [(isEcho, echoHandler), (isQuit, quitHandler)]

dispatchCommand :: Handle -> IrcMsg -> IO ()
dispatchCommand h msg = do
  case selectHandler msg commands of
    Nothing -> socketHandler h
    Just handler -> handler h msg >> socketHandler h

socketHandler :: Handle -> IO ()
socketHandler h = do
  dead <- hIsEOF h
  if dead then putStrLn "Quitting..."
    else do
      line <- hGetLine h
      let stripped = take (length line - 1) line
      let msg = parseIrcMsg stripped
      case msg of
        Left e -> putStrLn stripped >> socketHandler h
        Right (PingMsg ping) -> respondToPing h ping >> socketHandler h
        Right ircMsg -> dispatchCommand h ircMsg

initializeIrc :: Handle -> IO ()
initializeIrc h = do
  hPutStr h "NICK clbt\r\n"
  hPutStr h "USER colebot colebot colebot colebot\r\n"
  hFlush h
  waitForReady h joinChan

joinChan :: Handle -> IO ()
joinChan h = do
  sendCmd h "JOIN #test"
  socketHandler h

waitForReady :: Handle -> (Handle -> IO ()) -> IO ()
waitForReady h cb = do
  dead <- hIsEOF h
  if dead then putStrLn "Quitting wait..." else do
    line <- hGetLine h
    let stripped = take (length line - 1) line
    let msg = parseIrcMsg stripped
    case msg of
      Right (ServerMsg _ 376 _ _) -> cb h
      _ -> waitForReady h cb
  
main :: IO ()
main = do
  [server, port] <- getArgs
  putStrLn $ "Connecting to " ++ server ++ ":" ++ port
  h <- initSocket server port
  initializeIrc h

