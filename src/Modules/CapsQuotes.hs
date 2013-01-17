module Modules.CapsQuotes (initializeCapsQuotes) where

import Text.Parsers.IRC
import Network.IRC.SevenInch
import Network.Socket
import Network.BSD
import Text.Regex.Posix
import System.IO
import Control.Concurrent.STM
import System.Random

--PubMsg Command IrcUser Channel String
isCapsQuotes :: IrcMsg -> Bool
isCapsQuotes (PubMsg PRIVMSG _ _ msg) = msg =~ "[A-Z\\W]+" :: Bool
isCapsQuotes _ = False

capsQuotesHandler :: TVar [String] -> Handle -> IrcMsg -> SocketHandler -> IO ()
capsQuotesHandler quoteRef h (PubMsg _ _ c msg) cb = do
  quotes <- atomically $ readTVar quoteRef
  index <- getStdRandom $ randomR (0, (length quotes) - 1)
  let response = quotes !! index
  atomically $ writeTVar quoteRef $ quotes ++ [msg]
  sendCmd h PRIVMSG [channelToString c, response]
  cb

newQuotes :: IO (TVar [String])
newQuotes = newTVarIO ["ANGRY FISH"]

initializeCapsQuotes :: IO (MsgHandler)
initializeCapsQuotes = do
  quotes <- newQuotes
  return (isCapsQuotes, capsQuotesHandler quotes)

