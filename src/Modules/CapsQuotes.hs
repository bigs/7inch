module Modules.CapsQuotes (initializeCapsQuotes) where

import Text.Parsers.IRC
import Network.IRC.SevenInch
import Network.Socket
import Network.BSD
import Text.Regex.Posix
import System.IO
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan (TChan)
import System.Random

--PubMsg Command IrcUser Channel String
isCapsQuotes :: IrcMsg -> Bool
isCapsQuotes (PubMsg PRIVMSG _ _ msg) = msg =~ "^[^a-z]*[A-Z]+[^a-z]*$" :: Bool
isCapsQuotes _ = False

capsQuotesHandler :: TVar [String] -> TChan String -> IrcMsg -> SocketHandler -> IO ()
capsQuotesHandler quoteRef chan (PubMsg _ (IrcUser nick _ _) c msg) cb = if nick == "lrts"
  then cb
  else do
    quotes <- atomically $ readTVar quoteRef
    index <- getStdRandom $ randomR (0, (length quotes) - 1)
    let response = quotes !! index
    atomically $ writeTVar quoteRef $ quotes ++ [msg]
    atomically $ sendCmd chan PRIVMSG [channelToString c, response]
    cb

newQuotes :: IO (TVar [String])
newQuotes = newTVarIO ["ANGRY FISH"]

initializeCapsQuotes :: IO (MsgHandler)
initializeCapsQuotes = do
  quotes <- newQuotes
  return (isCapsQuotes, capsQuotesHandler quotes)

