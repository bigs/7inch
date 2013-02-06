module Modules.Boss (bossCommand) where

import Text.Parsers.IRC
import Network.IRC.SevenInch
import Network.Socket
import Network.BSD
import Text.Regex.Posix
import System.IO
import System.Random

isBoss :: IrcMsg -> Bool
isBoss (PubMsg PRIVMSG _ _ msg) = msg =~ "^[a-zA-Z-_.]+$"
isBoss _ = False

bossHandler :: Handle -> IrcMsg -> SocketHandler -> IO ()
bossHandler h (PubMsg _ _ c msg) cb = do
  let response = msg ++ ", boss"
  r <- getStdRandom (randomR (0 :: Integer, 1000 :: Integer))
  if r < 100
    then sendCmd h PRIVMSG [channelToString c, response] >> cb
    else cb

bossCommand = (isBoss, bossHandler)

