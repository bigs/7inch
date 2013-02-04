module Modules.Invite (inviteCommand) where

import Text.Parsers.IRC
import Network.IRC.SevenInch
import Network.Socket
import Network.BSD
import Text.Regex.Posix ((=~))
import Text.Regex
import System.IO

isInviteCommand :: IrcMsg -> Bool
isInviteCommand (InviteMsg _ _ _) = True
isInviteCommand (PubMsg _ _ _ msg) = msg =~ "^!i(nvite)? .+$" :: Bool
isInviteCommand _ = False

inviteHandler :: Handle -> IrcMsg -> SocketHandler -> IO ()
inviteHandler h (PubMsg _ _ chan msg) cb = do
  let reg = mkRegex "^!i(nvite)? (.+)$"
  case (matchRegex reg msg) of
    Just [_, user] -> sendCmd h INVITE [user, channelToString chan] >> cb
    Nothing        -> cb

inviteHandler h (InviteMsg _ _ chan) cb = sendCmd h JOIN [channelToString chan] >> cb

inviteCommand = (isInviteCommand, inviteHandler)

