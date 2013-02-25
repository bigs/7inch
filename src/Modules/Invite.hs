module Modules.Invite (inviteCommand) where

import Text.Parsers.IRC
import Network.IRC.SevenInch
import Network.Socket
import Network.BSD
import Text.Regex.Posix ((=~))
import Text.Regex
import System.IO
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan)

isInviteCommand :: IrcMsg -> Bool
isInviteCommand (InviteMsg _ _ _) = True
isInviteCommand (PubMsg _ _ _ msg) = msg =~ "^!i(nvite)? .+$" :: Bool
isInviteCommand _ = False

inviteHandler :: TChan String -> IrcMsg -> SocketHandler -> IO ()
inviteHandler chan (PubMsg _ _ channel msg) cb = do
  let reg = mkRegex "^!i(nvite)? (.+)$"
  case (matchRegex reg msg) of
    Just [_, user] -> atomically (sendCmd chan INVITE [user, channelToString channel]) >> cb
    Nothing        -> cb

inviteHandler chan (InviteMsg _ _ channel) cb = atomically (sendCmd chan JOIN [channelToString channel]) >> cb

inviteCommand = (isInviteCommand, inviteHandler)

