{-# LANGUAGE DeriveGeneric #-}

module Main where

import Service.Chat
import Service.Chat.Server
import GHC.Generics
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Proxy

data Message = Msg { user :: String, content :: String } deriving Generic

instance FromJSON Message
instance ToJSON   Message

main :: IO ()
main = do
  chat <- newChat
  readSID <- sparkChatProcessor "/tmp/chat/" chat
  run 9621 $ chatApplication "/tmp/chat" readSID chat (Proxy :: Proxy (ChatAPI Message))
