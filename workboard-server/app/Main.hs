{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Service.Chat
import Service.Chat.Server

data Message = Msg { user :: String, content :: String } deriving Generic

instance FromJSON Message
instance ToJSON   Message

main :: IO ()
main = do
  let messageFolder = "/tmp/chat"
  chat <- newChat
  currentSID <- sparkChatProcessor messageFolder chat
  let chatConfig = ChatConfig Proxy messageFolder currentSID chat :: ChatConfig Message
  run 9621 (chatApplication chatConfig)
