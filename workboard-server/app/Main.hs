{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson
import GHC.Generics
import Network.Wai.Handler.Warp
import Service.Chat
import Service.Chat.Server
import System.Environment

data Message = Msg { user :: String, content :: String } deriving Generic

instance FromJSON Message
instance ToJSON   Message

main :: IO ()
main = do
  port        <- read <$> getEnv "PORT"
  chat'       <- newChat
  currentSID' <- sparkChatProcessor chat'
  run port $ chatApplication (ChatConfig currentSID' chat')
