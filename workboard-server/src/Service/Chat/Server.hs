{-# LANGUAGE DataKinds, TypeOperators #-}

module Service.Chat.Server where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.ByteString.Lazy
import Network.Wai
import Servant
import Service.Chat


type ListenEndpoint = Capture "serialId" SerialId :> Get '[OctetStream] ByteString

type TellEndpoint = ReqBody '[OctetStream] ByteString :> PostAccepted '[OctetStream] NoContent

type ChatAPI = ListenEndpoint :<|> TellEndpoint

data ChatConfig = ChatConfig
  { currentSID    :: STM SerialId
  , chat          :: Chat
  }

listenEndpoint :: ChatConfig -> Server ListenEndpoint
listenEndpoint config sid = liftIO $ listen (currentSID config) sid

tellEndpoint :: ChatConfig -> Server TellEndpoint
tellEndpoint config message = liftIO $ tell (chat config) message *> pure NoContent

chatServer :: ChatConfig -> Server ChatAPI
chatServer config = listenEndpoint config :<|> tellEndpoint config

chatApplication :: ChatConfig -> Application
chatApplication config = serve (Proxy :: Proxy ChatAPI) (chatServer config)
