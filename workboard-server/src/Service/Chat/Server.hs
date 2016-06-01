{-# LANGUAGE DataKinds, TypeOperators #-}

module Service.Chat.Server where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Aeson
import Data.Word
import Network.Wai
import Servant
import Servant.Server
import Service.Chat


type ListenEndpoint a = Capture "serialId" SerialId :> Get '[JSON] (Maybe a)

type TellEndpoint a = ReqBody '[JSON] a :> Post '[JSON] ()

type ChatAPI a = ListenEndpoint a :<|> TellEndpoint a

data ChatConfig a = ChatConfig
  { proxy         :: Proxy (ChatAPI a)
  , currentSID    :: STM SerialId
  , chat          :: Chat
  }

listenEndpoint :: FromJSON a => ChatConfig a -> Server (ListenEndpoint a)
listenEndpoint config sid = liftIO $ do
  rawMessage <- listen (currentSID config) sid
  pure (decode' rawMessage)

tellEndpoint :: ToJSON a => ChatConfig a -> Server (TellEndpoint a)
tellEndpoint config = liftIO . tell (chat config) . encode

chatServer :: (FromJSON a, ToJSON a) => ChatConfig a -> Server (ChatAPI a)
chatServer config = listenEndpoint config :<|> tellEndpoint config

chatApplication :: (FromJSON a, ToJSON a) => ChatConfig a -> Application
chatApplication config = serve (proxy config) (chatServer config)
