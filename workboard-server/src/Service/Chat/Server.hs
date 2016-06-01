{-# LANGUAGE DataKinds, TypeOperators #-}

module Service.Chat.Server where

import Servant
import Servant.Server
import Network.Wai
import Data.Aeson
import Data.Word
import Service.Chat
import Control.Monad.IO.Class
import Network.Wai.Middleware.AddHeaders
import Control.Concurrent.STM


type ListenEndpoint a = Capture "serialId" SerialId :> Get '[JSON] (Maybe a)

type TellEndpoint a = ReqBody '[JSON] a :> Post '[JSON] ()

type ChatAPI a = ListenEndpoint a :<|> TellEndpoint a

data ChatConfig a = ChatConfig
  { proxy         :: Proxy (ChatAPI a)
  , messageFolder :: FilePath
  , currentSID    :: STM SerialId
  , chat          :: Chat
  }

listenEndpoint :: FromJSON a => ChatConfig a -> Server (ListenEndpoint a)
listenEndpoint config sid = liftIO $ do
  rawMessage <- listen (messageFolder config) (currentSID config) sid
  pure (decode' rawMessage)

tellEndpoint :: ToJSON a => ChatConfig a -> Server (TellEndpoint a)
tellEndpoint config = liftIO . tell (chat config) . encode

chatServer :: (FromJSON a, ToJSON a) => ChatConfig a -> Server (ChatAPI a)
chatServer config = listenEndpoint config :<|> tellEndpoint config

chatApplication :: (FromJSON a, ToJSON a) => ChatConfig a -> Application
chatApplication config = serve (proxy config) (chatServer config)
