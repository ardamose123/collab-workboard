{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

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


listenEndpoint :: FromJSON a => FilePath -> STM SerialId -> Server (ListenEndpoint a)
listenEndpoint root readSID sid = liftIO (decode' <$> listen root readSID sid)

tellEndpoint :: ToJSON a => Chat -> Server (TellEndpoint a)
tellEndpoint chat = liftIO . tell chat . encode

chatServer :: (FromJSON a, ToJSON a) => FilePath -> STM SerialId -> Chat -> Server (ChatAPI a)
chatServer root readSID chat = listenEndpoint root readSID :<|> tellEndpoint chat


chatApplication :: (FromJSON a, ToJSON a) => FilePath -> STM SerialId -> Chat -> Proxy (ChatAPI a) -> Application
chatApplication root readSID chat proxy = serve proxy (chatServer root readSID chat)
