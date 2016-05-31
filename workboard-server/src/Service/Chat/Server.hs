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


type ChatAPI a
  =    Capture "serialId" Word64 :> Get  '[JSON] (Maybe a)
  :<|> ReqBody '[JSON]    a      :> Post '[JSON] SerialId
  :<|> Options ()


chatServer :: Chat a -> Server (ChatAPI a)
chatServer c = (liftIO . listen c) :<|> (liftIO . say c)


chatApplication :: (FromJSON a, ToJSON a) => Proxy (ChatAPI a) -> Chat a -> Application
chatApplication proxy
  = addHeaders [("Access-Control-Allow-Origin", "*")]
  . serve proxy
  . chatServer
