{-# LANGUAGE TypeOperators  #-}

module Service.Chat where

import qualified Data.ByteString.Lazy as BS

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import System.AtomicWrite.Writer.LazyByteString
import System.Directory
import System.FilePath
import Text.Printf
import Data.Word
import Data.Aeson


type SerialId = Word64

data ChatAction message
  = Say    message (TMVar SerialId)
  | Listen Word64  (TMVar (Maybe message))

type Chat a = TChan (ChatAction a)


newChat :: IO (Chat message)
newChat = newTChanIO


sparkChatProcessor :: (FromJSON message, ToJSON message)
  => FilePath
  -> Chat message
  -> IO ThreadId
sparkChatProcessor root queue = do
  startId  <- (fromIntegral . subtract 2 . length) <$> getDirectoryContents root
  forkIO $ evalStateT (forever processChat) startId

  where
    processChat :: StateT SerialId IO ()
    processChat = do
      liftIO $ putStrLn "Waiting for message..."
      liftIO (atomically $ readTChan queue) >>= processChatAction

    processChatAction :: (ToJSON a, FromJSON a) => ChatAction a -> StateT SerialId IO ()
    processChatAction (Say message tmvar) = do
      serialId <- get
      liftIO $ do
        atomicWriteFile (root </> printf "%020u" serialId) (encode message)
        atomically $ putTMVar tmvar serialId
      put (succ serialId)
    processChatAction (Listen i tmvar) = liftIO $ do
      contents <- liftIO $ BS.readFile (root </> printf "%020u" i)
      atomically (putTMVar tmvar (decode contents))


listen :: Chat a -> SerialId -> IO (Maybe a)
listen stream serialId = do
  responseBox <- newEmptyTMVarIO
  atomically $ writeTChan stream (Listen serialId responseBox)
  atomically $ takeTMVar responseBox


say :: Chat a -> a -> IO SerialId
say stream bytes = do
  responseBox <- newEmptyTMVarIO
  atomically $ writeTChan stream (Say bytes responseBox)
  atomically $ takeTMVar responseBox
