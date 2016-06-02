{-# LANGUAGE TypeOperators  #-}

module Service.Chat where

import qualified Data.ByteString.Lazy as BS

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Word
import System.AtomicWrite.Writer.LazyByteString
import System.Directory
import Text.Printf


type SerialId = Word64

type Chat = TChan BS.ByteString


newChat :: IO Chat
newChat = newTChanIO


sparkChatProcessor :: Chat -> IO (STM SerialId)
sparkChatProcessor chat = do
  startId  <- (fromIntegral . subtract 2 . length) <$> getDirectoryContents "."
  serialIdVar <- newTVarIO startId
  void . forkIO . forever $ do
    (message, serialId) <- atomically ((,) <$> readTChan chat <*> readTVar serialIdVar)
    atomicWriteFile (printf "%020u" serialId) message
    atomically (writeTVar serialIdVar $ succ serialId)
  pure (readTVar serialIdVar)


listen :: STM SerialId -> SerialId -> IO BS.ByteString
listen readCurrentSID lastReadSID = do
  atomically $ do
    currentSID <- readCurrentSID
    guard (lastReadSID < currentSID)
  BS.readFile (printf "%020u" lastReadSID)


tell :: Chat -> BS.ByteString -> IO ()
tell chat message = atomically $ writeTChan chat message
