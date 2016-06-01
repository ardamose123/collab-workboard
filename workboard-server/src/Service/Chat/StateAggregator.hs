module Service.Chat.StateAggregator(sparkChatProcessor) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Aeson
import Data.Maybe
import Data.Word
import Service.Chat


-- | Runs a new state updater thread that monitors a given chat.
sparkChatStateUpdater :: FromJSON a
  => (a -> s -> s)  -- ^ A function to derive a new state from an old state and a chat message.
  -> s              -- ^ The start state.
  -> Chat           -- ^ The chat to monitor.
  -> IO (TVar s)    -- ^ A variable to retrieve the updated state when needed.
sparkChatStateUpdater f start chat = do
  (stateVar, chat') <- atomically ((,) <$> newTVar start <*> dupTChan chat)
  forkIO $ evalStateT (loop f stateVar chat') 0
  pure stateVar


-- | The loop of the state updater thread.
loop :: FromJSON a
  => (a -> s -> s)  -- ^ A function to derive a new state from an old state and a chat message.
  -> TVar s         -- ^ A variable with the current state.
  -> Chat           -- ^ The chat to monitor.
  -> StateT SerialId IO () -- ^ A state monad that holds the latest-read serial.
loop merge stateVar chat = forever $ do
  i <- get
  msg <- liftIO (listen chat i)
  when (isJust msg) $ do
    oldState <- liftIO . atomically $ readTVar stateVar
    let newState = merge (fromJust msg) oldState
    liftIO . atomically $ writeTVar stateVar newState
    put (succ i)
