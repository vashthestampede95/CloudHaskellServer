{-# LANGUAGE RecordWildCards    #-}

module Logger where

import Control.Distributed.Process ( receiveWait
                                   , match
                                   , register
                                   , nsend
                                   , Process )
import Control.Monad.IO.Class (liftIO)
import Control.Distributed.Process.Node (runProcess, forkProcess, LocalNode)
import Types

chatMessageToStr :: ChatMessage -> String
chatMessageToStr ChatMessage{..} =
  case from of
    Server ->  message
    Client sender-> sender ++ ": " ++ message

trainLogger :: Process ()
trainLogger = receiveWait
  [ match $ \chatMessage -> do
      liftIO . putStrLn $ chatMessageToStr chatMessage
      trainLogger
  , match $ \str -> do
      liftIO . putStrLn $ str
      trainLogger
  ]

runtrainLogger :: LocalNode -> IO ()
runtrainLogger node = do
  logger <- forkProcess node trainLogger
  runProcess node $ register "trainlogger" logger

logChatMessage :: ChatMessage -> Process ()
logChatMessage = nsend "trainLogger"

logStr :: String -> Process ()
logStr = nsend "trainLogger"
