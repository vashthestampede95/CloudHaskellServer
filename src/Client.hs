module Client where

import Control.Distributed.Process.ManagedProcess.Client (callChan, cast)
import Control.Distributed.Process ( expectTimeout
                                   , whereisRemoteAsync
                                   , spawnLocal
                                   , receiveChan
                                   , link
                                   , NodeId(..)
                                   , Process
                                   , ProcessId
                                   , ReceivePort
                                   , WhereIsReply(..) )
import Control.Distributed.Process.Node ( initRemoteTable
                                        , runProcess
                                        , newLocalNode )
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.Transport     (EndPointAddress(..))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, forever)
import qualified Data.ByteString.Char8 as BS (pack)
import Types
import Logger (runtrainLogger, logChatMessage, logStr)


searchChatServer :: ChatName -> ServerAddress -> Process ProcessId
searchChatServer name serverAddr = do
  let addr = EndPointAddress (BS.pack serverAddr)
      srvId = NodeId addr
  whereisRemoteAsync srvId name
  reply <- expectTimeout 1000
  case reply of
    Just (WhereIsReply _ (Just sid)) -> return sid
    _ -> searchChatServer name serverAddr

launchChatClient :: ServerAddress -> Host -> Int -> ChatName -> IO ()
launchChatClient serverAddr clientHost port name  = do
  mt <- createTransport clientHost (show port) defaultTCPParameters
  case mt of
    Left err -> print err
    Right transport -> do
      node <- newLocalNode transport initRemoteTable
      runtrainLogger node
      runProcess node $ do
        serverPid <- searchChatServer name serverAddr
        link serverPid
        logStr "Joining server .."
        logStr "Please provide Train Name  "
        nickName <- liftIO getLine
        rp <- callChan serverPid (JoinChatMessage nickName) :: Process (ReceivePort ChatMessage)
        logStr "You have joined the server ... "
        void $ spawnLocal $ forever $ do
          msg <- receiveChan rp
          logChatMessage msg
        forever $ do
          chatInput <- liftIO getLine
          cast serverPid (ChatMessage (Client nickName) chatInput)
          liftIO $ threadDelay 500000
