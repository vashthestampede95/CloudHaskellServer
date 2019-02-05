{-# LANGUAGE RecordWildCards #-}

module Server where

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process.ManagedProcess ( serve
                                                  , defaultProcess
                                                  , handleRpcChan
                                                  , handleCast
                                                  , handleInfo
                                                  , InitResult(..)
                                                  , UnhandledMessagePolicy(..)
                                                  , ChannelHandler
                                                  , ActionHandler
                                                  , CastHandler
                                                  , ProcessDefinition(..) )
import Control.Distributed.Process ( spawnLocal
                                   , register
                                   , monitorPort
                                   , sendPortId
                                   , processNodeId
                                   , Process
                                   , DiedReason(..)
                                   , ProcessId(..)
                                   , NodeId(..)
                                   , PortMonitorNotification(..) )
import Control.Distributed.Process.ManagedProcess.Server (replyChan, continue)
import Control.Distributed.Process.Extras.Time (Delay(..))
import Control.Distributed.Process.Node ( initRemoteTable
                                        , runProcess
                                        , newLocalNode )
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, forM_, void)
import Logger (runtrainLogger, logStr)
import qualified Data.Map as M (insert, empty, member, delete, filter, elemAt)
import Types

serveChatRoom :: Host -> Int -> ChatName -> IO ()
serveChatRoom host port name = do
  mt <- createTransport host (show port) defaultTCPParameters
  case mt of
    Right transport -> do
      node <- newLocalNode transport initRemoteTable
      runtrainLogger node
      runProcess node $ do
        pId <- launchChatServer
        logStr $ "Server launched at: " ++ show (nodeAddress . processNodeId $ pId)
        register name pId
        liftIO $ forever $ threadDelay 500000
    Left err -> print err

broadcastMessage :: ClientPortMap -> ChatMessage -> Process ()
broadcastMessage clientPorts msg =
  forM_ clientPorts (`replyChan` msg)

messageHandler :: CastHandler ClientPortMap ChatMessage
messageHandler = handler
  where
    handler :: ActionHandler ClientPortMap ChatMessage
    handler clients msg = do
      broadcastMessage clients msg
      continue clients

joinChatHandler :: ChannelHandler ClientPortMap JoinChatMessage ChatMessage
joinChatHandler sendPort = handler
  where
    handler :: ActionHandler ClientPortMap JoinChatMessage
    handler clients JoinChatMessage{..} =
      if clientName `M.member` clients
        then replyChan sendPort (ChatMessage Server "train already connected into the server ... ") >> continue clients
        else do
          void $ monitorPort sendPort
          let clients' = M.insert clientName sendPort clients
              msg = clientName ++ " has joined the server ..."
          logStr msg
          broadcastMessage clients $ ChatMessage Server msg
          continue clients'

disconnectHandler :: ActionHandler ClientPortMap PortMonitorNotification
disconnectHandler clients (PortMonitorNotification _ spId reason) = do
  let search = M.filter (\v -> sendPortId v == spId) clients
  case (null search, reason) of
    (False, DiedDisconnect)-> do
      let (clientName, _) = M.elemAt 0 search
          clients' = M.delete clientName clients
      broadcastMessage clients' (ChatMessage Server $ clientName ++ " has left the server... ")
      continue clients'
    _ -> continue clients

launchChatServer :: Process ProcessId
launchChatServer =
  let server = defaultProcess {
          apiHandlers =  [ handleRpcChan joinChatHandler
                         , handleCast messageHandler
                         ]
        , infoHandlers = [ handleInfo disconnectHandler ]
        , unhandledMessagePolicy = Log
        }
  in spawnLocal $ serve () (const (return $ InitOk M.empty Infinity)) server
