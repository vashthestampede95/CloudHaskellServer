{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Types where

import GHC.Generics
import Data.Binary
import Data.Typeable.Internal
import Data.Map (Map)
import Control.Distributed.Process (SendPort)


type ChatName = String
type TrainNo =Int
type TrainName = String

type Host = String

type ServerAddress = String

type ClientPortMap = Map TrainName (SendPort ChatMessage)

data Sender = Server | Client TrainName
  deriving (Generic, Typeable, Eq, Show)

instance Binary Sender

data ChatMessage = ChatMessage {
    from :: Sender
  , message :: String
} deriving (Generic, Typeable, Show)

instance Binary ChatMessage

newtype JoinChatMessage = JoinChatMessage {
    clientName :: TrainName

  } deriving (Generic, Typeable, Show)

instance Binary JoinChatMessage
