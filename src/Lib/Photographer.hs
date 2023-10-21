{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Photographer
    ( Photographer(..)
    , Photographers(..)
    , getPhotographers
    , getPhotographers'
    , writePhotographers
    , initalState
    , tid
    , name
    , Model(..)
    ) where
import Control.DeepSeq

import Control.Concurrent.MVar.Strict
import Utils.ListZipper
import Control.Lens

import Lib.Data

type Name = String
type Tid = String

data Photographer = Photographer
    { _name :: Name
    , _tid :: Tid
    } deriving (Show, Ord, Eq, Generic, ToJSON, FromJSON, NFData)


makeLenses ''Photographer


newtype Photographers = Photographers { unPhotographers :: ListZipper Photographer }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON, NFData)


getPhotographers' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Photographers)
getPhotographers' = readJSONFile'

writePhotographers' :: (MonadIO m) => FilePath -> Photographers -> m ()
writePhotographers' = writeJSONFile



newtype Model = Model { unModel :: Data String Photographers }
    deriving (Generic, NFData)


initalState :: Model
initalState = Model NotAsked


--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> Photographers -> m ()
write file photographers = liftIO $ withMVar file $ \f -> writePhotographers' f photographers

--TODO could handle error on write.
writePhotographers :: (MonadIO m) => MVar FilePath -> Photographers -> m ()
writePhotographers file photographers = liftIO $ (write file photographers )


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Photographers)
read file = liftIO $ withMVar file $ getPhotographers'


getPhotographers :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Photographers)
getPhotographers file = liftIO $ read file
