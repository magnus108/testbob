{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.DagsdatoBackup
    ( DagsdatoBackup(..)
    , getDagsdatoBackup
    , getDagsdatoBackup'
    , writeDagsdatoBackup
    , Model(..)
    , initialState
    ) where

import Control.DeepSeq

import System.Directory
import Control.Concurrent.MVar.Strict

import Lib.Data

import Control.Lens

newtype DagsdatoBackup = DagsdatoBackup { unDagsdatoBackup :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON, NFData)

newtype Model = Model { unModel :: Data String DagsdatoBackup }
    deriving (Show, Generic, NFData)

makeLenses ''Model

initialState :: Model
initialState = Model NotAsked

getDagsdatoBackup' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String DagsdatoBackup)
getDagsdatoBackup' = readJSONFile'


writeDagsdatoBackup' :: (MonadIO m) => FilePath -> DagsdatoBackup -> m ()
writeDagsdatoBackup' = writeJSONFile


--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> DagsdatoBackup -> m ()
write file dagsdatoBackup' = liftIO $ withMVar file $ \f -> writeDagsdatoBackup' f dagsdatoBackup'


--TODO could handle error on write.
writeDagsdatoBackup :: (MonadIO m) => MVar FilePath -> DagsdatoBackup -> m ()
writeDagsdatoBackup file dagsdatoBackup' = liftIO $ (write file dagsdatoBackup') 


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String DagsdatoBackup)
read file = liftIO $ withMVar file $ \f -> do
        file' <- getDagsdatoBackup' f
        case file' of
          Left e -> return $ Left e
          Right string -> do
                isDir <- doesDirectoryExist (unDagsdatoBackup string)
                if isDir then
                    return $ Right string
                else
                    return $ Left "Er ikke mappe"


getDagsdatoBackup :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String DagsdatoBackup)
getDagsdatoBackup file = liftIO $ read file
