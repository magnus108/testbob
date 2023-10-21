{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Dagsdato
    ( Dagsdato(..)
    , Model(..)
    , getDagsdato'
    , getDagsdato
    , writeDagsdato
    , initialState
    ) where
import Control.DeepSeq

import System.Directory

import Control.Concurrent.MVar.Strict


import Lib.Data

import Control.Lens

newtype Dagsdato = Dagsdato { unDagsdato :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON, NFData)


newtype Model = Model { unModel :: Data String Dagsdato }
    deriving (Show, NFData)

makeLenses ''Model

initialState :: Model
initialState = Model NotAsked

getDagsdato' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Dagsdato)
getDagsdato' = readJSONFile'


writeDagsdato' :: (MonadIO m) => FilePath -> Dagsdato -> m ()
writeDagsdato' = writeJSONFile


--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> Dagsdato -> m ()
write file dagsdato' = liftIO $ withMVar file $ \f -> writeDagsdato' f dagsdato'


--TODO could handle error on write.
writeDagsdato :: (MonadIO m) => MVar FilePath -> Dagsdato -> m ()
writeDagsdato file dagsdato' = liftIO $ (write file dagsdato')


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Dagsdato)
read file = liftIO $ withMVar file $ \f -> do
        file' <- getDagsdato' f
        case file' of
          Left e -> return $ Left e
          Right string -> do
                isDir <- doesDirectoryExist (unDagsdato string)
                if isDir then
                    return $ Right string
                else
                    return $ Left "Er ikke mappe"
            



getDagsdato :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Dagsdato)
getDagsdato file = liftIO $ read file
