{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
module Lib.Build
    ( Build(..)
    , Model(..)
    , writeBuild
    , getBuild
    , toString
    , initalState
    ) where

import Prelude hiding (toString)
import qualified Lib.Translation  as Translation --todo should not be here

import Control.Concurrent.MVar.Strict

import Control.DeepSeq
import Control.Lens

import qualified Control.Lens as Lens

import Lib.Data
import qualified Lib.Photographee as Photographee



data Build
    = DoneBuild Photographee.Photographee String
    | Building Photographee.Photographee String
    | NoBuild
    | NoJpgBuild
    deriving (Eq, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)
    deriving (NFData)



toString :: Build -> Translation.Translation -> String
toString build translation = Lens.view translator translation ++ ". " ++ info
    where 
        translator = case build of
                (DoneBuild _ _) -> Translation.doneBuild
                (Building _ _) -> Translation.building
                NoBuild -> Translation.noBuild
                NoJpgBuild -> Translation.noJpgBuild
        info = case build of
                (DoneBuild photographee x) -> Photographee.toName' photographee ++ " " ++ x
                (Building photographee x) -> Photographee.toName' photographee ++ " " ++ x
                NoBuild -> ""
                NoJpgBuild ->"" 


getBuild' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Build)
getBuild' = readJSONFile'


writeBuild' :: (MonadIO m) => FilePath -> Build -> m ()
writeBuild' = writeJSONFile

newtype Model = Model { unModel :: Data String Build }
    deriving (Generic, NFData)

makeLenses ''Model


initalState :: Model
initalState = Model NotAsked


--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> Build -> m ()
write file build = liftIO $ withMVar file $ \f -> writeBuild' f build

--TODO could handle error on write.
writeBuild :: (MonadIO m) => MVar FilePath -> Build -> m ()
writeBuild file build = liftIO $ (write file build )


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Build)
read file = liftIO $ withMVar file $ \f -> getBuild' f


getBuild :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Build)
getBuild file = liftIO $ read file
