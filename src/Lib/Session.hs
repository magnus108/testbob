{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

module Lib.Session
    ( Sessions(..)
    , Session(..)
    , Decisions(..)
    , translationSessionButton
    , translationSession
    , translationDecision
    , getSessions
    , writeSessions
    , getSessions'
    , initialState
    , Model(..)
    , writeSessions'
    , toInteger
    ) where

import Prelude hiding (toInteger)
import qualified Control.Lens as Lens
import Control.DeepSeq
import qualified Utils.ListZipper as ListZipper

import Control.Concurrent.MVar.Strict

import Lib.Data
import Control.Lens
import qualified Lib.Translation  as Translation --todo should not be here


-- hvordan skal det her virke med shootings
data Session
    = KindergartenGroup Int
    | KindergartenSingle Int
    | School Int
    | Business Int
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON, NFData)

toInteger :: Session -> Int
toInteger (KindergartenSingle x) = x
toInteger (KindergartenGroup x) = x
toInteger (School x) = x
toInteger (Business x) = x


data Decisions
    = SchoolOrKindergarten
    | GroupOrSingleForKindergarten
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON, NFData)



--TODO this is rediculose
translationDecision :: Decisions -> Translation.Translation -> String
translationDecision decision = Lens.view translator
    where translator = case decision of
            SchoolOrKindergarten -> Translation.schoolOrKindergarten
            GroupOrSingleForKindergarten -> Translation.groupOrSingleForKindergarten


translationSessionButton :: Session -> Translation.Translation -> String
translationSessionButton session = Lens.view translator
    where translator = case session of
            KindergartenGroup _ -> Translation.buildGroup
            KindergartenSingle _ -> Translation.buildSingle
            School _ -> Translation.build
            Business _ -> Translation.buildBusiness

translationSession :: Session -> Translation.Translation -> String
translationSession session = Lens.view translator
    where translator = case session of
            KindergartenGroup _ -> Translation.kindergartenGroup
            KindergartenSingle  _-> Translation.kindergartenSingle
            School _ -> Translation.school
            Business _ -> Translation.business


newtype Sessions = Sessions { unSessions:: ListZipper.ListZipper Session }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON, NFData)


getSessions' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Sessions)
getSessions' = readJSONFile'


writeSessions' :: (MonadIO m) => FilePath -> Sessions -> m ()
writeSessions' = writeJSONFile

newtype Model = Model { unModel :: Data String Sessions }
    deriving (NFData, Generic)


makeLenses ''Model


initialState :: Model
initialState = Model NotAsked


--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> Sessions -> m ()
write file sessions = liftIO $ withMVar file $ \f -> writeSessions' f sessions

--TODO could handle error on write.
writeSessions :: (MonadIO m) => MVar FilePath -> Sessions -> m ()
writeSessions file sessions = liftIO $ (write file sessions) 


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Sessions)
read file = liftIO $ withMVar file $ \f -> do
        --_ <- liftIO $ handle (Model Loading)
        getSessions' f


getSessions :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Sessions)
getSessions file = liftIO $ read file
