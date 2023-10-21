{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

module Lib.Grade
    ( Grades(..)
    , Grade'(..)
    , showGrade'
    , Grade(..)
    , Model(..)
    , inputGrade
    , mkNewGrade
    , grades
    , unGrades
    , unGrade
    , extractGrade
    , getGrades
    , getGrades'
    , initialState
    , showGrade
    , writeGrades'
    , writeGrades
    ) where

import Control.Concurrent.MVar.Strict
import Control.DeepSeq

import Utils.Comonad
import Utils.ListZipper

import Lib.Data

import Control.Lens

newtype Grade' = Grade' { _unGrade :: String }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON, NFData)

makeLenses ''Grade'

data Grade
    = Known Grade'
    | Unknown Grade'
        deriving (Eq, Ord, Show)
        deriving (Generic)
        deriving (FromJSON, ToJSON, NFData)

makeLenses ''Grade


newtype Grades = Grades { _unGrades :: ListZipper Grade }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON, NFData)

makeLenses ''Grades


extractGrade :: Grades -> Grade
extractGrade = extract . (view unGrades)


showGrade' :: Grade -> String
showGrade' (Unknown x) = _unGrade x
showGrade' (Known x) = _unGrade x

showGrade :: Grades -> String
showGrade = showGrade' . extract . view unGrades

mkNewGrade :: Grades -> Grades
mkNewGrade grades' =
    grades' & unGrades %~ (\x -> insert x newGrade)
        where
            newGrade = Unknown (Grade' "")

inputGrade :: String -> Grades -> Grades
inputGrade name grades' =
    grades' & unGrades %~ mapFocus (const (Unknown (Grade' name)))


getGrades' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Grades)
getGrades' = readJSONFile'


writeGrades' :: (MonadIO m) => FilePath -> Grades -> m ()
writeGrades' = writeJSONFile



--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> Grades -> m ()
write file grades = liftIO $ withMVar file $ \f -> writeGrades' f grades


--TODO could handle error on write.
writeGrades :: (MonadIO m) => MVar FilePath -> Grades -> m ()
writeGrades file grades = liftIO $ (write file grades) 


data Model = Model { _grades :: Data String Grades } deriving (Show, Generic, NFData)

makeLenses ''Model


initialState :: Model
initialState = Model NotAsked


forker :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Grades)
forker file = liftIO $ withMVar file $ \f -> do
        getGrades' f


getGrades :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Grades)
getGrades file = liftIO $ forker file
