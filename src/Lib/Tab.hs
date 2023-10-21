{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

module Lib.Tab
    ( Tabs(..)
    , Tab(..)
    , getTabs
    , toTranslation
    , writeTabs
    ) where

import Utils.ListZipper
import Lib.Translation

data Tab
    = DumpTab
    | DagsdatoTab
    | DagsdatoBackupTab
    | DoneshootingTab
    | DoneshootingBackupTab
    | PhotographersTab
    | CamerasTab
    | ShootingsTab
    | SessionsTab
    | LocationTab
    | MainTab
    | InsertPhotographeeTab
    | ControlTab
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


newtype Tabs = Tabs { unTabs :: ListZipper Tab }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)



getTabs :: (MonadIO m, MonadThrow m) => FilePath -> m Tabs
getTabs = readJSONFile


writeTabs :: (MonadIO m) => FilePath -> Tabs -> m ()
writeTabs = writeJSONFile

toTranslation :: Functor f => Tab -> (String -> f String) -> Translation -> f Translation
toTranslation DumpTab = dumpTab
toTranslation DagsdatoTab = dagsdatoTab
toTranslation DagsdatoBackupTab = dagsdatoBackupTab
toTranslation DoneshootingTab = doneshootingTab
toTranslation DoneshootingBackupTab = doneshootingBackuptab
toTranslation PhotographersTab = photographersTab
toTranslation CamerasTab = cameraTab
toTranslation ShootingsTab = shootingTab
toTranslation SessionsTab = sessionTab
toTranslation LocationTab = locationTab
toTranslation MainTab = mainTab
toTranslation ControlTab = controlTab
toTranslation InsertPhotographeeTab = insertPhotographeeTab
