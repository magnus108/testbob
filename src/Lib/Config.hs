{-# LANGUAGE DeriveAnyClass #-}
module Lib.Config
       ( Config (..)
       , loadConfig
       ) where

data Config = Config
    { dumpFile :: !FilePath
    , doneshootingFile :: !FilePath
    , dagsdatoFile :: !FilePath
    , dagsdatoBackupFile :: !FilePath
    , shootingsFile :: !FilePath
    , sessionsFile :: !FilePath
    , photographersFile :: !FilePath
    , gradesFile :: !FilePath
    , camerasFile :: !FilePath
    , tabsFile :: !FilePath
    , locationConfigFile :: !FilePath
    , translationFile :: !FilePath
    , photograheesFile :: !FilePath
    , buildFile :: !FilePath
    } deriving (Generic)
      deriving (FromJSON, ToJSON)
      deriving (Show)

loadConfig :: (MonadIO m, MonadThrow m) => FilePath -> m Config
loadConfig = readJSONFile
