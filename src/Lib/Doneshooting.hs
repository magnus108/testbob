{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Doneshooting
    ( Doneshooting(..)
    , count
    , DoneshootingDir(..)
    , DoneshootingDirModel(..)
    , getDoneshootingDir
    , initialStateDir
    , Model(..)
    , getDoneshooting'
    , getDoneshooting
    , writeDoneshooting
    , initialState
    ) where

import System.FilePath
import Control.Exception
import Control.Concurrent.MVar.Strict
import Control.DeepSeq

import Lib.Data
import Utils.Comonad
import System.Directory

import qualified Lib.Grade as Grade
import qualified Lib.Camera as Camera
import qualified Lib.Location as Location

import Control.Lens

newtype Doneshooting = Doneshooting { unDoneshooting :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON, NFData)


newtype Model = Model { unModel :: Data String Doneshooting }
    deriving (Show, NFData)

makeLenses ''Model

initialState :: Model
initialState = Model NotAsked

getDoneshooting' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Doneshooting)
getDoneshooting' = readJSONFile'


writeDoneshooting' :: (MonadIO m) => FilePath -> Doneshooting -> m ()
writeDoneshooting' = writeJSONFile


--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> Doneshooting -> m ()
write file dagsdato' = liftIO $ withMVar file $ \f -> writeDoneshooting' f dagsdato'


--TODO could handle error on write.
writeDoneshooting :: (MonadIO m) => MVar FilePath -> Doneshooting -> m ()
writeDoneshooting file dagsdato' = liftIO $ (write file dagsdato') 


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Doneshooting)
read file = liftIO $ withMVar file $ \f -> do
        file' <- getDoneshooting' f
        case file' of
          Left e -> return $ Left e
          Right string -> do
                isDir <- doesDirectoryExist (unDoneshooting string)
                if isDir then
                    return $ Right string
                else
                    return $ Left "Er ikke mappe"
            


getDoneshooting :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Doneshooting)
getDoneshooting file = liftIO $ read file

--
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------



newtype DoneshootingDir = DoneshootingDir { unDoneshootingDir :: ([FilePath], ([FilePath], FilePath))}
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON, NFData)



getDoneshootingFiles :: Doneshooting -> Camera.Camera -> Location.LocationFile -> Grade.Grades -> IO (Either String DoneshootingDir)
getDoneshootingFiles doneshooting camera loc grades = do
    let location = takeBaseName $ Location.unLocationFile $ loc
    let extension = snd $ Camera.toExtension camera
    let filepath = unDoneshooting doneshooting
    let grade = Grade.showGrade grades
    --mangler GRADE
    let path = filepath </> location </> extension </> grade 
    _ <- doesDirectoryExist path
    files <- try $ listDirectory (filepath </> location </> extension </> grade ) :: IO (Either SomeException [FilePath])
    case files of
        Left _ ->
            return $ Right $ DoneshootingDir ( [], ([] , path))
        Right files' ->
            return $ Right $ DoneshootingDir ( filter (\file' -> ("." ++ extension) == (takeExtension file')) files'
                                                , (filter (\file' -> ".xmp" == (takeExtension file')) files'
                                                , path)
                                                )


getDoneshootingDir' :: (MonadIO m, MonadThrow m) => Doneshooting -> Camera.Camera -> Location.LocationFile -> Grade.Grades -> m (Either String DoneshootingDir)
getDoneshootingDir' doneshooting camera loc grades = do
    dir <- liftIO $ getDoneshootingFiles doneshooting camera loc grades
    return $ dir


newtype DoneshootingDirModel = DoneshootingDirModel { unDoneshootingDirModel :: Data String DoneshootingDir }
    deriving (Show, NFData)

count :: DoneshootingDir -> Int
count = length . fst . unDoneshootingDir


initialStateDir :: DoneshootingDirModel
initialStateDir = DoneshootingDirModel NotAsked


readDir :: (MonadIO m, MonadThrow m) => MVar FilePath -> MVar FilePath -> MVar FilePath -> MVar FilePath -> m (Either String DoneshootingDir)
readDir file mCamerasFile mLocationConfigFile mGradesFile = do
    grades <- Grade.getGrades mGradesFile 
    case grades of
        Left yyy -> return $ Left yyy
        Right grades' -> do
            loc <- Location.getLocationFile mLocationConfigFile
            case loc of
                Left xxx -> return $ Left xxx
                Right loc' -> 
                    liftIO $ withMVar file $ \f -> do
                        cameras <- Camera.read mCamerasFile
                        case cameras of
                                Left x -> return $ Left x
                                Right cameras' -> do
                                    doneshootingPath <- getDoneshooting' f --TODO fix this shit
                                    case doneshootingPath of
                                            Left x -> return $ Left x
                                            Right ff -> getDoneshootingDir' ff (extract (Camera.unCameras cameras')) loc' grades'


getDoneshootingDir :: (MonadIO m, MonadThrow m) => MVar FilePath -> MVar FilePath -> MVar FilePath -> MVar FilePath -> m (Either String DoneshootingDir)
getDoneshootingDir file mCamerasFile mLocationConfigFile mGradesFile = liftIO $ readDir file mCamerasFile mLocationConfigFile mGradesFile 
