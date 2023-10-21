{-# LANGUAGE ScopedTypeVariables #-}
module Lib.Server.Build
    ( entry
    , getDate
    , mkDagsdatoBackupPath
    , mkDagsdatoPath
    , mkDoneshootingPath
    , mkDoneshootingPathJpg
    ) where

import qualified System.Directory as SD
import System.Directory
import qualified Lib.App as App

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Chan.Strict as Chan

import Data.Char

import Control.Exception

import Utils.Mealy

import System.Time.Extra


import qualified Data.List.Index
import Data.Strings

import Utils.Comonad

import qualified Lib.Main as Main
import qualified Control.Lens as Lens

import qualified Lib.Build as Build
import qualified Lib.Shooting as Shooting
import qualified Lib.Photographer as Photographer
import qualified Lib.Photographee as Photographee
import qualified Lib.Session as Session
import qualified Lib.Location as Location
import qualified Lib.Grade as Grade
import qualified Lib.Dump as Dump
import qualified Lib.Doneshooting as Doneshooting
import qualified Lib.Dagsdato as Dagsdato
import qualified Lib.DagsdatoBackup as DagsdatoBackup
import qualified Lib.Camera as Camera

import Development.Shake
import Development.Shake.FilePath 
import qualified Development.Shake.FilePath as FP

import Data.Time.Format
import Data.Time.Clock










shakeDir :: FilePath
shakeDir = "._build"

getDate :: UTCTime -> String
getDate = formatTime defaultTimeLocale "%Y - %m%d"


message :: Mealy (Double, Progress) (Double, Progress) -> Mealy (Double, Progress) (Double, Int)
message input = liftA2 (,) done todo
    where
        progress = snd <$> input
        _ = fst <$> input
        done = timeBuilt <$> progress
        todo = countBuilt <$> progress



myProgressProgram :: Int -> Chan.Chan App.Action -> Photographee.Photographee -> IO Progress -> IO ()
myProgressProgram sample c photographee progress = do
    time <- offsetTime
    catchJust (\x -> if x == ThreadKilled then Just () else Nothing)
        (loop time $ message echoMealy)
        (const $ do _ <- time
                    p <- progress
                    let todo = countBuilt p
                    Chan.writeChan c (App.BuilderMessage (Build.DoneBuild photographee (show (div todo 8))))
        )
    where
        loop :: IO Double -> Mealy (Double, Progress) (Double, Int) -> IO ()
        loop time mealy = do
            threadDelay sample
            t <- time
            p <- progress
            ((_,todo), mealyy) <- pure $ runMealy mealy (t, p)
            let f = isFailure p
            case f of
                Nothing -> do
                    Chan.writeChan c (App.BuilderMessage (Build.Building photographee (show (div todo 8))))
                Just _ ->
                    Chan.writeChan c (App.BuilderMessage (Build.NoBuild))
            loop time mealyy


opts :: Chan.Chan App.Action -> Photographee.Photographee -> ShakeOptions
opts  c photographee = shakeOptions
                    { shakeFiles = shakeDir
                    , shakeProgress = progress -- should change
                    , shakeThreads = 0
                    }
    where
        progress p = do
            myProgressProgram 1000000 c photographee p

gradePath :: Main.Item -> FilePath
gradePath item =
    Doneshooting.unDoneshooting doneshooting </> location </> extension </> grade
        where
            location = takeBaseName $ Location.unLocationFile $ Lens.view Main.location item
            session = Lens.view Main.session item
            sessionId = show $ Session.toInteger session

            camera = Lens.view Main.camera item
            extension = snd $ Camera.toExtension camera
            photographer = Lens.view Main.photographer item
            photographerId = Lens.view Photographer.tid photographer
            doneshooting = Lens.view Main.doneshooting item
            shooting = Lens.view Main.shooting item
            shootingId = if isKindergartenGroup session then "3" else show $ Shooting.toInteger shooting
            grade = Grade.showGrade (Lens.view Main.grades item)
            photographees = Lens.view Main.photographees item
            photographee = extract (Photographee.unPhotographees photographees)
            tea = Photographee.toTea' photographee

isKindergartenGroup (Session.KindergartenGroup _) = True
isKindergartenGroup _ = False

mkDoneshootingPath :: Int -> FilePath -> Main.Item -> FilePath
mkDoneshootingPath index' file item =
    Doneshooting.unDoneshooting doneshooting </> location </> extension </> grade </> sessionId ++ "." ++ tea ++ "." ++ shootingId ++ "." ++ photographerId ++ "." ++ no ++ (toLower <$> (FP.takeExtension file))
        where
            location = takeBaseName $ Location.unLocationFile $ Lens.view Main.location item
            session = Lens.view Main.session item
            sessionId = show $ Session.toInteger session

            camera = Lens.view Main.camera item
            extension = snd $ Camera.toExtension camera
            photographer = Lens.view Main.photographer item
            photographerId = Lens.view Photographer.tid photographer
            doneshooting = Lens.view Main.doneshooting item
            shooting = Lens.view Main.shooting item
            shootingId = if isKindergartenGroup session then "3" else show $ Shooting.toInteger shooting
            grade = Grade.showGrade (Lens.view Main.grades item)
            photographees = Lens.view Main.photographees item
            photographee = extract (Photographee.unPhotographees photographees)
            tea = Photographee.toTea' photographee
            pad x = strPadLeft '0' 3 (show x)
            no = pad index'


mkDoneshootingPathJpg :: Int -> FilePath -> Main.Item -> FilePath
mkDoneshootingPathJpg index' file item =
    Doneshooting.unDoneshooting doneshooting </> location </> extension </> "_webshop" </> sessionId ++ "." ++ tea ++ "." ++ shootingId ++ "." ++ photographerId ++ "." ++ no ++ (toLower <$> (FP.takeExtension file))
        where
            location = takeBaseName $ Location.unLocationFile $ Lens.view Main.location item
            session = Lens.view Main.session item
            sessionId = show $ Session.toInteger session

            camera = Lens.view Main.camera item
            extension = snd $ Camera.toExtension camera
            photographer = Lens.view Main.photographer item
            photographerId = Lens.view Photographer.tid photographer
            doneshooting = Lens.view Main.doneshooting item
            shooting = Lens.view Main.shooting item
            shootingId = if isKindergartenGroup session then "3" else show $ Shooting.toInteger shooting
            photographees = Lens.view Main.photographees item
            photographee = extract (Photographee.unPhotographees photographees)
            tea = Photographee.toTea' photographee
            pad x = strPadLeft '0' 3 (show x)
            no = pad index'


mkDagsdatoPath :: FilePath -> String -> Main.Item -> FilePath
mkDagsdatoPath file date item = dagsdato </> date ++ " - " ++ location </> grade </> (name ++ " - " ++ tea) </> file -<.> (toLower <$> (FP.takeExtension file))
        where
            dagsdato = Dagsdato.unDagsdato $ Lens.view Main.dagsdato item
            location = takeBaseName $ Location.unLocationFile $ Lens.view Main.location item
            grade = Grade.showGrade (Lens.view Main.grades item)
            photographees = Lens.view Main.photographees item
            photographee = extract (Photographee.unPhotographees photographees)
            tea = Photographee.toTea' photographee
            name = Photographee.toName' photographee


mkDagsdatoBackupPath :: FilePath -> String -> Main.Item -> FilePath
mkDagsdatoBackupPath file date item = dagsdatoBackup </> date ++ " - " ++ location </> grade </> (name ++ " - " ++ tea) </> file -<.> (toLower <$> (FP.takeExtension file))
        where
            dagsdatoBackup = DagsdatoBackup.unDagsdatoBackup $ Lens.view Main.dagsdatoBackup item
            location = takeBaseName $ Location.unLocationFile $ Lens.view Main.location item
            grade = Grade.showGrade (Lens.view Main.grades item)
            photographees = Lens.view Main.photographees item
            photographee = extract (Photographee.unPhotographees photographees)
            name   = Photographee.toName' photographee
            tea   = Photographee.toTea' photographee


entry :: Chan.Chan App.Action -> MVar FilePath -> MVar FilePath -> Main.Item -> IO ()
entry messages mBuildFile mDumpFile item = do
    time <- getCurrentTime
    let date = getDate time

    let photographees = Lens.view Main.photographees item
    let photographee = extract (Photographee.unPhotographees photographees)

    shaken <- try $ myShake (opts messages photographee) date item :: IO (Either SomeException ())
    case shaken of
        Left e ->  do
            case show e of
                "user error (missingjpg)" ->  do
                    Chan.writeChan messages (App.BuilderMessage (Build.NoJpgBuild))
                _ ->
                    Chan.writeChan messages (App.BuilderMessage (Build.NoBuild))
        Right _ -> do
            Chan.writeChan messages (App.BuilderMessage (Build.DoneBuild photographee ("")))
            --HACK
            let dump = Lens.view Main.dump item
            Chan.writeChan messages (App.WriteDump dump)


myShake :: ShakeOptions -> String -> Main.Item -> IO ()
myShake opts' time item = do
    let dump = Lens.view Main.dump item
    let root = Dump.unDump dump
    let dumpDir = Lens.view Main.dumpDir item
    let sortDir = sort (Dump.unDumpDir dumpDir)
    let pathCR = gradePath item
    files <- try $ listDirectory pathCR :: IO (Either SomeException [FilePath])
    --traceShowM files
    let count = case files of
            Left e -> []
            Right filess -> filess

    let iindex = length count

    -- læs her.

    tmp <- case sortDir  of
            [] -> error "empty"
            xss ->  Data.List.Index.imapM (\index' cr -> do 

                let index'' = iindex + index' + 1
                let jpg = cr -<.> "jpg"
                eh <- SD.doesFileExist (root </> jpg)
                case eh of
                    True -> do
                        let doneshootingCr = mkDoneshootingPath index'' cr item
                        let doneshootingJpg = mkDoneshootingPathJpg index'' jpg item

                        let dagsdatoCr = mkDagsdatoPath cr time item
                        let dagsdatoJpg = mkDagsdatoPath jpg time item

                        let dagsdatoBackupCr = mkDagsdatoBackupPath cr time item
                        let dagsdatoBackupJpg = mkDagsdatoBackupPath jpg time item
                        return ((cr, (doneshootingCr,dagsdatoCr, dagsdatoBackupCr)), (jpg, (doneshootingJpg, dagsdatoJpg, dagsdatoBackupJpg)))
                    False -> fail "missingjpg"
                ) xss

    case tmp of
        [] -> error "empty"
        xs -> do
            shake opts' $ do
                forM_ xs $ \ ((cr, (doneshootingCr,dagsdatoCr, dagsdatoBackupCr)),(jpg, (doneshootingJpg, dagsdatoJpg, dagsdatoBackupJpg))) -> do

                    want [doneshootingCr, doneshootingJpg, dagsdatoCr, dagsdatoJpg , dagsdatoBackupCr, dagsdatoBackupJpg]

                    doneshootingCr %> copyFile' (root </> cr)

                    doneshootingJpg %> copyFile' (root </> jpg)

                    dagsdatoCr %> copyFile' (root </> cr)

                    dagsdatoJpg %> copyFile' (root </> jpg)

                    dagsdatoBackupCr %> copyFile' (root </> cr)

                    dagsdatoBackupJpg %> copyFile' (root </> jpg)

                    action $ removeFilesAfter root ["//*.CR3", "//*.JPG", "//*.cr3", "//*.jpg","//*.CR2","//*.cr2"]
