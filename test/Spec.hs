import qualified Data.List.Index
import System.Directory
import qualified Control.Lens as Lens
import qualified Lib.Grade as Grade
import qualified Lib.Session as Session
import qualified Lib.Shooting as Shooting
import qualified Lib.Dump as Dump
import qualified Lib.Build as Build
import qualified Lib.Dagsdato as Dagsdato
import qualified Lib.Location as Location
import qualified Lib.Doneshooting as Doneshooting
import qualified Lib.DagsdatoBackup as DagsdatoBackup
import qualified Lib.Photographer as Photographer
import qualified Lib.Photographee as Photographee
import qualified Lib.Main as Main
import qualified Lib.Camera as Camera

import qualified Lib.Server.Build as SBuild

import Relude.Unsafe

import Data.Time.Calendar
import Data.Time.Format
import Data.Time.Clock

import Test.Tasty
import Test.Tasty.Golden

import Development.Shake.FilePath

import qualified Utils.RoseTree as RT
import qualified Utils.TreeZipper as TZ
import Utils.Comonad

import qualified Lib.App as App
import Lib.Config (loadConfig)
import Lib (mkEnv)

import qualified Data.ByteString.Lazy as LBS

import Development.Shake
import Development.Shake.FilePath

main :: IO ()
main = loadConfig "test/config.json" >>= mkEnv >>= run


run :: App.Env -> IO ()
run env@App.Env{..} = do
    photographers <- Photographer.getPhotographers mPhotographersFile
    let photographer = extract . Photographer.unPhotographers <$> photographers
    dump <- Dump.getDump mDumpFile
    dagsdato <- Dagsdato.getDagsdato mDagsdatoFile
    dagsdatoBackup <- DagsdatoBackup.getDagsdatoBackup mDagsdatoBackupFile
    doneshooting <- Doneshooting.getDoneshooting mDoneshootingFile
    cameras <- Camera.getCameras mCamerasFile
    let camera = extract . Camera.unCameras <$> cameras
    shootings <- Shooting.getShootings mShootingsFile
    let shooting = extract . Shooting.unShootings <$> shootings
    sessions <- Session.getSessions mSessionsFile
    let session = (\(Session.Sessions sessions) -> case sessions of
                            (TZ.TreeZipper (RT.Leaf x) _) -> do
                                Right x
                            (TZ.TreeZipper (RT.Branch _ _) _) -> do
                                Left "Session"
                                ) =<< sessions

    grades <- Grade.getGrades mGradesFile
    photographees <- Photographee.getPhotographees mPhotographeesFile
    location <- Location.getLocationFile mLocationConfigFile
    dumpDir <- Dump.getDumpDir mDumpFile mCamerasFile
    build <- Build.getBuild mBuildFile

    let item = Main.Item <$> location
                         <*> grades
                         <*> dump
                         <*> dumpDir
                         <*> photographees
                         <*> session
                         <*> camera
                         <*> dagsdato
                         <*> shooting
                         <*> doneshooting
                         <*> photographer
                         <*> dagsdatoBackup
                         <*> build

    golden <- mapM (goldenTests env) item
    defaultMain $ testGroup "tests" [ fromJust (rightToMaybe golden) ]


shakeDir :: FilePath
shakeDir = "test/._build"

opts :: ShakeOptions
opts = shakeOptions { shakeFiles = shakeDir }



check date item = Data.List.Index.imap (\index' cr -> 
        let 
                                            root = Dump.unDump dump
                                            index = index' + 1
                                            jpg = cr -<.> "jpg"
                                            doneshootingCr = SBuild.mkDoneshootingPath index cr item
                                            doneshootingJpg = SBuild.mkDoneshootingPathJpg index jpg item
                                            dagsdatoCr = SBuild.mkDagsdatoPath cr date item
                                            dagsdatoJpg = SBuild.mkDagsdatoPath jpg date item
                                            dagsdatoBackupCr = SBuild.mkDagsdatoBackupPath cr date item
                                            dagsdatoBackupJpg = SBuild.mkDagsdatoBackupPath jpg date item
                                            dump = Lens.view Main.dump item
        in
            (doneshootingCr, doneshootingJpg, dagsdatoCr, dagsdatoJpg, dagsdatoBackupCr, dagsdatoBackupJpg)
            ) (sort (Dump.unDumpDir dumpDir))
        where 
            dumpDir = Lens.view Main.dumpDir item


goldenTests :: App.Env -> Main.Item -> IO TestTree
goldenTests App.Env{..} item@Main.Item{..} = do
        writeFile "test/dump/test.cr2" ""
        writeFile "test/dump/test.jpg" ""
        
        doneShootingFiles <- listDirectory "test/dump"
        traceShowM doneShootingFiles


        let time = fromGregorian 2009 12 31
        let date = SBuild.getDate (UTCTime time (secondsToDiffTime 0))
        let photographees = Lens.view Main.photographees item
        let photographee = extract (Photographee.unPhotographees photographees)
        let ident = Lens.view Photographee.ident photographee
        let goldenDir = "test" </> ident

        let dump = Lens.view Main.dump item
        let dumpDir = Lens.view Main.dumpDir item

        let lol = check date item
        traceShowM lol
        
        _ <- SBuild.myShake' opts date item


        return $ testGroup "all files moved" $ []
            {-
        return $ testGroup "all files moved" $ 
                [ goldenVsString
                    (takeBaseName file)
                    goldenFile
                    (LBS.readFile file)
                | file <- fmap (\x -> doneshootingPath </> x) doneShootingFiles --could be nicer
                , let goldenFile = replaceDirectory file goldenDir
                ] 
                -}
