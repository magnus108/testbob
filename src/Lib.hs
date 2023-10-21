{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( mkEnv
    , runServer
    , main
    ) where

import Control.DeepSeq

import qualified Lib.Server.Build as SBuild
import Lib.Data

import Relude.Unsafe (fromJust)

import qualified Graphics.UI.Threepenny as UI

import Control.Exception (SomeException(..), catch)

import System.FilePath
import qualified Control.Concurrent as TT
import Control.Concurrent.MVar.Strict (modifyMVar_)
import qualified Data.HashMap.Strict as HashMap
import System.FSNotify
import qualified Control.Concurrent.Chan.Strict as Chan

import qualified Lib.App as App

import Lib.App (Env(..),Action(..))
import Lib.Config (Config (..), loadConfig)
import Lib.Tab (Tabs, getTabs)
import Lib.Photographer (getPhotographers)

import Utils.ListZipper

import qualified Lib.Build as Build
import qualified Lib.Translation as Translation
import qualified Lib.Photographee as Photographee
import Lib.Dagsdato
import Lib.DagsdatoBackup
import Lib.Doneshooting
import Lib.Dump
import qualified Lib.Grade as Grade
import qualified Lib.Session as Session
import qualified Lib.Shooting as Shooting
import qualified Lib.Dump as Dump
import qualified Lib.Dagsdato as Dagsdato
import qualified Lib.Location as Location
import qualified Lib.Doneshooting as Doneshooting
import qualified Lib.DagsdatoBackup as DagsdatoBackup
import qualified Lib.Photographer as Photographer
import qualified Lib.Camera as Camera

import qualified Lib.Server.Server as Server

import Graphics.UI.Threepenny (newEvent, Handler)

import Control.Concurrent (forkIO) 

mkEnv :: FilePath -> Config -> IO Env
mkEnv root' Config{..} = do
    let root = root' </> "config"
    let serverRoot = root'

    chan <- Chan.newChan

    mPhotographersFile <- newMVar (root </> photographersFile)
    mGradesFile <- newMVar (root </> gradesFile)
    mDumpFile <- newMVar (root </> dumpFile)
    mDagsdatoFile <- newMVar (root </> dagsdatoFile)
    mDagsdatoBackupFile <- newMVar (root </> dagsdatoBackupFile)
    mDoneshootingFile <- newMVar (root </> doneshootingFile)
    mCamerasFile <- newMVar (root </> camerasFile)
    mShootingsFile <- newMVar (root </> shootingsFile)
    mSessionsFile <- newMVar (root </> sessionsFile)
    mLocationConfigFile <- newMVar (root </> locationConfigFile)

    mTranslationFile <- newMVar (root </> translationFile)

    mPhotographeesFile <- newMVar (root </> photograheesFile)

    mBuildFile <- newMVar (root </> buildFile)

    --ROOT'.....
    let tabs = (root' </> tabsFile)
    mTabsFile <- newMVar tabs
    pure Env{..}


runServer :: Int -> Env -> IO ()
runServer port env@Env{..} = do
    (eDirDoneshooting, hDirDoneshooting) <- newEvent
    (eConfigDoneshooting, hConfigDoneshooting) <- newEvent

    (_, hDirDagsdato) <- newEvent
    (eConfigDagsdato, hConfigDagsdato) <- newEvent

    (_, hDirDagsdatoBackup) <- newEvent
    (eConfigDagsdatoBackup, hConfigDagsdatoBackup) <- newEvent

    (eDumpDir, hDumpDir) <- newEvent
    (eConfigDump, hConfigDump) <- newEvent

    (eTabs, hTab) <- newEvent
    (ePhotographers, hPhotographers) <- newEvent

    (eCameras, hCameras) <- newEvent

    (eShootings, hShootings) <- newEvent

    (eSessions, hSessions) <- newEvent

    (eGrades, hGrades) <- newEvent
    (eLocationConfigFile, hLocationConfigFile) <- newEvent

    (ePhotographees, hPhotographees) <- newEvent

    (eBuild, hBuild) <- newEvent

    watchers <- newMVar mempty
    withManagerConf ( defaultConfig { confDebounce = Debounce ( 0.001) })$ \mgr -> do
        --Build
        stopBuild <- build env mgr mBuildFile watchers hBuild

        --Photographers
        stopConfigPhotographers <- configPhotographers env mgr mPhotographersFile watchers hPhotographers

        --Grades
        stopGrades <- grades env mgr mGradesFile mLocationConfigFile mPhotographeesFile mDoneshootingFile mCamerasFile watchers hGrades hDirDoneshooting

        --Grades
        stopPhotographees <- photographees env mgr mPhotographeesFile watchers hPhotographees

        --Dump
        stopConfigDump <- configDump env mgr mDumpFile mCamerasFile watchers hConfigDump hDumpDir
        stopDirDump <- dirDump env mgr mDumpFile mCamerasFile watchers hDumpDir

        --Dagsdato
        stopConfigDagsdato <- configDagsdato env mgr mDagsdatoFile watchers hConfigDagsdato hDirDagsdato
        stopDirDagsdato <- dirDagsdato env mgr mDagsdatoFile watchers hDirDagsdato

        --Dagsdato backup
        stopConfigDagsdatoBackup <- configDagsdatoBackup env mgr mDagsdatoBackupFile watchers hConfigDagsdatoBackup hDirDagsdatoBackup
        stopDirDagsdatoBackup <- dirDagsdatoBackup env mgr mDagsdatoBackupFile watchers hDirDagsdatoBackup

        --Doneshooting
        stopConfigDoneshooting <- configDoneshooting env mgr mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile watchers hConfigDoneshooting hDirDoneshooting
        stopDirDoneshooting <- dirDoneshooting env mgr mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile watchers hDirDoneshooting

        --Cameras
        stopConfigCameras <- configCameras env mgr mCamerasFile mLocationConfigFile mDumpFile mDoneshootingFile mGradesFile watchers hCameras hDumpDir hDirDoneshooting

        --Shootings
        stopConfigShootings <- configShootings env mgr mShootingsFile watchers hShootings

        --Sessions
        stopConfigSessions <- configSessions env mgr mSessionsFile watchers hSessions

        --Location
        stopConfigLocationFile <- configLocationFile env mgr mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile watchers hLocationConfigFile hDirDoneshooting

        --Tabs
        stopConfigTab <- configTab env mgr mTabsFile watchers hTab

        --TODO setter
        TT.modifyMVar_ watchers $ \_ -> do
            return $ HashMap.fromList
                [("stopConfigTab", stopConfigTab )

                ,("stopConfigLocationFile", stopConfigLocationFile)

                ,("stopBuild", stopBuild)
                ,("stopGrades", stopGrades)

                ,("stopConfigPhotographers", stopConfigPhotographers)

                ,("stopConfigCameras", stopConfigCameras)

                ,("stopConfigShootings", stopConfigShootings)

                ,("stopConfigSessions", stopConfigSessions)
                
                ,("stopConfigDoneshooting", stopConfigDoneshooting)
                ,("stopDirDoneshooting", stopDirDoneshooting)

                ,("stopConfigDagsdato", stopConfigDagsdato)
                ,("stopDirDagsdato", stopDirDagsdato)

                ,("stopConfigDagsdatoBackup", stopConfigDagsdatoBackup)
                ,("stopDirDagsdatoBackup", stopDirDagsdatoBackup)

                ,("stopConfigDump", stopConfigDump)
                ,("stopDirDump", stopDirDump)
                ,("stopPhotographees", stopPhotographees)
                ]

        --Photographers
        bBuild <- UI.stepper Build.initalState eBuild
        Chan.writeChan chan SBuild

        --Photographers
        bPhotographers <- UI.stepper Photographer.initalState ePhotographers
        Chan.writeChan chan ReadPhographers

        --Dump
        bDump <- UI.stepper Dump.initalState eConfigDump
        Chan.writeChan chan ReadDump


        --Dagsdato
        bDagsdato <- UI.stepper Dagsdato.initialState eConfigDagsdato
        Chan.writeChan chan ReadDagsdato


        --DagsdatoBackup
        bDagsdatoBackup <- UI.stepper DagsdatoBackup.initialState eConfigDagsdatoBackup
        Chan.writeChan chan ReadDagsdatoBackup

        -- Doneshooting
        bDoneshooting <- UI.stepper Doneshooting.initialState eConfigDoneshooting
        Chan.writeChan chan ReadDoneshooting


        bDoneshootingDir <- UI.stepper Doneshooting.initialStateDir eDirDoneshooting
        Chan.writeChan chan ReadDoneshootingDir

        -- Cameras
        bCameras <- UI.stepper Camera.initalState eCameras
        Chan.writeChan chan ReadCamera

    
        -- Shootings
        bShootings <- UI.stepper Shooting.initialState eShootings
        Chan.writeChan chan ReadShooting

        -- Sessions
        bSessions <- UI.stepper Session.initialState eSessions
        Chan.writeChan chan ReadSessions

        -- Grades
        bGrades <- UI.stepper Grade.initialState eGrades
        Chan.writeChan chan ReadGrades


        -- Photographees this is REAL BAD AS it does not wrk with bLocationConfigFile as expected
        bPhotographees <- UI.stepper Photographee.initialState ePhotographees
        Chan.writeChan chan ReadPhographees


        -- Location
        bLocationConfigFile <- UI.stepper Location.initialState eLocationConfigFile
        Chan.writeChan chan ReadLocation

        -- DumpDir
        bDumpDir <- UI.stepper Dump.initalStateDir eDumpDir
        Chan.writeChan chan ReadDumpDir


        translations <- Translation.read mTranslationFile
        --VERY important this is here.. BADNESS FIX AT THE END

        receive <- liftIO $ forkIO $ receiveMessages env mgr watchers hPhotographers hConfigDump hConfigDagsdato hConfigDagsdatoBackup hConfigDoneshooting hDirDoneshooting hCameras hShootings hSessions hGrades hPhotographees hLocationConfigFile hDumpDir hDirDagsdatoBackup  hDirDagsdato hBuild hTab chan

        Server.run port env (fromJust (rightToMaybe translations)) bDoneshootingDir bBuild bGrades bLocationConfigFile bSessions bShootings bCameras bDump bDumpDir bDoneshooting bDagsdato bDagsdatoBackup eTabs bPhotographers bPhotographees receive


receiveMessages :: Env -> WatchManager -> WatchMap -> Handler Photographer.Model -> Handler Dump.DumpModel -> Handler Dagsdato.Model -> Handler DagsdatoBackup.Model -> Handler Doneshooting.Model -> Handler Doneshooting.DoneshootingDirModel -> Handler Camera.Model -> Handler Shooting.Model -> Handler Session.Model -> Handler Grade.Model -> Handler Photographee.Model -> Handler Location.Model -> Handler Dump.DumpDirModel -> Handler () -> Handler () -> Handler Build.Model -> Handler Tabs -> Chan.Chan App.Action -> IO ()
receiveMessages env@Env{..} mgr watchMap hPhotographers hConfigDump hConfigDagsdato hConfigDagsdatoBackup hConfigDoneshooting  hDirDoneshooting  hCameras hShootings  hSessions hGrades hPhotographees hLocationConfigFile  hDumpDir hDirDagsdatoBackup hDirDagsdato hBuild hTab msgs = do
    messages <- Chan.getChanContents msgs
    forM_ messages $ \msg -> do
        traceShowM msg
        case msg of
            ReadPhographers ->
                getPhotographers mPhotographersFile >>= \case
                        Left e' -> hPhotographers $ Photographer.Model (Failure e')
                        Right s -> hPhotographers $ Photographer.Model (Data s)

            WritePhographers photographers' ->
                Photographer.writePhotographers mPhotographersFile $ photographers'

            ReadDump -> do 
                Dump.getDump mDumpFile >>= \case
                    Left e' -> hConfigDump $ Dump.DumpModel (Failure e')
                    Right s -> hConfigDump $ Dump.DumpModel (Data s)

                Dump.getDumpDir mDumpFile mCamerasFile >>= \case
                        Left e' -> hDumpDir $ DumpDirModel (Failure e')
                        Right s -> hDumpDir $ DumpDirModel (Data s)


            WriteDump dir -> 
                writeDump mDumpFile $ dir

            ReadDagsdato ->
                Dagsdato.getDagsdato mDagsdatoFile >>= \case
                        Left e' -> hConfigDagsdato $ Dagsdato.Model (Failure e')
                        Right s -> hConfigDagsdato $ Dagsdato.Model (Data s)

            WriteDagsdato dagsdato' ->
                writeDagsdato mDagsdatoFile $ dagsdato'

            ReadDagsdatoBackup ->
                DagsdatoBackup.getDagsdatoBackup mDagsdatoBackupFile  >>= \case
                        Left e' -> hConfigDagsdatoBackup $ DagsdatoBackup.Model (Failure e')
                        Right s -> hConfigDagsdatoBackup $ DagsdatoBackup.Model (Data s)

            WriteDagsdatoBackup dagsdatoBackup' ->
                writeDagsdatoBackup mDagsdatoBackupFile $ dagsdatoBackup'

            ReadDoneshooting ->
                Doneshooting.getDoneshooting mDoneshootingFile >>= \case
                    Left e' -> hConfigDoneshooting $ Doneshooting.Model (Failure e')
                    Right s -> hConfigDoneshooting $ Doneshooting.Model (Data s)

            WriteDoneshooting dir ->
                writeDoneshooting mDoneshootingFile $ dir

            ReadDoneshootingDir ->
                Doneshooting.getDoneshootingDir mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile >>= \case
                    Left e' -> hDirDoneshooting $ Doneshooting.DoneshootingDirModel (Failure e')
                    Right s -> hDirDoneshooting $ Doneshooting.DoneshootingDirModel (Data s)

            ReadCamera ->
                    Camera.getCameras mCamerasFile >>= \case
                        Left e' -> hCameras $ Camera.Model (Failure e')
                        Right s -> hCameras $ Camera.Model (Data s)

            WriteCamera cameras' ->
                Camera.writeCameras mCamerasFile $ cameras'

            ReadShooting ->
                Shooting.getShootings mShootingsFile >>= \case
                    Left e' -> hShootings $ Shooting.Model (Failure e')
                    Right s -> hShootings $ Shooting.Model (Data s)

            WriteShooting shootings' ->
                Shooting.writeShootings mShootingsFile $ shootings'

            ReadSessions ->
                Session.getSessions mSessionsFile >>= \case
                    Left e' -> hSessions $ Session.Model (Failure e')
                    Right s -> hSessions $ Session.Model (Data s)

            WriteSessions sessions' ->
                Session.writeSessions mSessionsFile $ sessions'

            ReadGrades ->
                Grade.getGrades mGradesFile >>= \case
                    Left e' -> hGrades $ Grade.Model $ Failure e'
                    Right s -> hGrades $ Grade.Model $ Data s

            WriteGrades grades' ->
                Grade.writeGrades mGradesFile $ grades'

            ReadPhographees ->
                Photographee.getPhotographees mPhotographeesFile >>= \case
                    Left e' -> hPhotographees $ Photographee.Model (Failure e')
                    Right s -> hPhotographees $ Photographee.Model (Data s)

            WritePhotographees photographees' dumpDir ->
                if (Dump.count dumpDir == 0) then
                    case (photographees') of
                       (Photographee.ChangedPhotographees _) ->
                            Photographee.writePhotographees mPhotographeesFile $ (Photographee.CorrectPhotographees (Photographee.toZip photographees'))
                       (Photographee.NotFoundPhotographees _) ->
                            Photographee.writePhotographees mPhotographeesFile $ (Photographee.NotFoundPhotographees (Photographee.toZip photographees'))
                       (Photographee.CorrectPhotographees _) ->
                            Photographee.writePhotographees mPhotographeesFile $ (Photographee.CorrectPhotographees (Photographee.toZip photographees'))
                else 
                    Photographee.writePhotographees mPhotographeesFile $ (Photographee.ChangedPhotographees (Photographee.toZip photographees'))

            WritePhotographeesOK photographees' ->
                Photographee.writePhotographees mPhotographeesFile $ (Photographee.CorrectPhotographees (Photographee.toZip photographees'))


            ReadLocation ->
                Location.getLocationFile mLocationConfigFile >>= \case
                        Left e' -> hLocationConfigFile $ Location.Model (Failure e')
                        Right s -> hLocationConfigFile $ Location.Model (Data s)

            WriteLocation loc -> 
                Location.writeLocationFile  mLocationConfigFile $ loc

            ReadDumpDir ->
                 Dump.getDumpDir mDumpFile mCamerasFile >>= \case
                        Left e' -> do
                            hDumpDir $ DumpDirModel (Failure e')
                        Right s -> do
                            hDumpDir $ DumpDirModel (Data s)

            SDirDagsdatoBackup -> hDirDagsdatoBackup ()
            SDirDagsdato -> hDirDagsdato ()

            SConfigLocationFile -> do
                filepath' <- readMVar mLocationConfigFile
                locationFile <- Location.getLocationFile' filepath'
                grades' <- mapM Photographee.parseGrades locationFile
                let grades'' = either (const (Grade.Grades (ListZipper [] (Grade.Unknown (Grade.Grade' "")) []))) id $! join grades'
                _ <- Location.getLocationFile mLocationConfigFile >>= \case
                    Left e' -> hLocationConfigFile $ Location.Model (Failure e')
                    Right s -> hLocationConfigFile $ Location.Model (Data s)

                getDoneshootingDir mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile >>= \case
                    Left e' -> hDirDoneshooting $ DoneshootingDirModel (Failure e')
                    Right s -> hDirDoneshooting $ DoneshootingDirModel (Data s)

                void $! Grade.writeGrades mGradesFile $ grades''

            SGrades -> do
                _ <- Grade.getGrades mGradesFile >>= \case
                        Left e' -> hGrades $ Grade.Model $ Failure e'
                        Right s -> hGrades $ Grade.Model $ Data s
                
                _ <- Photographee.reloadPhotographees mGradesFile mLocationConfigFile mPhotographeesFile

                getDoneshootingDir mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile >>= \case
                    Left e' -> hDirDoneshooting $ DoneshootingDirModel (Failure e')
                    Right s -> hDirDoneshooting $ DoneshootingDirModel (Data s)

            SConfigCameras -> do
                _ <- Camera.getCameras mCamerasFile >>= \case
                    Left e' -> hCameras $ Camera.Model (Failure e')
                    Right s -> hCameras $ Camera.Model (Data s)

                getDumpDir mDumpFile mCamerasFile >>= \case
                    Left e' -> hDumpDir $ DumpDirModel (Failure e')
                    Right s -> hDumpDir $ DumpDirModel (Data s)

                getDoneshootingDir mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile >>= \case
                    Left e' -> hDirDoneshooting $ DoneshootingDirModel (Failure e')
                    Right s -> hDirDoneshooting $ DoneshootingDirModel (Data s)
            SBuild -> do
                Build.getBuild mBuildFile >>= \case
                    Left e' -> hBuild $ Build.Model (Failure (e' ++ "Kunne ikke finde byg"))
                    Right s -> hBuild $ Build.Model (Data s)

            MFcker i ->
                --DANGEROUS 
                --DANGEROUS 
                --DANGEROUS 
                --DANGEROUS 
                --DANGEROUS 
                --DANGEROUS 
                --DANGEROUS 
                void $ forkIO $ SBuild.entry msgs mBuildFile mDumpFile $!! i
                --void $ SBuild.entry msgs mBuildFile mDumpFile $ i

            BuilderMessage msg ->
                Build.writeBuild mBuildFile $ msg

            STab -> do
                filepath' <- readMVar mTabsFile
                getTabs filepath'  >>= hTab



type WatchMap = MVar (HashMap String StopListening)


build :: Env -> WatchManager -> MVar FilePath -> WatchMap -> Handler Build.Model -> IO StopListening
build env mgr mBuildFile _ handler = do
    filepath <- readMVar mBuildFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> void $ do
            print e
            Chan.writeChan (chan env) SBuild
        )



configTab :: Env -> WatchManager -> MVar FilePath -> WatchMap -> Handler Tabs -> IO StopListening
configTab env mgr mTabsFile _ handler = do
    filepath <- readMVar mTabsFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            print e
            Chan.writeChan (chan env) STab
        )


configLocationFile :: Env -> WatchManager -> MVar FilePath -> MVar FilePath -> MVar FilePath -> MVar FilePath ->  WatchMap -> Handler Location.Model -> Handler (Doneshooting.DoneshootingDirModel) -> IO StopListening
configLocationFile env mgr mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile _ handler handleDonshootingDir = do
    filepath <- readMVar mLocationConfigFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            --TODO madness of baddness
            print e
            Chan.writeChan (chan env) SConfigLocationFile
        )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


-- der skal skydes et lag in herimellem der kan lytte på locationen
photographees :: Env -> WatchManager -> MVar FilePath -> WatchMap -> Handler Photographee.Model -> IO StopListening
photographees env mgr mPhotographeesFile _ handler = do
    filepath <- readMVar mPhotographeesFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> void $ do
            print e
            Chan.writeChan (chan env) ReadPhographees
        )`catch` (\( _ :: SomeException ) -> do
            -- ikke helt nok for den skal jo også laves på ny hvis den mangler
            ----------------------------------------------------- handler $ Photographee.Model (Failure "Der er en fejl med Photographees")
            return $ return () ) --TODO this sucks


-- der skal skydes et lag in herimellem der kan lytte på locationen
grades :: Env -> WatchManager -> MVar FilePath -> MVar FilePath -> MVar FilePath -> MVar FilePath -> MVar FilePath -> WatchMap -> Handler Grade.Model -> Handler DoneshootingDirModel -> IO StopListening
grades env mgr mGradesFile mLocationConfigFile mPhotographeesFile mDoneshootingFile mCamerasFile watchMap handler handleDonshootingDir = do
    filepath <- readMVar mGradesFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\_ -> void $ do
            Chan.writeChan (chan env) SGrades

            TT.modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopDirDoneshooting"
                stopDirDoneshooting <- dirDoneshooting env mgr mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile watchMap handleDonshootingDir
                return $ HashMap.insert "stopDirDoneshooting" stopDirDoneshooting  h

        )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


configPhotographers :: Env -> WatchManager -> MVar FilePath -> WatchMap -> Handler Photographer.Model -> IO StopListening
configPhotographers env mgr mFilepath _ handler = do
    filepath <- readMVar mFilepath
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        --TODO SPAWNER THREAD SOM IKKE DØR
        --TODO SPAWNER THREAD SOM IKKE DØR
        --TODO SPAWNER THREAD SOM IKKE DØR
        (\e -> void $ do
            print e 
            Chan.writeChan (chan env) ReadPhographers
        )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


configSessions :: Env -> WatchManager -> MVar FilePath -> WatchMap -> Handler Session.Model -> IO StopListening
configSessions env mgr mSessionsFile _ handler = do
    filepath <- readMVar mSessionsFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            print e 
            Chan.writeChan (chan env) ReadSessions
        )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


configCameras :: Env -> WatchManager -> MVar FilePath -> MVar FilePath -> MVar FilePath -> MVar FilePath -> MVar FilePath -> WatchMap -> Handler Camera.Model -> Handler (Dump.DumpDirModel) -> Handler (Doneshooting.DoneshootingDirModel) -> IO StopListening
configCameras env mgr mCamerasFile mLocationConfigFile mDumpFile mDoneshootingFile mGradesFile _ handler handleDumpDir handleDonshootingDir = do
    filepath <- readMVar mCamerasFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            print e
            Chan.writeChan (chan env) SConfigCameras
        )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


configShootings :: Env -> WatchManager -> MVar FilePath -> WatchMap -> Handler Shooting.Model -> IO StopListening
configShootings env mgr mShootingsFile _ handler = do
    filepath <- readMVar mShootingsFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            print e
            Chan.writeChan (chan env) ReadShooting
        )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


configDoneshooting :: Env -> WatchManager -> MVar FilePath -> MVar FilePath -> MVar FilePath -> MVar FilePath -> WatchMap -> Handler Doneshooting.Model -> Handler Doneshooting.DoneshootingDirModel -> IO StopListening
configDoneshooting env mgr mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile watchMap handler handleDonshootingDir = do
    filepath <- readMVar mDoneshootingFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            print e
            Chan.writeChan (chan env) ReadDoneshooting
            -- TODO these two are related
            TT.modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopDirDoneshooting"
                stopDirDoneshooting <- dirDoneshooting env mgr mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile watchMap handleDonshootingDir
                return $ HashMap.insert "stopDirDoneshooting" stopDirDoneshooting  h
        )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks



dirDoneshooting :: Env -> WatchManager -> MVar FilePath -> MVar FilePath -> MVar FilePath -> MVar FilePath -> WatchMap -> Handler Doneshooting.DoneshootingDirModel -> IO StopListening
dirDoneshooting env mgr mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile _ handler = do
    doneshootingFile <- readMVar mDoneshootingFile
    doneshootingPath <- getDoneshooting' doneshootingFile
    case doneshootingPath of
        Left _ -> return ( return ())
        Right path -> do
            watchTree ---BADNESS
            ---BADNESS
            ---BADNESS
            ---BADNESS
            ---BADNESS
            ---BADNESS
                mgr
                (unDoneshooting path)
                (\e -> (takeExtension (eventPath e)) /= ".tmp")
                (\e -> do
                    print e 
                    Chan.writeChan (chan env) ReadDoneshootingDir
                ) `catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


configDump :: Env -> WatchManager -> MVar FilePath -> MVar FilePath -> WatchMap -> Handler (Dump.DumpModel) -> Handler (Dump.DumpDirModel) -> IO StopListening
configDump env mgr mDumpFile mCamerasFile watchMap handler handleDumpDir = do
    filepath <- readMVar mDumpFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        --TODO SPAWNER THREAD SOM IKKE DØR
        --TODO SPAWNER THREAD SOM IKKE DØR
        --TODO SPAWNER THREAD SOM IKKE DØR
        (\e -> do
            print e
            Chan.writeChan (chan env) ReadDump
            TT.modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopDirDump"
                stopDirDump <- dirDump env mgr mDumpFile mCamerasFile watchMap handleDumpDir
                return $ HashMap.insert "stopDirDump" stopDirDump h

        )  `catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


dirDump :: Env -> WatchManager -> MVar FilePath -> MVar FilePath -> WatchMap -> Handler Dump.DumpDirModel -> IO StopListening
dirDump env mgr mDump mCamerasFile _ handler = do
    dumpFile <- readMVar mDump
    dumpPath <- getDump' dumpFile
    case dumpPath of
      Left _ -> return (return ()) -- TODO this sucks
      Right path -> do
        watchDir
            mgr
            (unDump path)
            (\e -> (takeExtension (eventPath e)) /= ".tmp" &&
                case e of
                Added _ _ _ -> True
                Removed _ _ _ -> True
                _ -> False
            )
            (\e -> do
                print e
                Chan.writeChan (chan env) ReadDumpDir
            )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


configDagsdato :: Env -> WatchManager -> MVar FilePath -> WatchMap -> Handler (Dagsdato.Model) -> Handler () -> IO StopListening
configDagsdato env mgr mDagsdatoFile watchMap handler handleDagsdatoDir = do
    filepath <- readMVar mDagsdatoFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            print e
            Chan.writeChan (chan env) ReadDagsdato
            TT.modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopDirDagsdato"
                stopDirDagsdato <- dirDagsdato env mgr mDagsdatoFile watchMap handleDagsdatoDir
                return $ HashMap.insert "stopDirDagsdato" stopDirDagsdato h
        )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks




dirDagsdato :: Env ->  WatchManager -> MVar FilePath -> WatchMap -> Handler () -> IO StopListening
dirDagsdato env mgr mDagsdatoFile _ handler = do
    dagsdatoFile <- readMVar mDagsdatoFile
    dagsdatoPath <- getDagsdato' dagsdatoFile
    case dagsdatoPath of
        Left _ -> return (return ())
        Right path -> 
            watchDir
                mgr
                (unDagsdato path)
                (const True)
                (\e -> do
                    print e
                    Chan.writeChan (chan env) SDirDagsdato
                )
                    `catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


configDagsdatoBackup :: Env -> WatchManager -> MVar FilePath -> WatchMap -> Handler DagsdatoBackup.Model -> Handler () -> IO StopListening
configDagsdatoBackup env mgr mDagsdatoBackupFile watchMap handler handleDagsdatoBackupDir = do
    filepath <- readMVar mDagsdatoBackupFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            print e

            Chan.writeChan (chan env) ReadDagsdatoBackup

            TT.modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopDirDagsdatoBackup"
                stopDirDagsdatoBackup <- dirDagsdatoBackup env mgr mDagsdatoBackupFile watchMap handleDagsdatoBackupDir
                return $ HashMap.insert "stopDirDagsdatoBackup" stopDirDagsdatoBackup h
        )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks



dirDagsdatoBackup :: Env -> WatchManager -> MVar FilePath -> WatchMap -> Handler () -> IO StopListening
dirDagsdatoBackup env mgr mDagsdatoBackupFile _ handler = do
    dagsdatoBackupFile <- readMVar mDagsdatoBackupFile
    dagsdatoBackupPath <- getDagsdatoBackup' dagsdatoBackupFile
    case dagsdatoBackupPath of
        Left _ -> return (return ()) -- TODO this sucks
        Right path -> watchDir
            mgr
            (unDagsdatoBackup path)
            (const True)
            (\e -> do
                print e
                Chan.writeChan (chan env) SDirDagsdatoBackup
            ) `catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


main :: Int -> FilePath -> IO ()
main port root = do
    --traceShowM "starting"
    config' <- loadConfig (root </> "config.json")
    --traceShowM "starting still"
    mkEnv root config' >>= runServer port
