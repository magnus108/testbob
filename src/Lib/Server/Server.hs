module Lib.Server.Server
    ( run
    ) where

import Lib.Data
import Lib.Client.Element
import qualified Control.Lens as Lens
import Lib.Client.Utils
import qualified Lib.Main as Main
import qualified Control.Concurrent.Chan.Strict as Chan

import Control.Concurrent (ThreadId, killThread)
import           Reactive.Threepenny

import Lib.Client.Tab
import System.FilePath
import Graphics.UI.Threepenny.Core 
import qualified Graphics.UI.Threepenny as UI

import Control.Concurrent.MVar.Strict (withMVar)
import Lib.App (Env(..))

import Lib.Translation
import Lib.Tab
import qualified Lib.Build as Build
import qualified Lib.Photographer as Photographer
import qualified Lib.Photographee as Photographee
import qualified Lib.Dump as Dump
import qualified Lib.Session as Session
import qualified Lib.Shooting as Shooting
import qualified Lib.Dagsdato as Dagsdato
import qualified Lib.DagsdatoBackup as DagsdatoBackup
import qualified Lib.Doneshooting as Doneshooting
import qualified Lib.Camera as Camera
import qualified Lib.Location as Location
import qualified Lib.Grade as Grade

import qualified Lib.ControlModel as ControlModel
import qualified Lib.Client.Location as CLocation
import qualified Lib.Client.InsertPhotographee as InsertPhotographee
import qualified Lib.Client.Main as CMain
import Lib.Client.Session
import Lib.Client.Shooting
import Lib.Client.Camera
import Lib.Client.Dump
import Lib.Client.Dagsdato
import Lib.Client.DagsdatoBackup
import Lib.Client.Doneshooting
import Lib.Client.Photographer
import Lib.Client.Control



import Utils.ListZipper (focus)
import Utils.Comonad


import Lib.App (Action(..))

--view :: Env -> Window -> Translation -> Behavior Doneshooting.DoneshootingDirModel -> Behavior Build.Model -> Behavior Grade.Model -> Behavior Location.Model -> UI.Behavior Session.Model -> UI.Behavior Shooting.Model -> UI.Behavior Camera.Model -> UI.Behavior Dump.DumpModel -> UI.Behavior Dump.DumpDirModel -> UI.Behavior Doneshooting.Model -> UI.Behavior Dagsdato.Model -> UI.Behavior DagsdatoBackup.Model -> UI.Behavior Photographer.Model -> UI.Behavior Photographee.Model -> Handler (Grade.Model) -> Handler (Location.Model) -> Handler Dump.DumpModel -> Handler Dump.DumpDirModel -> Handler Photographee.Model -> Tabs -> UI ()
--view env@Env{..} win translation bDoneshootingDir bBuild bGrades bLocationConfigFile bSessions bShootings bCameras bDump bDumpDir bDoneshooting bDagsdato bDagsdatoBackup bPhotographers bPhotographees tabs  bModelLocation1 bModelInserter1 bModel1 bModel2= do





run :: Int -> Env -> Translation -> UI.Behavior Doneshooting.DoneshootingDirModel -> UI.Behavior Build.Model -> UI.Behavior Grade.Model ->  UI.Behavior Location.Model -> UI.Behavior Session.Model -> UI.Behavior Shooting.Model -> UI.Behavior Camera.Model -> UI.Behavior Dump.DumpModel -> UI.Behavior Dump.DumpDirModel -> UI.Behavior Doneshooting.Model -> UI.Behavior Dagsdato.Model -> UI.Behavior DagsdatoBackup.Model -> UI.Event Tabs -> UI.Behavior (Photographer.Model) -> UI.Behavior (Photographee.Model) -> ThreadId -> IO ()
run port env@Env{..} translations bDoneshootingDir bBuild eGrades bLocationConfigFile eSessions eShootings eCameras eDump bDumpDir eDoneshooting eDagsdato eDagsdatoBackup eTabs bPhotographers bPhotographees messageReceiver= do
    tabs <- withMVar mTabsFile $ \tabsFile -> getTabs tabsFile

    startGUI defaultConfig
        { jsWindowReloadOnDisconnect = False
        , jsPort = Just port
        , jsStatic = Just (serverRoot </> "static")
        , jsCustomHTML = Just "index.html"
        } $ \win -> do

        -- behaviors
        bTabs <- stepper tabs eTabs

        let bModelLocation1 = liftA2 CLocation.mkModel bLocationConfigFile eGrades
        let bModelInserter1 = InsertPhotographee.mkModel <$> bLocationConfigFile <*> eGrades <*> bPhotographees <*> bDumpDir

        let bSession1 =
                fmap (\x -> (\(Session.Sessions sessions) -> extract sessions
                            ) <$> (Session.unModel x) ) eSessions

        let bCamera1 = (\camerasData -> fmap (\(Camera.Cameras x) -> extract x) (Camera.unModel camerasData)) <$> eCameras
        let bDagsdato1' = Dagsdato.unModel <$> eDagsdato
        let bShooting1 = fmap (\(Shooting.Shootings x) -> extract x) <$> Shooting.unModel <$> eShootings
        let bPhotographer1= fmap (\(Photographer.Photographers x) -> extract x) <$> Photographer.unModel <$> bPhotographers
        let bDoneshooting1' = Doneshooting.unModel <$> eDoneshooting
        let bDagsdatoBackup1' = DagsdatoBackup.unModel <$> eDagsdatoBackup
        let bBuild1' = Build.unModel <$> bBuild
        let bModel1 = CMain.mkModel <$> bLocationConfigFile <*> eGrades <*> eDump <*> bDumpDir <*> bPhotographees <*> bSession1 <*> bCamera1 <*> bDagsdato1' <*> bShooting1 <*> bDoneshooting1' <*> bPhotographer1 <*> bDagsdatoBackup1' <*> bBuild1'


        let bModel2 = ControlModel.mkModel <$> eGrades <*> bDoneshootingDir <*> bPhotographees


        tabs' <- mkElement "nav" #. "section" #+ [mkTabs env translations tabs]
        navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translations tabs]
        inputteren <- UI.input #. "input"


        contentInner <- UI.div

        --chaos
        let isOk = Lens.view isChanged translations
        changedButton <- mkButton "mkChange" isOk
        changed' <- UI.p
        changed <- UI.div #. "section"


        mkBuild' <- mkButton "mkBuild" ""
        mkBuild <- UI.div #. "section" # set children [mkBuild']

        build' <- UI.div #. "section"

        photographerName <- UI.p #. "section has-text-info"

        contentTT <- UI.div
        select <- UI.select
        currentPhotographee <- UI.h1 #. "is-size-4"

        help <- string "Valgt: "
        findHelp <- string "Søg: "
        inputSection <- UI.div #. "section" # set children [help, currentPhotographee, findHelp, inputteren]

        selectSection <-
            UI.div
            #. "section"
            #+ [ UI.div
                    #. "field is-horizontal"
                    #+ [ UI.div
                        #. "field-body"
                        #+ [ UI.div
                            #. "field"
                            #+ [UI.p #. "control" #+ [UI.div #. "select" #+ [element select]]]
                            ]
                    ]
                ]

        count <- UI.div

        bEditingInput <- bEditing inputteren
        bEditingSelect  <- bEditing select

        selectPhotographee <- UI.select
        bEditingSelectPhotographee <- bEditing selectPhotographee

        ok <- UI.div

        countPhotographees' <- UI.span
        --chaos

        mainSection' <- 
            CMain.mainSection env win translations contentInner tabs' navigation bModel1 inputteren isOk
                changedButton
                changed'
                changed
                mkBuild' 
                mkBuild
                build'
                photographerName
                contentTT
                select
                currentPhotographee
                help
                findHelp
                inputSection
                selectSection
                count
                bEditingInput
                bEditingSelect
                selectPhotographee
                bEditingSelectPhotographee
                ok
                countPhotographees'

        dumpSection' <- dumpSection env win translations tabs' navigation eDump
        doneshootingSection' <- doneshootingSection env win translations tabs' navigation eDoneshooting
        photographersSection' <- photographersSection env win translations tabs' navigation bPhotographers
        shootingsSection' <- shootingsSection env win translations tabs' navigation eShootings
        sessionsSection' <- sessionsSection env win translations tabs' navigation eSessions
        camerasSection' <- camerasSection env win translations tabs' navigation eCameras
        dagsdatoSection' <- dagsdatoSection env win translations tabs' navigation eDagsdato
        dagsdatoBackupSection' <- dagsdatoBackupSection env win translations tabs' navigation  eDagsdatoBackup
        locationSection' <- CLocation.locationSection env win translations tabs' navigation bModelLocation1
        controlSection' <- controlSection env win translations tabs' navigation bModel2
        insertPhotographeeSection' <- InsertPhotographee.insertPhotographeeSection env win translations tabs' navigation bModelInserter1

        let eSelect   = CMain.selectPhotographeeF <$> filterJust (selectionChange' selectPhotographee)

        let eSelectGrade = CLocation.selectGrade <$> filterJust (selectionChange' select)
        let eFind = Photographee.tryFindById <$> UI.valueChange inputteren

        let gradeEvent = concatenate' <$> unions' (eSelectGrade :| [])
        let findEvent = concatenate' <$> unions' (eFind :| [])
        let ee  = filterJust
                    $   fmap
                            (\m f -> case toJust (Main._unModel m) of
                                Nothing -> Nothing
                                Just x -> Just $ Main.Model $ Data $ Lens.over Main.grades f x
                            )
                            bModel1
                    <@> gradeEvent


        let ee2 = filterJust
                    $   fmap
                            (\m f -> case toJust (Main._unModel m) of
                                Nothing -> Nothing
                                Just x -> Just $ Main.Model $ Data $ Lens.over Main.photographees f x
                            )
                            bModel1
                    <@> findEvent

        let ee3 = filterJust
                    $   fmap
                            (\m f -> case toJust (Main._unModel m) of
                                Nothing -> Nothing
                                Just x -> Just $ Main.Model $ Data $ Lens.over Main.photographees f x
                            )
                            bModel1
                    <@> eSelect



        _ <- onEvent ee3 $ \model -> do
            void $ liftIO $ do
                case toJust (Main._unModel model) of
                    Nothing -> return ()
                    Just item'  -> do
                        --Location.writeLocationFile mLocationConfigFile (location i)
                        _ <- Chan.writeChan chan (WritePhotographees (Main._photographees item') (Main._dumpDir item'))
                        return ()


        let enterKeydown = filterJust $ (\keycode -> if (keycode == 13) then Just () else Nothing) <$> (UI.keydown inputteren)

        let buildClick = seq <$> UI.click mkBuild'

        let changeOk = UI.click changedButton

        let ee5 = filterJust
                    $   fmap
                            (\m -> case toJust (Main._unModel m) of
                                Nothing -> Nothing
                                Just x -> Just (Main.Model (Data x))
                            )
                            bModel1
                    <@ changeOk

        _ <- onEvent ee5 $ \model -> do
            void $ liftIO $ do
                case toJust (Main._unModel model) of
                    Nothing -> return ()
                    Just item'  -> do
                        _ <- Chan.writeChan chan $ ( WritePhotographeesOK (Main._photographees item') )
                        return ()

        let buildEvent = concatenate' <$> unions' (buildClick :| [fmap const enterKeydown])

        let ee4 = filterJust
                    $   fmap
                            (\m -> case toJust (Main._unModel m) of
                                Nothing -> Nothing
                                Just x -> Just (Main.Model (Data x))
                            )
                            bModel1
                    <@ buildEvent



        _ <- onEvent ee4 $ \model -> do
            UI.setFocus (help) -- hack
            _ <- element inputteren # set value ""
            void $ liftIO $ do
                case toJust (Main._unModel model) of
                    Nothing -> return ()
                    Just item'  -> do
                        case (Main._photographees item') of
                            (Photographee.CorrectPhotographees ys) -> do
                                _ <- Chan.writeChan chan $ ( MFcker (item'))
                                return ()
                            (Photographee.ChangedPhotographees ys) -> do
                                return ()
                            (Photographee.NotFoundPhotographees ys) -> do
                                return ()


        _ <- onEvent ee2 $ \model -> do
            void $ liftIO $ do
                case toJust (Main._unModel model) of
                    Nothing -> return ()
                    Just item'  -> do
                        --Location.writeLocationFile mLocationConfigFile (location i)
                        model2 <- currentValue bModel1
                        case toJust (Main._unModel model2) of
                            Nothing -> do
                                    _ <- Chan.writeChan chan $ ( WritePhotographees (Main._photographees item') (Main._dumpDir item'))
                                    return $ ()
                            Just item'' -> do
                                if ((Main._photographees item') /= (Main._photographees item'')) then
                                    void $ Chan.writeChan chan $ ( WritePhotographees (Main._photographees item') (Main._dumpDir item'))
                                else
                                    return $ ()

        _ <- onEvent ee $ \model -> do
            void $ liftIO $ do
                case toJust (Main._unModel model) of
                    Nothing -> return ()
                    Just item'  -> do
                        --Location.writeLocationFile mLocationConfigFile (location i)
                        _ <- Chan.writeChan chan $ (WriteGrades ( Main._grades item'))
                        return ()


        content <- UI.div
        liftIOLater $ do
            model <- currentValue bTabs
            runUI win $ void $ do
                let currentTab = focus (unTabs model)
                let childe = case currentTab of
                        DumpTab -> dumpSection'
                        DoneshootingTab -> doneshootingSection'
                        PhotographersTab -> photographersSection'
                        ShootingsTab -> shootingsSection'
                        SessionsTab -> sessionsSection'
                        CamerasTab -> camerasSection'
                        DagsdatoTab -> dagsdatoSection'
                        DagsdatoBackupTab -> dagsdatoBackupSection'
                        LocationTab -> locationSection'
                        MainTab -> mainSection'
                        ControlTab -> controlSection'
                        InsertPhotographeeTab -> insertPhotographeeSection'

                tt <- mkTabs env translations (model)
                ttt <- mkNavigation env translations (model)
                element tabs' # set children [tt]
                element navigation # set children [ttt]

                void $ element content # set children [tabs', childe, navigation]

                case currentTab of
                    MainTab -> UI.setFocus (getElement inputteren)
                    _ -> return ()


        liftIOLater $ onChange bTabs $ \tabs'' -> runUI win $ do
            let currentTab = focus (unTabs tabs'')
            let childe = case currentTab of
                    DumpTab -> dumpSection'
                    DoneshootingTab -> doneshootingSection'
                    PhotographersTab -> photographersSection'
                    ShootingsTab -> shootingsSection'
                    SessionsTab -> sessionsSection'
                    CamerasTab -> camerasSection'
                    DagsdatoTab -> dagsdatoSection'
                    DagsdatoBackupTab -> dagsdatoBackupSection'
                    LocationTab -> locationSection'
                    MainTab -> mainSection'
                    ControlTab -> controlSection'
                    InsertPhotographeeTab -> insertPhotographeeSection'


            tt <- mkTabs env translations (tabs'')
            ttt <- mkNavigation env translations (tabs'')
            element tabs' # set children [tt]
            element navigation # set children [ttt]

            
            void $ element content # set children [tabs',childe, navigation]

            case currentTab of
                    MainTab -> UI.setFocus (getElement inputteren)
                    _ -> return ()

        void $ UI.getBody win # set children [content]

        UI.on UI.disconnect win $ const $ liftIO $ do
            killThread messageReceiver

