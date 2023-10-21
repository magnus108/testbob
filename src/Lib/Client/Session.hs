module Lib.Client.Session
    ( sessionsSection
    ) where

import Lib.App (Action(..))
import qualified Control.Concurrent.Chan.Strict as Chan

import           Reactive.Threepenny
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Utils.Comonad

import qualified Control.Lens as Lens

import Lib.App
import Lib.Translation
import Lib.Data
import Lib.Tab
import Lib.Client.Tab
import Lib.Session

import Lib.Client.Element

        {-

import           Reactive.Threepenny
import Data.Bitraversable

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import qualified Control.Lens as Lens

import Lib.App
import Lib.Translation
import Lib.Data
import Lib.Tab
import Lib.Client.Tab
import Lib.Session

import Lib.Client.Utils
import Lib.Client.Element


import qualified Utils.RoseTree as RT
import qualified Utils.TreeZipper as TZ


sessionsSection :: Env -> Window -> Translation -> Tabs -> Behavior Model -> UI ()
sessionsSection env@Env{..} win translations tabs bModel = do
    content <- UI.div #. "section" 

    liftIO $ do
        model <- currentValue bModel
        runUI win $ void $ do
            case unModel model of
                NotAsked -> do
                    msg <- Lens.views starting string translations
                    _ <- element content # set children [msg]
                    return ()
                Loading -> do
                    msg <- Lens.views loading string translations
                    _ <- element content # set children [msg]
                    return ()
                Failure e -> do
                    err <- UI.p #+ [Lens.views sessionsError string translations]
                    err' <- UI.div #+ [string e]
                    picker <- mkFilePicker "sessionPicker" (Lens.view filePicker translations) $ \file ->
                        when (file /= "") $ do
                            --TODO er det engentligt det her man vil?
                            parseSessions <- liftIO $ getSessions' file
                            forM_ parseSessions $ writeSessions mSessionsFile

                    child <- UI.div # set children [err, err', picker]
                    _ <- element content # set children [child]
                    return ()
                Data (Sessions sessions) -> do
                    let children' = case sessions of
                            (TZ.TreeZipper (RT.Leaf x) []) -> do
                                let this = mkSelected env translations x
                                [this]

                            (TZ.TreeZipper (RT.Branch _ xs) []) -> do
                                let children'' = mkChildren env translations (Sessions sessions) xs
                                [children'']

                            (TZ.TreeZipper (RT.Leaf x) (TZ.Context _ _ _:_)) -> do
                                let parent = mkParent env translations (Sessions sessions) --kan give mening senere.
                                let this = mkSelected env translations x
                                [parent, this]

                            (TZ.TreeZipper (RT.Branch _ xs) (TZ.Context _ _ _:_)) -> do
                                let parent = mkParent env translations (Sessions sessions)
                                let children'' = mkChildren env translations (Sessions sessions) xs
                                [parent, children'']

                    child <- UI.div #+ children'
                    _ <- element content # set children [child]
                    return ()


    liftIO $ onChange bModel $ \model -> runUI win $ do
        case unModel model of
            NotAsked -> do
                msg <- Lens.views starting string translations
                _ <- element content # set children [msg]
                return ()
            Loading -> do
                msg <- Lens.views loading string translations
                _ <- element content # set children [msg]
                return ()
            Failure e -> do
                err <- UI.p #+ [Lens.views sessionsError string translations]
                err' <- UI.div #+ [string e]
                picker <- mkFilePicker "sessionPicker" (Lens.view filePicker translations) $ \file ->
                    when (file /= "") $ do
                        --TODO er det engentligt det her man vil?
                        parseSessions <- liftIO $ getSessions' file
                        forM_ parseSessions $ 
                            Chan.writeChan chan . WriteSessions

                child <- UI.div # set children [err, err', picker]
                _ <- element content # set children [child]
                return ()
            Data (Sessions sessions) -> do
                let children' = case sessions of
                        (TZ.TreeZipper (RT.Leaf x) []) -> do
                            let this = mkSelected env translations x
                            [this]

                        (TZ.TreeZipper (RT.Branch _ xs) []) -> do
                            let children'' = mkChildren env translations (Sessions sessions) xs
                            [children'']

                        (TZ.TreeZipper (RT.Leaf x) (TZ.Context _ _ _:_)) -> do
                            let parent = mkParent env translations (Sessions sessions) --kan give mening senere.
                            let this = mkSelected env translations x
                            [parent, this]

                        (TZ.TreeZipper (RT.Branch _ xs) (TZ.Context _ _ _:_)) -> do
                            let parent = mkParent env translations (Sessions sessions)
                            let children'' = mkChildren env translations (Sessions sessions) xs
                            [parent, children'']

                child <- UI.div #+ children'
                _ <- element content # set children [child]
                return ()


    tabs' <- mkElement "nav" #. "section" #+ [mkTabs env translations tabs]
    navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translations tabs]

    view <- UI.div #+ fmap element [ content ]

    void $ UI.getBody win # set children [tabs', view, navigation]




mkChildren :: Env -> Translation -> Sessions -> [RT.RoseTree Decisions Session] -> UI Element
mkChildren env translations (Sessions sessions) elems = do
        sessions' <- mapM (bimapM (mkDecision env translations (Sessions sessions)) (mkSession env translations (Sessions sessions))) elems
        UI.div #. "buttons has-addons" #+ fmap (element . fromEither . RT.datum ) sessions'


mkSelected :: Env -> Translation -> Session -> UI Element
mkSelected Env{..} translations session = do
    let name = translationSession session translations
    mkButton "idd" name #. "button is-selected" # set (attr "disabled") "true"


mkParent :: Env -> Translation -> Sessions -> UI Element
mkParent Env{..} translations (Sessions sessions) = do
    let name = Lens.view up translations
    chooseButton <- mkButton "idd" name
    UI.on UI.click chooseButton $ \_ ->
            --TODO get rid of either by using extend
            forM_ (fmap Sessions (TZ.up sessions)) $ 
                Chan.writeChan chan . WriteSessions
    UI.div #. "buttons addons" #+ [element chooseButton]


mkDecision :: Env -> Translation -> Sessions -> Decisions -> UI Element
mkDecision Env{..} translations (Sessions sessions) decision = do
    let name = translationDecision decision translations
    chooseButton <- mkButton "idd" name
    UI.on UI.click chooseButton $ \_ ->
            forM_ (fmap Sessions (TZ.down (Left decision) sessions)) $ 
                Chan.writeChan chan . WriteSessions
    return chooseButton



mkSession :: Env -> Translation -> Sessions -> Session -> UI Element
mkSession Env{..} translations (Sessions sessions) session = do
    let name = translationSession session translations
    chooseButton <- mkButton "idd" name
    UI.on UI.click chooseButton $ \_ ->
            --TODO get rid of either by using extend
            forM_ (fmap Sessions (TZ.down (Right session) sessions)) $ do
                Chan.writeChan chan . WriteSessions
    return chooseButton
    -}


sessionsSection :: Env -> Window -> Translation -> Element -> Element -> Behavior Model -> UI Element
sessionsSection env@Env{..} win translations tabs' navigation bModel = do

    content <- UI.div #. "section"

    liftIO $ do
        model <- currentValue bModel
        runUI win $ void $ do
            case unModel model of
                NotAsked -> do
                    msg <- Lens.views starting string translations
                    _ <- element content # set children [msg]
                    return ()
                Loading -> do
                    msg <- Lens.views loading string translations
                    _ <- element content # set children [msg]
                    return ()
                Failure e -> do
                    err <- UI.p #+ [Lens.views sessionsError string translations]
                    errMsg <- UI.p #+ [string e]
                    picker <- mkFilePicker "sessionPicker" (Lens.view filePicker translations) $ \file ->
                        when (file /= "") $ do
                            --TODO er det engentligt det her man vil?
                            parseSessions <- liftIO $ getSessions' file
                            forM_ parseSessions $ 
                                Chan.writeChan chan . WriteSessions

                    child <- UI.div # set children [err, errMsg, picker]
                    _ <- element content # set children [child]
                    return ()

                Data (Sessions sessions) -> do
                        let currentSession = extract sessions
                        let elems = sessions =>> \sessions'' -> let
                                        thisSession = extract sessions''
                                    in
                                        ( thisSession
                                        , thisSession == currentSession
                                        , Sessions sessions''
                                        )
                        elems' <- forM elems $ mkSession env translations
                        
                        section <- UI.div #. "buttons has-addons" # set children (toList elems')
                        _ <- element content # set children [section]
                        return ()

    liftIO $ onChange bModel $ \model -> runUI win $ do
        case unModel model of
            NotAsked -> do
                msg <- Lens.views starting string translations
                _ <- element content # set children [msg]
                return ()
            Loading -> do
                msg <- Lens.views loading string translations
                _ <- element content # set children [msg]
                return ()
            Failure e -> do
                err <- UI.p #+ [Lens.views sessionsError string translations]
                errMsg <- UI.p #+ [string e]
                picker <- mkFilePicker "sessionPicker" (Lens.view filePicker translations) $ \file ->
                    when (file /= "") $ do
                        --TODO er det engentligt det her man vil?
                        parseSessions <- liftIO $ getSessions' file
                        forM_ parseSessions $ 
                            Chan.writeChan chan . WriteSessions

                child <- UI.div # set children [err, errMsg, picker]
                _ <- element content # set children [child]
                return ()

            Data (Sessions sessions) -> do
                        let currentSession = extract sessions
                        let elems = sessions =>> \sessions'' -> let
                                        thisSession = extract sessions''
                                    in
                                        ( thisSession
                                        , thisSession == currentSession
                                        , Sessions sessions''
                                        )
                        elems' <- forM elems $ mkSession env translations
                        
                        section <- UI.div #. "buttons has-addons" # set children (toList elems')
                        _ <- element content # set children [section]
                        return ()


    view <- UI.div #+ fmap element [ content ]

    UI.div # set children [tabs', view, navigation]


mkSession :: Env -> Translation -> (Session, Bool, Sessions) -> UI Element
mkSession Env{..} translations (session, isCenter, sessions)
    | isCenter = do
        let name = translationSession session translations
        mkButton "idd" name #. "button is-selected is-success" # set (attr "disabled") "true"
    | otherwise = do
        let name = translationSession session translations
        button <- mkButton "idd" name #. "button"
        UI.on UI.click button $ \_ ->
                liftIO $ Chan.writeChan chan (WriteSessions sessions)
        return button
