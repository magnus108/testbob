module Lib.Client.Camera
    ( camerasSection
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
import Lib.Camera

import Lib.Client.Element


camerasSection :: Env -> Window -> Translation -> Element -> Element  -> Behavior Model -> UI Element
camerasSection env@Env{..} win translations tabs' navigation bModel = do

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
                    err <- UI.p #+ [Lens.views camerasError string translations]
                    errMsg <- UI.p #+ [string e]
                    picker <- mkFilePicker "cameraPicker" (Lens.view filePicker translations) $ \file ->
                        when (file /= "") $ do
                            --TODO er det engentligt det her man vil?
                            parseCameras <- liftIO $ getCameras' file
                            forM_ parseCameras $ 
                                Chan.writeChan chan . WriteCamera

                    section <- UI.div # set children [err, errMsg, picker]

                    _ <- element content # set children [section]
                    return ()

                Data (Cameras cameras) -> do
                        let currentCamera = extract cameras
                        let elems = cameras =>> \cameras'' -> let
                                        thisCamera = extract cameras''
                                    in
                                        ( thisCamera
                                        , thisCamera == currentCamera
                                        , Cameras cameras''
                                        )
                        elems' <- forM elems $ mkCamera env translations
                        
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
                err <- UI.p #+ [Lens.views camerasError string translations]
                errMsg <- UI.p #+ [string e]
                picker <- mkFilePicker "cameraPicker" (Lens.view filePicker translations) $ \file ->
                    when (file /= "") $ do
                        --TODO er det engentligt det her man vil?
                        parseCameras <- liftIO $ getCameras' file
                        forM_ parseCameras $ do
                                Chan.writeChan chan . WriteCamera

                section <- UI.div # set children [err, errMsg, picker]
                _ <- element content # set children [section]
                return ()

            Data (Cameras cameras) -> do
                    let currentCamera = extract cameras
                    let elems = cameras =>> \cameras'' -> let
                                    thisCamera = extract cameras''
                                in
                                    ( thisCamera
                                    , thisCamera == currentCamera
                                    , Cameras cameras''
                                    )
                    elems' <- forM elems $ mkCamera env translations
                    
                    section <- UI.div #. "buttons has-addons" # set children (toList elems')
                    _ <- element content # set children [section]
                    return ()



    view <- UI.div #+ fmap element [ content ]

    UI.div # set children [tabs', view, navigation]
    --void $ UI.getBody win # set children [tabs', view, navigation]


mkCamera :: Env -> Translation -> (Camera, Bool, Cameras) -> UI Element
mkCamera Env{..} translations (camera, isCenter, cameras)
    | isCenter = do
        mkButton "idd" name #. "button is-selected is-success is-large" # set (attr "disabled") "true"
    | otherwise = do
        button <- mkButton "idd" name #. "button is-large"
        UI.on UI.click button $ \_ ->
                liftIO $ Chan.writeChan chan (WriteCamera cameras)
        return button
    where
        translator = case camera of
                CR2 -> cr2
                CR3 -> cr3
        name = Lens.view translator translations
