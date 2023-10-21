module Lib.Client.Shooting
    ( shootingsSection
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
import Lib.Shooting

import Lib.Client.Element


shootingsSection :: Env -> Window -> Translation -> Element -> Element -> Behavior Model -> UI Element
shootingsSection env@Env{..} win translations tabs' navigation bModel = do
    content <- UI.div #. "section" 


    liftIOLater $ do
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
                    err <- UI.p #+ [Lens.views shootingsError string translations]
                    err' <- UI.div #+ [string e]
                    picker <- mkFilePicker "shootingPicker" (Lens.view filePicker translations) $ \file ->
                        when (file /= "") $ do
                            --TODO er det engentligt det her man vil?
                            parseShootings <- liftIO $ getShootings' file
                            forM_ parseShootings $ 
                                Chan.writeChan chan . WriteShooting

                    section <- UI.div #. "section" # set children [err, err', picker]
                    _ <- element content # set children [section]
                    return ()

                Data (Shootings shootings) -> do
                        let currentShooting = extract shootings
                        let elems = shootings =>> \shootings'' -> let
                                        thisShooting = extract shootings''
                                    in
                                        ( thisShooting
                                        , thisShooting == currentShooting
                                        , Shootings shootings''
                                        )
                        elems' <- forM elems $ mkShooting env translations
                        section <- UI.div #. "buttons has-addons" # set children (toList elems')
                        _ <- element content # set children [section]
                        return ()

    liftIOLater $ onChange bModel $ \model -> runUI win $ do
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
                    err <- UI.p #+ [Lens.views shootingsError string translations]
                    err' <- UI.div #+ [string e]
                    picker <- mkFilePicker "shootingPicker" (Lens.view filePicker translations) $ \file ->
                        when (file /= "") $ do
                            --TODO er det engentligt det her man vil?
                            parseShootings <- liftIO $ getShootings' file
                            forM_ parseShootings $ 
                                Chan.writeChan chan . WriteShooting

                    section <- UI.div #. "section" # set children [err, err', picker]
                    _ <- element content # set children [section]
                    return ()

                Data (Shootings shootings) -> do
                        let currentShooting = extract shootings
                        let elems = shootings =>> \shootings'' -> let
                                        thisShooting = extract shootings''
                                    in
                                        ( thisShooting
                                        , thisShooting == currentShooting
                                        , Shootings shootings''
                                        )
                        elems' <- forM elems $ mkShooting env translations
                        section <- UI.div #. "buttons has-addons" # set children (toList elems')
                        _ <- element content # set children [section]
                        return ()


    view <- UI.div #+ fmap element [ content ]

    UI.div # set children [tabs', view, navigation]


mkShooting :: Env -> Translation -> (Shooting, Bool, Shootings) -> UI Element
mkShooting Env{..} translations (shooting, isCenter, shootings)
    | isCenter = do
        mkButton "idd" name #. "button is-selected is-success is-large" # set (attr "disabled") "true"
    | otherwise = do
        button <- mkButton "idd" name #. "button is-large"
        UI.on UI.click button $ \_ ->
                liftIO $ Chan.writeChan chan (WriteShooting shootings)
        return button
    where
        translator = case shooting of
                ReShoot -> reShoot
                Normal -> normal
        name = Lens.view translator translations
