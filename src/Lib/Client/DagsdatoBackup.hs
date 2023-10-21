module Lib.Client.DagsdatoBackup
    ( dagsdatoBackupSection
    ) where

import Lib.App (Action(..))
import qualified Control.Concurrent.Chan.Strict as Chan

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI


import qualified Control.Lens as Lens

import Lib.App
import Lib.Translation
import Lib.Data
import Lib.Tab
import Lib.DagsdatoBackup
import Lib.Client.Tab
import Lib.Client.Utils
import Lib.Client.Element


mkDagsdatoBackup :: Env -> Translation -> Model -> [UI Element]
mkDagsdatoBackup Env{..} translations model = do
    case unModel model of
        NotAsked -> [UI.p #+ [Lens.views starting string translations]]
        Loading -> [UI.p #+ [Lens.views loading string translations]]
        Failure e ->
            [ UI.div #. "section" #+ [Lens.views dagsdatoBackupError string translations, UI.div #+ [string e]]
                   , UI.div #. "section" #+
                       [ mkFolderPicker "dagsdatoBackupPicker" (Lens.view folderPicker translations) $ \folder ->
                            when (folder /= "") $
                                -- todo: handle bad input
                                void $ Chan.writeChan chan (WriteDagsdatoBackup (DagsdatoBackup folder))
                        ]
                    ]

        Data (DagsdatoBackup dagsdatoBackup') ->
            [ UI.div #. "section" #+ [UI.h2 #+ [Lens.views dagsdatoTitle string translations], UI.string dagsdatoBackup']
                   , UI.div #. "section" #+
                        [mkFolderPicker "dagsdatoPicker" (Lens.view folderPicker translations) $ \folder ->
                            when (folder /= "") $
                                -- todo: handle bad input
                                void $ Chan.writeChan chan (WriteDagsdatoBackup (DagsdatoBackup folder))
                        ]
                   ]


dagsdatoBackupSection :: Env -> Window -> Translation -> Element -> Element -> Behavior Model -> UI Element
dagsdatoBackupSection env@Env{..} win translation tabs' navigation bModel = do
    let bView = mkDagsdatoBackup env translation <$> bModel
    content <- UI.div # sink items bView


    view <- UI.div #+ fmap element [ content ]

    UI.div # set children [tabs', view, navigation]
