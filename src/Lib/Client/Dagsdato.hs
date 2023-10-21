module Lib.Client.Dagsdato
    ( dagsdatoSection
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
import Lib.Dagsdato
import Lib.Client.Tab
import Lib.Client.Utils
import Lib.Client.Element


mkDagsdato :: Env -> Translation -> Model -> [UI Element]
mkDagsdato Env{..} translations model = do
    case unModel model of
        NotAsked -> [UI.p #+ [Lens.views starting string translations]]
        Loading -> [UI.p #+ [Lens.views loading string translations]]
        Failure e ->
            [ UI.div #. "section" #+ [Lens.views dagsdatoError string translations, UI.div #+ [string e]]
                   , UI.div #. "section" #+
                       [ mkFolderPicker "dagsdatoPicker" (Lens.view folderPicker translations) $ \folder ->
                            when (folder /= "") $
                                -- todo: handle bad input
                                void $ do
                                    Chan.writeChan chan (WriteDagsdato (Dagsdato folder))
                        ]
                    ]

        Data (Dagsdato dagsdato') ->
            [ UI.div #. "section" #+ [UI.h2 #+ [Lens.views dagsdatoTitle string translations], UI.string dagsdato']
                   , UI.div #. "section" #+
                        [mkFolderPicker "dagsdatoPicker" (Lens.view folderPicker translations) $ \folder ->
                            when (folder /= "") $
                                -- todo: handle bad input
                                void $ Chan.writeChan chan (WriteDagsdato (Dagsdato folder))
                        ]
                   ]


dagsdatoSection :: Env -> Window -> Translation -> Element -> Element -> Behavior Model -> UI Element
dagsdatoSection env@Env{..} win translation tabs' navigation bModel = do
    let bView = mkDagsdato env translation <$> bModel
    content <- UI.div # sink items bView


    view <- UI.div #+ fmap element [ content ]

    UI.div # set children [tabs', view, navigation]
