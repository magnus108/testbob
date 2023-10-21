module Lib.Client.Dump
    ( dumpSection
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
import Lib.Dump
import Lib.Client.Tab
import Lib.Client.Utils
import Lib.Client.Element



mkDump :: Env -> Translation -> DumpModel -> [UI Element]
mkDump Env{..} translations model = do
    case unModel model of
        NotAsked -> [UI.p #+ [Lens.views starting string translations]]
        Loading -> [UI.p #+ [Lens.views loading string translations]]
        Failure e ->
            [ UI.div #. "section" #+ [Lens.views dumpError string translations, UI.div #+ [string e]]
                   , UI.div #. "section" #+
                       [ mkFolderPicker "dumpPicker" (Lens.view folderPicker translations) $ \folder ->
                            when (folder /= "") $
                                -- todo: handle bad input
                                void $ Chan.writeChan chan ( WriteDump (Dump folder))
                        ]
                    ]

        Data (Dump dump') ->
            [ UI.div #. "section" #+ [UI.h2 #+ [Lens.views dumpTitle string translations], UI.string dump']
                   , UI.div #. "section" #+
                        [mkFolderPicker "dumpPicker" (Lens.view folderPicker translations) $ \folder ->
                            when (folder /= "") $
                                -- todo: handle bad input
                                void $ Chan.writeChan chan ( WriteDump (Dump folder))
                        ]
                   ]


dumpSection :: Env -> Window -> Translation -> Element -> Element -> Behavior DumpModel -> UI Element
dumpSection env@Env{..} win translation tabs' navigation bModel = do
    let bView = mkDump env translation <$> bModel
    content <- UI.div # sink items bView

    view <- UI.div #+ fmap element [ content ]
    
    UI.div # set children [tabs', view, navigation]
    --void $ UI.getBody win # set children [tabs', view, navigation]
