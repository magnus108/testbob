module Lib.Client.Photographer
    ( photographersSection
    , Data(..)
    ) where

import Lib.App (Action(..))
import qualified Control.Concurrent.Chan.Strict as Chan

import Lib.Client.Utils

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Utils.Comonad
import qualified Utils.ListZipper as ListZipper

import qualified Control.Lens as Lens
import           Reactive.Threepenny

import Lib.App
import Lib.Translation
import Lib.Data
import Lib.Tab
import Lib.Client.Tab
import Lib.Photographer
import qualified Lib.Photographer as Photographer

import Lib.Client.Element


mkPhotographers :: Env -> Photographer.Photographers -> [UI Element]
mkPhotographers env (Photographer.Photographers photographers') = do
    let elems = ListZipper.iextend (\i photographers'' -> (i, photographers' == photographers'', extract photographers'')) photographers'
    map (mkPhotographerListItem env) (ListZipper.toList elems)


mkPhotographerListItem :: Env -> (Int, Bool, Photographer.Photographer) -> UI Element
mkPhotographerListItem Env {..} (thisIndex, isCenter, photographer) = do
    let name' = Photographer._name photographer
    let option = UI.option # set value (show thisIndex) # set text name'
    if isCenter then option # set UI.selected True else option


photographersSection :: Env -> Window -> Translation -> Element -> Element -> Behavior Model -> UI Element
photographersSection env@Env{..} win translations tabs' navigation bModel = do

    content <- UI.div #. "section"
    select <- UI.select
    selectContent <- UI.div #. "select" #+ [element select]

    bEditingSelect <- bEditing select

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
                    msg <- UI.p #+ [Lens.views photographersError string translations]
                    picker <- mkFilePicker "photographerPicker" (Lens.view filePicker translations) $ \file ->
                        when (file /= "") $ do
                            --TODO er det engentligt det her man vil?
                            parsePhotographers <- liftIO $ getPhotographers' file

                            forM_ parsePhotographers $ do
                                Chan.writeChan chan . WritePhographers

                    err <- UI.p #+ [string e]
                    section <- UI.div #. "section" # set children [msg,err, picker]
                    _ <- element content # set children [section]
                    return ()

                Data (Photographers photographers) -> do
                    _ <- element select # set children [] #+ (mkPhotographers env (Photographers photographers))
                    _ <- element content # set children [selectContent]

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
                msg <- UI.p #+ [Lens.views photographersError string translations]
                err <- UI.p #+ [string e]
                picker <- mkFilePicker "photographerPicker" (Lens.view filePicker translations) $ \file ->
                    when (file /= "") $ do
                        --TODO er det engentligt det her man vil?
                        parsePhotographers <- liftIO $ getPhotographers' file
                        forM_ parsePhotographers $ do
                            Chan.writeChan chan . WritePhographers

                section <- UI.div #. "section" # set children [msg, err, picker]
                _ <- element content # set children [section]
                return ()
            Data (Photographers photographers) -> do
                editingSelect <- liftIO $ currentValue bEditingSelect

                when (not editingSelect) $ void $
                    element select # set children [] #+ (mkPhotographers env (Photographers photographers))

                when (not (editingSelect)) $ void $ do
                    element content # set children [selectContent]
                return ()


    let eSelect   = selectPhotographeeF <$> filterJust (selectionChange' select)
    let allEventsPhotographee = concatenate' <$> unions' (eSelect :| [])

    let ee3 = filterJust
                $   fmap
                        (\m f -> case toJust (unModel m) of
                            Nothing -> Nothing
                            Just x -> Just $ Model $ Data $ f x
                        )
                        bModel
                <@> allEventsPhotographee

    _ <- onEvent ee3 $ \model -> do
        void $ liftIO $ do
            case toJust (unModel model) of
                Nothing -> return ()
                Just item'  -> do
                    --Location.writeLocationFile mLocationConfigFile (location i)
                    _ <- Chan.writeChan chan (WritePhographers item')
                    return ()


    view <- UI.div #+ fmap element [ content ]

    UI.div # set children [tabs', view, navigation]


selectPhotographeeF :: Int -> Photographer.Photographers -> Photographer.Photographers
selectPhotographeeF selected (Photographer.Photographers photographers') =
        -- TODO this just wierd
    fromMaybe (Photographer.Photographers photographers') $ asum $ ListZipper.toNonEmpty $ ListZipper.iextend
        (\thisIndex photographers'' -> if selected == thisIndex
            then Just (Photographer.Photographers photographers'')
            else Nothing
        ) photographers'

