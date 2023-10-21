{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Location
    ( locationSection
    , mkGrades
    , mkModel
    , selectGrade
    )
where

import Lib.App (Action(..))
import qualified Control.Concurrent.Chan.Strict as Chan

import           Data.Char
import           Lib.Client.Utils
import qualified Control.Lens                  as Lens

import           Reactive.Threepenny
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI

import           Utils.Comonad
import qualified Utils.ListZipper              as ListZipper

import           Lib.Translation
import           Lib.Data
import qualified Lib.Grade                     as Grade
import           Lib.Tab
import qualified Lib.Location                  as Location
import           Lib.Client.Tab

import           Lib.Client.Element
import           Lib.App                        ( Env(..)
                                                )


data Item = Item { location :: Location.LocationFile
                 , grades :: Grade.Grades
                 }

newtype Model = Model { unModel :: Data String Item }

mkModel :: Location.Model -> Grade.Model -> Model
mkModel location grades =
    Model $ Item <$> Location.unModel location <*> Grade._grades grades


insertButton :: Env -> Window -> Translation -> UI (Element, Element)
insertButton _ _ translations = do
    button <- mkButton "insert" (Lens.view newGrade translations)
    content <- UI.div #. "section" #+ [element button]
    return (button, content)


selectSection :: Env -> Window -> Translation -> Element -> Element -> Grade.Grades -> UI Element
selectSection env _ _ input select grades = do
    _ <- element input # set value (Grade.showGrade grades)
    lol <- mkGrades env grades
    _ <- element select # set children [] #+ fmap element lol
    content <-
        UI.div
        #. "section"
        #+ [ UI.div
                #. "field is-horizontal"
                #+ [ UI.div
                    #. "field-body"
                    #+ [ UI.div
                        #. "field"
                        #+ [ UI.p
                            #. "control"
                            #+ [element input #. "input"]
                        ]
                        , UI.div
                        #. "field"
                        #+ [UI.p #. "control" #+ [UI.div #. "select" #+ [element select]]]
                        ]
                ]
            ]

    return content


sinkModel :: Env -> Window -> Translation -> Behavior Model -> UI (Element, Element)
sinkModel env@Env{..} win translations bModel = do
    input   <- UI.input
    select  <- UI.select
    (button, buttonContent) <- insertButton env win translations

    bEditingInput                     <- bEditing input
    bEditingSelect  <- bEditing select

    content <- UI.div
    locationFileView' <- UI.div #. "section"

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
                    msg <- Lens.views locationPageError string translations
                    err <- UI.div #+ [string e]

                    -- make <- mkFileMaker "locationsPicker" (Lens.view newLocation translations)
                    --      $ \file -> when (file /= "") $ void $ Location.writeLocationFile mLocationConfigFile (Location.LocationFile file)

                    pick <- mkFilePicker "locationFilePicker" (Lens.view pickLocation translations)
                                    $ \file -> when (file /= "") $ void $ 
                                        Chan.writeChan chan (WriteLocation (Location.LocationFile file))

                    pickers <- UI.div #. "buttons has-addons" # set children [pick] -- , make]

                    section <- UI.div #. "section" # set children [msg, err, pickers]
                    _ <- element content # set children [section]
                    return ()

                Data (Item locfile grades) -> do
                    _ <- element locationFileView' # set children [] #+ (locationFileView env translations locfile)

                    _ <- case (extract (Grade._unGrades grades)) of
                            (Grade.Unknown _) -> do
                                runFunction  $ ffi "$(%1).removeAttr('disabled')" (input)
                            (Grade.Known _ ) -> do
                                void $ element input # set (attr "disabled") "true"

                    selectInputSection <- selectSection env win translations input select grades
                    _ <- element content # set children [locationFileView', selectInputSection, buttonContent]
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
                msg <- Lens.views locationPageError string translations
                err <- UI.div #+ [string e]
                section <- UI.div #. "section" # set children [msg, err]
                _ <- element content # set children [section]
                return ()
            Data (Item locfile grades) -> do
                editingInput <- liftIO $ currentValue bEditingInput
                editingSelect <- liftIO $ currentValue bEditingSelect

                _ <- case (extract (Grade._unGrades grades)) of
                        (Grade.Unknown _) -> do
                            runFunction  $ ffi "$(%1).removeAttr('disabled')" (input)
                        (Grade.Known _ ) -> do
                            void $ element input # set (attr "disabled") "true"

                when (not editingInput ) $ void $
                    element input # set value (Grade.showGrade grades)

                when (not editingSelect) $ void $ do
                    lol <- mkGrades env grades
                    element select # set children [] #+ fmap element lol

                _ <- element locationFileView' # set children [] #+ (locationFileView env translations locfile)

                when (not (editingInput || editingSelect)) $ void $ do
                    selectInputSection <- selectSection env win translations input select grades
                    _ <- element content # set children [locationFileView', selectInputSection, buttonContent ]
                    return ()

    let eClick    = Grade.mkNewGrade <$ UI.click button
    let eInput    = Grade.inputGrade <$> UI.valueChange input
    let eSelect   = selectGrade <$> filterJust (selectionChange' select)
    let allEvents = concatenate' <$> unions' (eSelect :| [eInput, eClick])
    let ee  = filterJust
                $   fmap
                        (\m f -> case toJust (unModel m) of
                            Nothing -> Nothing
                            Just x ->
                                Just
                                    (Model
                                        (Data (Item (location x) (f (grades x))))
                                    )
                        )
                        bModel
                <@> allEvents

    _ <- onEvent ee $ \model -> do
        void $ liftIO $ do
            case toJust (unModel model) of
                Nothing -> return ()
                Just item'  -> do
                    --Location.writeLocationFile mLocationConfigFile (location i)
                    _ <- Chan.writeChan chan (WriteGrades (grades item'))
                    return ()

    return (input, content)


locationSection :: Env -> Window -> Translation -> Element -> Element -> Behavior Model -> UI Element
locationSection env@Env {..} win translations tabs' navigation bModel = do

    (input, view) <- sinkModel env win translations bModel

    UI.div # set children [tabs', view, navigation]

    --UI.setFocus (getElement input) -- Can only do this if element exists and should not do this if not focus


--TODO a little flawed
mkGrades :: Env -> Grade.Grades -> UI [Element]
mkGrades env (Grade.Grades grades') = do
    let elems = ListZipper.iextend (\index grades'' -> (index, grades' == grades'', extract grades'')) grades'
    let elems' = sortOn (\(_,_,g) -> fmap toLower (Grade.showGrade' g)) $ toList elems
    mapM (mkGrade env) elems'


mkGrade :: Env -> (Int, Bool, Grade.Grade) -> UI Element
mkGrade Env {..} (thisIndex, isCenter, grade) = do
    let name   = Grade.showGrade' grade
    let option = UI.option # set value (show thisIndex) # set text name
    if isCenter then option # set UI.selected True else option


selectGrade :: Int -> Grade.Grades -> Grade.Grades
selectGrade selected (Grade.Grades grades') =
        -- TODO this just wierd
    fromMaybe (Grade.Grades grades') $ asum $ ListZipper.toNonEmpty $ ListZipper.iextend
        (\thisIndex grades'' -> if selected == thisIndex
            then Just (Grade.Grades grades'')
            else Nothing
        ) grades'


locationFileView :: Env -> Translation -> Location.LocationFile -> [UI Element]
locationFileView Env {..} translations locationFile = do
    let title_  = UI.div #+ [Lens.views locationTitle string translations]
    let content = UI.div #+ [UI.string (Location.unLocationFile locationFile)]


    --let make = mkFileMaker "locationsPicker" (Lens.view newLocation translations)
     --       $ \file -> when (file /= "") $ void $ Location.writeLocationFile mLocationConfigFile (Location.LocationFile file)

    let pick = mkFilePicker "locationFilePicker" (Lens.view pickLocation translations)
                    $ \file -> when (file /= "") $ void $ do
                        Chan.writeChan chan (WriteLocation (Location.LocationFile file))

    let pickers = UI.div #. "buttons has-addons" #+ [pick] --, make]

    --let open = mkOpenFile "open"
     --                     (Lens.view openLocation translations)
      --                    (Location.unLocationFile locationFile)

    [title_, content, pickers] --, open]
