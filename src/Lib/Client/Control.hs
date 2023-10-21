module Lib.Client.Control
    ( controlSection
    ) where

import Lib.App (Action(..))
import qualified Control.Concurrent.Chan.Strict as Chan

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI


import qualified Lib.Client.Location as CLocation
import qualified Control.Lens as Lens

import           Lib.Client.Tab
import           Lib.Translation
import           Lib.Data
import           Lib.Tab
import qualified Lib.ControlModel as Model
import qualified Lib.Grade as Grade
import qualified Lib.Photographee as Photographee
import qualified Lib.Control as Control
import qualified Lib.Doneshooting as Doneshooting
import           Lib.App                        ( Env(..))
import Lib.Client.Utils
import Lib.Client.Element


mkControl :: Env -> Translation -> Element -> Model.Model -> UI Element
mkControl env@Env{..} translations select model =
    case Lens.view Model.unModel model of
        NotAsked -> UI.p #+ [Lens.views starting string translations]
        Loading -> UI.p #+ [Lens.views loading string translations]
        Failure e -> do
            err <- UI.p #+ [Lens.views controlError string translations]
            err' <- UI.p #+ [string e]

            UI.div #. "section" # set children [err, err']
        Data item' -> do
            counter <- UI.div #. "section" #+ [ mkLabel (Lens.view doneshootingDirCounter translations)
                                , UI.string (show $ Doneshooting.count (Lens.view Model.doneshootingDir item'))
                                ]


            options <- CLocation.mkGrades env (Lens.view Model.grades item')
            _ <- element select # set children [] #+ fmap element options
            selectGradeSection <- UI.div #. "section" #+ [UI.div #. "select" # set children [select]]

            --BAD BRUh
            lola <- liftIO $ Control.controlXMP item'
            let ratings = (\x -> UI.div #. "section" #+ [UI.p #+ [string (Photographee.toSys' (fst x)), string " ", string (Photographee.toName' (fst x))], string (Control.translationError (snd x) translations)]) <$> (Control._unResults lola)

            status <- mkStatus translations lola (Lens.view Model.doneshootingDir item')

            UI.div # set children ([counter, status, selectGradeSection]) #+ ratings


mkStatus :: Translation -> Control.Results -> Doneshooting.DoneshootingDir -> UI Element
mkStatus translations results dir =
    case (Control._unResults results, Doneshooting.count dir) of
      ([], 0) -> 
          UI.div #. "section" #+ [UI.span #. "icon is-large has-text-warning" #+ [UI.italics #. "fas fa-3x fa-exclamation-triangle"]
            , UI.div #+ [Lens.views doneshootingEmpty string translations]]
      ([], _) -> 
          UI.div #. "section" #+ [UI.span #. "icon is-large has-text-success" #+ [UI.italics #. "fas fa-3x fa-check-square"]]
      (_, _) -> 
          UI.div #. "section" #+ [UI.span #. "icon is-large has-text-danger" #+ [UI.italics #. "fas fa-3x fa-ban"]
            , UI.div #+ [Lens.views controlError string translations]]


controlSection :: Env -> Window -> Translation -> Element -> Element  -> Behavior Model.Model -> UI Element
controlSection env@Env{..} win translation tabs' navigation bModel = do
    selectGrade <- UI.select
    let eSelect = CLocation.selectGrade <$> filterJust (selectionChange' selectGrade)
    let gradeEvent = concatenate' <$> unions' (eSelect :| [])
    let ee  = filterJust
                $   fmap
                        (\m f -> case toJust (Lens.view Model.unModel m) of
                            Nothing -> Nothing
                            Just x -> Just $ Model.Model $ Data $ Lens.over Model.grades f x
                        )
                        bModel
                <@> gradeEvent

    _ <- onEvent ee $ \model -> do
        void $ liftIO $ do
            case toJust (Lens.view Model.unModel model) of
                Nothing -> return ()
                Just item'  -> do
                    _ <- 
                        Chan.writeChan chan $ WriteGrades (Lens.view Model.grades item')
                    return ()


    let bView = mkControl env translation selectGrade <$> bModel

    content <- UI.div # sink item bView


    view <- UI.div #+ fmap element [ content ]

    UI.div # set children [tabs', view, navigation]
