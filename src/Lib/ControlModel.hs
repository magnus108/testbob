{-# LANGUAGE TemplateHaskell #-}
module Lib.ControlModel
    ( mkModel
    , Model(..)
    , Item(..)
    , photographees
    , doneshootingDir
    , grades
    , unModel
    ) where

import           Lib.Data
import Control.Lens
import qualified Lib.Photographee as Photographee
import qualified Lib.Grade as Grade
import qualified Lib.Doneshooting as Doneshooting

data Item = Item
    { _grades :: Grade.Grades
    , _doneshootingDir :: Doneshooting.DoneshootingDir
    , _photographees :: Photographee.Photographees
    }

makeLenses ''Item

newtype Model = Model { _unModel :: Data String Item }

makeLenses ''Model

mkModel :: Grade.Model -> Doneshooting.DoneshootingDirModel -> Photographee.Model -> Model
mkModel grades' doneshootingDir' photograhees' =
    Model $ Item <$> Grade._grades grades' <*> (Doneshooting.unDoneshootingDirModel doneshootingDir') <*> (Photographee._unModel photograhees')


