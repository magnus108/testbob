{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Main
    ( Model(..)
    , Item(..)
    , location
    , dump
    , dumpDir
    , photographees
    , session
    , dagsdato
    , camera
    , shooting
    , doneshooting
    , dagsdatoBackup
    , photographer
    , grades
    , unModel
    , build
    ) where


import Lib.Data
import Control.DeepSeq
import qualified Lib.Photographee as Photographee
import qualified Lib.Grade as Grade
import qualified Lib.Dump as Dump
import qualified Lib.Session as Session
import qualified Lib.Location as Location
import qualified Lib.Camera as Camera
import qualified Lib.Dagsdato as Dagsdato
import qualified Lib.DagsdatoBackup as DagsdatoBackup

import qualified Lib.Shooting as Shooting
import qualified Lib.Doneshooting as Doneshooting
import qualified Lib.Photographer as Photographer
import qualified Lib.Build as Build

import Control.Lens


-- OVERVEJ DER SKAL LAVES EN SPECIFIKT TIL BUILD
data Item = Item { _location :: Location.LocationFile
                 , _grades :: Grade.Grades
                 , _dump :: Dump.Dump
                 , _dumpDir :: Dump.DumpDir --TODO this is wrong
                 , _photographees :: Photographee.Photographees
                 , _session :: Session.Session
                 , _camera :: Camera.Camera
                 , _dagsdato :: Dagsdato.Dagsdato
                 , _shooting :: Shooting.Shooting
                 , _doneshooting :: Doneshooting.Doneshooting
                 , _photographer :: Photographer.Photographer
                 , _dagsdatoBackup :: DagsdatoBackup.DagsdatoBackup
                 , _build :: Build.Build
                 } deriving (Show, Generic ,NFData)

makeLenses ''Item

newtype Model = Model { _unModel :: Data String Item } deriving Show

makeLenses ''Model
