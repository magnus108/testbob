{-# LANGUAGE DeriveAnyClass #-}

module Lib.App.Error
    ( Error (..)
    , unException
    ) where

import qualified Control.Exception as Exception


newtype Exception = Exception
    { unException :: Error
    } deriving (Show)
      deriving anyclass (Exception.Exception)

data Error = Error
    deriving (Show)
