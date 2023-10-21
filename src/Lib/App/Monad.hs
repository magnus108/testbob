{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib.App.Monad
       ( AppT
       , unAppT
       , runAppT
       , runAppAsIO
       ) where

import Control.Exception (try)
import Relude.Extra.Bifunctor (firstF)

import Lib.App.Env (Env)
import Lib.App.Error (Error, unException)

newtype AppT m a = AppT
    { unAppT :: ReaderT Env m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadReader Env
               )


runAppAsIO :: Env -> AppT IO a -> IO (Either Error a)
runAppAsIO env = firstF unException . try . runAppT env


runAppT :: Env -> AppT m a -> m a
runAppT env = usingReaderT env . unAppT
