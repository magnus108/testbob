{-# LANGUAGE ScopedTypeVariables #-}
module Prelude
    ( module Relude
    , module Json
    , module Conduit
    , readJSONFile
    , readJSONFile'
    , writeJSONFile
    , fromEither
    ) where

import Relude
import Conduit
import Data.Conduit.Attoparsec

import Control.Exception

import Data.Aeson as Json (FromJSON (parseJSON), ToJSON (toJSON), json, Result(Error, Success), fromJSON, fromEncoding, toEncoding)
import System.IO.Error


sinkFromJSON :: (MonadThrow m, FromJSON a) => ConduitM ByteString o m a
sinkFromJSON = do
    value <- sinkParser json
    case fromJSON value of
        Error e -> throwM (userError e)
        Success x -> return x


readJSONFile :: (MonadIO m, FromJSON a) => FilePath -> m a
readJSONFile fp = do
    --traceShowM fp
    liftIO $ runConduitRes $ sourceFile fp .| sinkFromJSON


writeJSONFile :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeJSONFile fp x = liftIO $ runConduitRes $ yield (fromEncoding $ toEncoding x)
                             .| builderToByteString
                             .| sinkFileBS fp


fromEither :: Either a a -> a
fromEither = either id id


readJSONFile' :: (MonadIO m, FromJSON a) => FilePath -> m (Either String a)
readJSONFile' fp = do
    let conduit = runConduitRes $ sourceFile fp .| sinkFromJSON
    liftIO $ try conduit <&> first (\e -> show (e :: SomeException))
