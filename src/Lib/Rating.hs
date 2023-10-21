module Lib.Rating
    ( Rating
    , five
    , fromString
    , toInt
    ) where

import Prelude hiding (fromString)

data Rating
    = One
    | Two
    | Three
    | Four
    | Five
    | Empty
    deriving (Show,Eq,Ord)

five :: Rating
five = Five

toInt :: Rating -> Int
toInt = \case
    One -> 1
    Two -> 2
    Three -> 3
    Four -> 4
    Five -> 5
    Empty -> 0

fromString :: String -> Rating
fromString = \case
    "1" -> One
    "2" -> Two
    "3" -> Three
    "4" -> Four
    "5" -> Five
    _ -> Empty
