module Lib.Client.Utils
    ( items
    , item
    , unions'
    , concatenate'
    , selectionChange'
    , bEditing
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


item :: WriteAttr Element (UI Element)
item = mkWriteAttr $ \item' container -> void $
    element container # set children [] #+ [item']

items :: WriteAttr Element [UI Element]
items = mkWriteAttr $ \item' container -> void $
    element container # set children [] #+ item'


unions' :: NonEmpty (Event a) -> Event (NonEmpty a)
unions' = foldr (unionWith (<>)) never . fmap (fmap (:|[]))

concatenate' :: Foldable f => f (a -> a) -> (a -> a)
concatenate' = foldr (.) id


unsafeMapUI :: Element -> (t -> UI b) -> Event t -> Event b
unsafeMapUI el f = unsafeMapIO (\a -> getWindow el >>= \w -> runUI w (f a))


selectionChange' :: Element -> Event (Maybe Int)
selectionChange' el = readMaybe <$> unsafeMapUI el (const $ UI.get UI.value el) (domEvent "change" el)


bEditing :: Element -> UI (Behavior Bool)
bEditing element' = stepper False $ and
    <$> unions [True <$ UI.focus element', False <$ UI.blur element']
