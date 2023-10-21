module Lib.Client.Tab
    ( mkTabs
    , mkTab
    , next
    , prev
    , mkNavigation
    ) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import qualified Lib.Tab as Tab
import Lib.Translation (Translation)
import qualified Lib.Translation as Translation

import Lib.Client.Element
import Utils.Comonad
import Utils.ListZipper (focus, backward, forward)
import qualified Utils.ListZipper as ListZipper
import Control.Concurrent.MVar
import Lib.App (Env(..)) 

import qualified Control.Lens as Lens

mkTabs :: Env -> Translation -> Tabs -> UI Element
mkTabs env translations (Tabs tabs) = do
    tabs' <- mapM (mkTab env translations) elems
    UI.div #. "buttons has-addons" #+ fmap element (ListZipper.toList tabs')
        where 
            --TODO Lav en indexedmap for zippers
            currentTab = focus tabs
            elems = tabs =>> \tabs'' ->
                let
                    thisTab = focus tabs''
                in
                    (thisTab, thisTab == currentTab, Tabs tabs'')


mkTab :: Env -> Translation -> (Tab, Bool, Tabs) -> UI Element
mkTab Env{..} translations (tab, isCenter, tabs)
    | isCenter = do
        let name = Lens.view (Tab.toTranslation tab) translations
        mkButton "idd" name #. "button is-selected" # set (attr "disabled") "true"
    | otherwise = do
        let name = Lens.view (Tab.toTranslation tab) translations
        button <- mkButton "idd" name
        UI.on UI.click button $ \_ ->
            liftIO $ withMVar mTabsFile $ \ tabsFile ->
                writeTabs tabsFile tabs
        return button


prev :: Env -> Translation -> Tabs -> UI (Maybe Element)
prev Env{..} translation tabs =
    control (ListZipper.isLeft, "prev", Lens.view Translation.prev translation) (unTabs tabs) $ \ _ ->
            liftIO $ withMVar mTabsFile $ \ tabsFile ->
                writeTabs tabsFile (Tabs (backward (unTabs tabs)))


next :: Env -> Translation -> Tabs -> UI (Maybe Element)
next Env{..} translation tabs =
    control (ListZipper.isRight,"next",Lens.view Translation.next translation) (unTabs tabs) $ \ _ ->
        liftIO $ withMVar mTabsFile $ \ tabsFile ->
                writeTabs tabsFile (Tabs (forward (unTabs tabs)))


mkNavigation :: Env -> Translation -> Tabs -> UI Element
mkNavigation env translation tabs = do
    next' <- next env translation tabs
    prev' <- prev env translation tabs
    UI.div #. "buttons has-addons"
        #+ fmap element (maybeToList prev' ++ maybeToList next')
