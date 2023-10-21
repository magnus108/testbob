module Lib.Client.Element
    ( mkButton
    , mkFolderPicker
    , mkFilePicker
    , mkLabel
    , control
    , mkFileMaker
    , mkOpenFile
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Utils.ListZipper (ListZipper)

-- condition
-- title
-- id
-- settings
-- action
-- tabs/zipper
control :: (ListZipper a -> Bool, String,String) -> ListZipper a -> (ListZipper a -> UI ()) -> UI (Maybe Element)
control (position, idd, name) items action
    | position items = return Nothing
    | otherwise = do
        button <- mkButton idd name
        UI.on UI.click button $ \_ -> action items
        return (Just button)


mkLabel :: String -> UI Element
mkLabel s = UI.p #. "has-text-dark has-text-weight-bold" # set UI.text s


mkButton :: String -> String -> UI Element
mkButton id' x = UI.button # set UI.id_ id' #. "button" #+ [string x]


mkFolderPicker :: String -> String -> (FilePath -> IO ()) -> UI Element
mkFolderPicker = mkShowOpenDialog ["openDirectory"]


mkFilePicker :: String -> String -> (FilePath -> IO ()) -> UI Element
mkFilePicker = mkShowOpenDialog ["openFile"]


mkShowOpenDialog :: [String] -> String -> String -> (FilePath -> IO ()) -> UI Element
mkShowOpenDialog options id' title' fx = do
    button <- mkButton id' title'

    UI.on UI.click button $ \_ -> do
        callback <- ffiExport fx
        runFunction $ ffi "require('electron').remote.dialog.showOpenDialog({properties: %2}).then(result => %1(result.filePaths[0]))" callback options 

    return button


mkOpenFile :: String -> String -> FilePath -> UI Element
mkOpenFile id' title' file = do
    button <- mkButton id' title'
    UI.on UI.click button $ \_ ->
        runFunction $ ffi $ "require('electron').shell.openPath(" ++ show file ++ ")"
    return button

mkFileMaker :: String -> String -> (FilePath -> IO ()) -> UI Element
mkFileMaker = mkShowSaveDialog []


mkShowSaveDialog :: [String] -> String -> String -> (FilePath -> IO ()) -> UI Element
mkShowSaveDialog options id' title' fx = do
    button <- mkButton id' title'

    UI.on UI.click button $ \_ -> do
        callback <- ffiExport fx
        runFunction $ ffi "require('electron').remote.dialog.showOpenDialog({properties: %2}).then(result => %1(result.filePaths[0]))" callback options 

    return button
