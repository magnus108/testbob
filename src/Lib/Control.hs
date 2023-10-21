module Lib.Control
    ( controlXMP
    , Error(..)
    , translationError
    , parseRating
    , between
    , Results(..)
    ) where
import qualified Lib.Translation as Translation --todo should not be here

import Data.Strings
import qualified Relude.Unsafe as Unsafe
import Data.Maybe

import qualified Utils.ListZipper as ListZipper
import Data.List.Extra hiding (elem)
import qualified Control.Lens as Lens
import qualified Lib.ControlModel as ControlModel
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (unpack)
import qualified Lib.Doneshooting as Doneshooting
import qualified Lib.Rating as Rating
import qualified Lib.Photographee as Photographee
import System.FilePath


between :: String -> String -> String -> String
between prefix' postfix' xs = rating
    where
        (_, after) = strSplit prefix' xs
        (rating, _) = strBreak postfix' after

parseRating :: FilePath -> IO Rating.Rating
parseRating filepath = do
    content <- BS.readFile filepath
    seq content (return ())
    let prefix' = "<xmp:Rating>"
    let postfix' = "</xmp:Rating>"
    let rating = between prefix' postfix' (unpack content)
    return (Rating.fromString rating)


data Error
    = Exactly1With5
    | Atleast5With1
    | CouldNotReadDoneshootingDir
    deriving (Show,Eq)


newtype Results = Results { _unResults :: [(Photographee.Photographee, Error)] }
    deriving Show

--
--TODO this is rediculose
translationError :: Error -> Translation.Translation -> String
translationError err = Lens.view translator
    where translator = case err of
            Exactly1With5 -> Translation.exactly1With5
            Atleast5With1 -> Translation.atleast5With1
            CouldNotReadDoneshootingDir -> Translation.couldNotReadDoneshootingDir


count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)


controlXMP :: ControlModel.Item -> IO Results
controlXMP item' = do
    let files = Doneshooting.unDoneshootingDir $ Lens.view ControlModel.doneshootingDir item'
    let photographees' = ListZipper.toList $ Photographee.unPhotographees $ Lens.view ControlModel.photographees item'
    let xmps = fst $ snd files
    let root = snd $ snd files
    let crs = fst files

    --SUPER HACK
    let pairPhotographeeAndCr' = map (\photographee' ->  (photographee', filter (\file' -> isInfixOf ("TEA_" ++ (Photographee.toSys' photographee')) file') crs ++ filter (\file' -> isInfixOf ("SYS_" ++ (Photographee.toSys' photographee')) file') crs)) photographees'
    let pairPhotographeeAndCr = filter (\x -> snd x /= []) pairPhotographeeAndCr' 
    let fuck = fmap (\x -> (fst x, filter isGroup (snd x))) $ filter (\x -> [] /= filter isGroup (snd x)) pairPhotographeeAndCr
    let fuck' = fmap changeName fuck

    let pairPhotographeeAndCrFix = fmap (\x -> (fst x, filter (not . isGroup) (snd x))) $ filter (\x -> [] /= filter (not . isGroup) (snd x)) pairPhotographeeAndCr

    let fix = pairPhotographeeAndCrFix ++ fuck'

    let pairPhotographeeAndCrAndXmp = 
            (\i -> do
                (i, catMaybes $ map (\cr -> find (\xmp -> (cr -<.> "xmp") == xmp) xmps) (snd i))
            ) <$> fix

    parsedAndRdy <- parsedAndRdy' root pairPhotographeeAndCrAndXmp

    let gg = fmap (\i -> if (1 == count Rating.five (snd i)) then Nothing else Just (i, Exactly1With5)) parsedAndRdy
    let gg2 = fmap (\i -> if length (filter (\i' -> Rating.toInt i' >= 1) (snd i)) > 5 then Nothing else Just (i, Atleast5With1)) parsedAndRdy

    let allErro = fmap (\i -> (fst (fst( fst i)), snd i)) $ catMaybes $ gg++gg2
    
    return (Results allErro)


parsedAndRdy' :: FilePath -> [((Photographee.Photographee, [FilePath]), [FilePath])]-> IO [((Photographee.Photographee, [FilePath]), [Rating.Rating])]
parsedAndRdy' root x = mapM (\x -> do 
        rate <- mapM (\y -> parseRating (root </> y)) (snd x)
        return (fst x,rate)) x


isGroup :: String -> Bool
isGroup x = Unsafe.at 2 ((splitOn "." x)) == "3"

changeName :: (Photographee.Photographee, [String]) -> (Photographee.Photographee, [String])
changeName (x,y) = (Photographee.setName' ("Gruppe " ++ (Photographee.toName' x)) x, y)

