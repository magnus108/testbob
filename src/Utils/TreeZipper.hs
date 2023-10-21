{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}

module Utils.TreeZipper
    ( TreeZipper(..)
    , Context(..)
    , mkTreeZipper
    , toRoseTree
    , toContext
    , up
    , down

    {-
    , TreeZipperS(..)
    , Tree(..)
    , ContextT(..)
    , Trunk(..)
    , getCONTEXT
    , upS
    -}
    ) where

import Utils.RoseTree
--import Utils.Comonad


--TODO should this not be Context l [RoseTree bl]?
data Context b l = Context [RoseTree b l] b [RoseTree b l]
    deriving (Show, Eq, Ord, Functor)
    deriving (Generic)
    deriving (FromJSON, ToJSON)







    {-

data Tree l b = b :< (Trunk l b)
    deriving (Show, Eq, Ord, Functor)
    deriving (Generic)
    deriving (Foldable)
    deriving (FromJSON, ToJSON)

data Trunk l b = L l | B [Tree l b]
    deriving (Show, Eq, Ord, Functor)
    deriving (Generic)
    deriving (Foldable)
    deriving (FromJSON, ToJSON)

data ContextT l b = ContextT [Tree l b] b [Tree l b]
    deriving (Show, Eq, Ord, Functor)
    deriving (Generic)
    deriving (FromJSON, ToJSON)

data TreeZipperS l b = TreeZipperS (Tree l b ) [ContextT l b]
    deriving (Show, Eq, Ord, Functor)
    deriving (Generic)
    deriving (FromJSON, ToJSON)

getCONTEXT :: TreeZipperS l b -> [ContextT l b]
getCONTEXT (TreeZipperS b ctx) = ctx

instance Comonad (Tree l) where
    extract (b :< _) = b
    extend k d@(_ :< as) = k d :< case as of
                        L e -> L e
                        B childs -> B (fmap (extend k) childs)

instance Comonad (TreeZipperS l) where
    extract (TreeZipperS x _) = extract x
    extend k d = TreeZipperS (extendT k d) (extendC k d)
            where
                extendC k (TreeZipperS _ [])= []
                extendC k (TreeZipperS thisTree ((ContextT ls a rs):az)) =
                        uu : extendC k d'
                                where d' = TreeZipperS (a :< B (ls ++ rs)) az
                                      uu = ContextT (fmap (\l -> extendT k (TreeZipperS l az)) ls) (k d') (fmap (\r -> extendT k (TreeZipperS r az)) rs)

                extendT k d@(TreeZipperS (a :< as) az) =
                        k d :< case as of
                                L e -> L e
                                B childs -> B (fmap (\(x,y) -> extendT k (TreeZipperS x (y:az))) (toCtx a childs))


toCtx :: b -> [Tree l b] -> [(Tree l b, ContextT l b)]
toCtx parent children = lol (children, [])
    where
        lol ([], _) = []
        lol (x:xs, ys) = (x, ContextT ys parent xs) : lol (xs, x:ys)

upS :: TreeZipperS b l -> Maybe (TreeZipperS b l)
upS (TreeZipperS item (ContextT ls x rs:bs)) =
    Just (TreeZipperS (x :< B (ls <> [item] <> rs)) bs)
upS _ = Nothing
-}


data TreeZipper b l = TreeZipper (RoseTree b l) [Context b l]
    deriving (Show, Eq, Ord, Functor)
    deriving (Generic)
    deriving (FromJSON, ToJSON)

toRoseTree :: TreeZipper b l -> RoseTree b l
toRoseTree (TreeZipper item _) = item

toContext :: TreeZipper b l -> [Context b l]
toContext (TreeZipper _ item) = item

mkTreeZipper :: RoseTree b l -> TreeZipper b l
mkTreeZipper x = TreeZipper x []

up :: TreeZipper b l -> Maybe (TreeZipper b l)
up (TreeZipper item (Context ls x rs:bs)) =
    Just (TreeZipper (Branch x (ls <> [item] <> rs)) bs)
up _ = Nothing


down :: (Eq b, Eq l) => Either b l -> TreeZipper b l -> Maybe (TreeZipper b l)
down x (TreeZipper (Branch parent items) bs) =
    let
        (ls, rs) = break (\item -> datum item == x) items
    in
        case rs of
            y:ys -> Just (TreeZipper y (Context ls parent ys:bs))
            _ -> Nothing
down _ _ = Nothing



{-

--fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)  
up :: TreeZipper b l -> Maybe (TreeZipper b l)
up (TreeZipper item ((Context _ x _):bs)) = Just (TreeZipper x bs)
up _ = Nothing

goToRightMostChild :: TreeZipper a -> Maybe (TreeZipper a)
goToRightMostChild (TreeZipper item@(RoseTree _ (y:ys)) xs) =
        Just (TreeZipper y (ListZipper [] item ys:xs))
goToRightMostChild _  = Nothing

--left :: TreeZipper a -> Maybe (TreeZipper a)
--left (TreeZipper item@(RoseTree x (y:ys)) xs) =
--left _  = Nothing

-}
