{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Utils.RoseTree
    ( RoseTree(..)
    , datum
    , children
    ) where


data RoseTree b l = Leaf l | Branch b [RoseTree  b l] --TODO fixpoint
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)
    deriving (Functor, Foldable, Traversable)


instance Bifunctor RoseTree where
    bimap f g (Branch b bs) = Branch (f b) (bimap f g <$> bs)
    bimap _ g (Leaf l) = Leaf (g l)


instance Bifoldable RoseTree where
    bifoldMap f g (Branch b bs) = f b `mappend` foldMap (bifoldMap f g) bs
    bifoldMap _ g (Leaf l) = g l


instance Bitraversable RoseTree where
    bitraverse f g (Branch b bs) = Branch <$> f b <*> traverse (bitraverse f g) bs
    bitraverse _ g (Leaf l) = Leaf <$> g l


datum :: RoseTree b l -> Either b l
datum (Branch b _) = Left b
datum (Leaf l) = Right l

children :: RoseTree b l -> [RoseTree b l]
children (Branch _ xs) = xs
children _ = []
