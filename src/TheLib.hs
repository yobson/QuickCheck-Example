module TheLib where

data Set a = Empty | Sing a | Union (Set a) (Set a)
  deriving Show


-- Functor

instance Functor Set where
  fmap _ Empty         = Empty
  fmap f (Sing x)      = Sing $ f x
  fmap f (Union s1 s2) = Union (f <$> s1) (f <$> s2)

-- Semigroup

instance Semigroup (Set a) where
  (<>) = Union

-- Monoid
instance Monoid (Set a) where
  mempty = Empty

-- Eq

foldSet :: b -> (a -> b) -> (b -> b -> b) -> Set a -> b
foldSet empty sing union (Empty)       = empty
foldSet empty sing union (Sing x)      = sing x
foldSet empty sing union (Union s1 s2) = foldSet empty sing union s1 `union` foldSet empty sing union s2

isIn :: (Eq a) => a -> Set a -> Bool
isIn x = foldSet False (== x) (||)

isSubset :: (Eq a) => Set a -> Set a -> Bool
isSubset s1 s2 = foldSet True (`isIn` s2) (&&) s1

instance Eq a => Eq (Set a) where
  s1 == s2 = s1 `isSubset` s2 && s2 `isSubset` s1

-- Foldable
-- Applicative

-- Monad

{-# RULES
  "set/unit0"   forall s .  s <> Empty = s
    #-}
