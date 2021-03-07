module Data.DefaultMap where

import Prelude
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.FoldableWithIndex (class FoldableWithIndex, foldrWithIndex, foldlWithIndex, foldMapWithIndex)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe (..))
import Data.Set (Set)

data DefaultMap k v = DefaultMap v (Map k v)

instance showDefaultMap :: (Show k, Show v) => Show (DefaultMap k v) where
  show (DefaultMap default m) =
    "DefaultMap (" <> show default <> " " <> show m <> ")"

instance foldableDefaultMap :: Foldable (DefaultMap k) where
  foldMap f (DefaultMap _ x) = foldMap f x
  foldr f z (DefaultMap _ x) = foldr f z x
  foldl f z (DefaultMap _ x) = foldl f z x

instance foldableWithIndexDefaultMap :: FoldableWithIndex k (DefaultMap k) where
  foldrWithIndex f z (DefaultMap _ xs) = foldrWithIndex f z xs
  foldlWithIndex f z (DefaultMap _ xs) = foldlWithIndex f z xs
  foldMapWithIndex f (DefaultMap _ xs) = foldMapWithIndex f xs

derive instance eqDefaultMap :: (Eq k, Eq v) => Eq (DefaultMap k v)

keys :: forall k v. DefaultMap  k v -> Set k
keys (DefaultMap _ m) = Map.keys m

empty :: forall k v. Ord k => v -> DefaultMap k v
empty default = DefaultMap default mempty

lookup :: forall k v. Ord k => k -> DefaultMap k v -> v
lookup k (DefaultMap default m) =
  case Map.lookup k m of
       Nothing -> default
       Just x -> x

-- | Inserts or updates a value with the given function.
-- |
-- | The combining function is called with the existing value as the first
-- | argument and the new value as the second argument.
insertWith :: forall k v
  . Ord k => Eq v
  => (v -> v -> v)
  -> k -> v
  -> DefaultMap k v -> DefaultMap k v
insertWith f k v d =
  let v' = f (lookup k d) v
   in insert k v' d

insert :: forall k v
  . Ord k => Eq v
  => k -> v -> DefaultMap k v -> DefaultMap k v
insert k v (DefaultMap default m) =
  let m' = if v == default
             then Map.delete k m
             else Map.insert k v m
   in DefaultMap default m'




