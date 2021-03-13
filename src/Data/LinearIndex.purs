
module Data.LinearIndex where

import Extra.Prelude
import Data.Array as Array
import Data.Maybe (Maybe)
import Control.MonadZero (guard)
import Data.Position (Position (..))
import Effect (Effect)

import Effect.Class.Console (log)
import Data.FoldableWithIndex
  ( class FoldableWithIndex
  , foldrWithIndex
  , foldlWithIndex
  , foldMapWithIndex
  )
import Data.Foldable
  ( class Foldable
  , foldr
  , foldl
  , foldMap
  , traverse_
  )

newtype LinearIndex t = LinearIndex
  { width :: Int
  , height :: Int
  , values :: Array t
  }

printLinearIndex :: forall t. Show t => LinearIndex t -> Effect Unit
printLinearIndex (LinearIndex {width, values}) =
  traverse_ (log <<< show) $ chunks width values

chunks :: forall a. Int -> Array a -> Array (Array a)
chunks _ [] = []
chunks n xs = pure (Array.take n xs) <> (chunks n $ Array.drop n xs)


fill :: forall t. Int -> Int -> t -> LinearIndex t
fill width height x =
  LinearIndex { width, height
  , values: Array.replicate (width * height) x
  }

indices :: forall t. LinearIndex t -> Array Position
indices (LinearIndex {width, height}) = do
  x <- Array.range 0 (width - 1)
  y <- Array.range 0 (height - 1)
  pure (V {x,y})

index :: forall t. LinearIndex t -> Position -> Maybe t
index (LinearIndex {width, height, values}) (V {x,y})  = do
  guard (x < width)
  guard (x >= 0)
  guard (y < height)
  guard (y >= 0)
  Array.index values (x + y*width)

insertAt :: forall t. Position -> t -> LinearIndex t -> Maybe (LinearIndex t)
insertAt (V {x,y}) t (LinearIndex {width, height, values}) = do
  guard (x < width)
  guard (x >= 0)
  guard (y < height)
  guard (y >= 0)
  values' <- Array.updateAt (x + y*width) t values
  pure $ LinearIndex { width, height, values: values' }

toPos :: forall a. LinearIndex a -> Int -> Position
toPos (LinearIndex {width}) ix = V { x: ix `mod` width , y: ix `div` width }

instance foldLI :: Foldable LinearIndex where
  foldr f z (LinearIndex {values}) = foldr f z values
  foldl f z (LinearIndex {values}) = foldl f z values
  foldMap f (LinearIndex {values}) = foldMap f values

instance foldWithPosition :: FoldableWithIndex (Vector Int) LinearIndex where
  foldrWithIndex f z l@(LinearIndex {values}) =
    foldrWithIndex (\i a b -> f (toPos l i) a b) z values
  foldlWithIndex f z l@(LinearIndex {values}) =
    foldlWithIndex (\i b a -> f (toPos l i) b a) z values
  foldMapWithIndex f l@(LinearIndex {values}) =
    foldMapWithIndex (\i m -> f (toPos l i) m) values

