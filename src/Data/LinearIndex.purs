
module Data.LinearIndex where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe)
import Control.MonadZero (guard)
import Data.Position (Position (..))
import Effect (Effect)

import Effect.Class.Console (log)
import Data.Foldable (traverse_)

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
  pure (Position {x,y})

index :: forall t. LinearIndex t -> Position -> Maybe t
index (LinearIndex {width, height, values}) (Position {x,y})  = do
  guard (x < width)
  guard (x >= 0)
  guard (y < height)
  guard (y >= 0)
  Array.index values (x + y*width)

insertAt :: forall t. Position -> t -> LinearIndex t -> Maybe (LinearIndex t)
insertAt (Position {x,y}) t (LinearIndex {width, height, values}) = do
  guard (x < width)
  guard (x >= 0)
  guard (y < height)
  guard (y >= 0)
  values' <- Array.updateAt (x + y*width) t values
  pure $ LinearIndex { width, height, values: values' }


