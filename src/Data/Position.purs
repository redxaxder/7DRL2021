module Data.Position where

import Prelude

newtype Position = Position { x :: Int, y :: Int }

instance showPosition :: Show Position where
  show (Position r) = show r

instance semigroupPosition :: Semigroup Position where
  append (Position p) (Position q) = Position
    { x: p.x + q.x
    , y: p.y + q.y
    }

derive instance eqPosition :: Eq Position
derive instance ordPosition :: Ord Position

