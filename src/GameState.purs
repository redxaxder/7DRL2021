module GameState where

import Extra.Prelude
import Framework.Direction (Direction, move)

newState :: Effect GameState
newState = pure $
  GameState
   { p: V {x: 1, y:1}
   }


step :: GameState -> GameAction -> Either FailedAction GameState
step (GameState gs) a@(GameAction _ dir) =
  let p' = move dir gs.p
   in if inBounds p'
      then Right (GameState gs {p = p'})
      else Left (FailedAction dir)

inBounds :: Vector Int -> Boolean
inBounds (V{x,y}) = 
  0 <= x && x <= 6 && 0 <= y && y <= 6

newtype GameState = GameState
  { p :: Vector Int
  }
data GameAction = GameAction Instant Direction
data FailedAction = FailedAction Direction

