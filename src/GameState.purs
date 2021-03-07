module GameState where

import Extra.Prelude
import Framework.Direction (Direction, move)
import Data.Set as Set
import Data.Map as Map
import Data.Array as Array
import Data.LinearIndex (LinearIndex)
import Data.LinearIndex as LinearIndex
import Data.Tuple as Tuple
import Random.Gen as R

newState :: Effect GameState
newState = do
  random <- R.newGen
  pure $ GameState
   { p: V {x: 1, y:1}
   , playerHealth: freshPlayerHealth
   , enemies: Map.empty
   , terrain: LinearIndex.fill 40 40 Floor -- TODO: adjust dimensions
   , rng: random
   }

data Terrain = Wall | Floor | Exit

freshPlayerHealth :: Health
freshPlayerHealth = Health
  { hpCount: hpCount freshPlayerBoard
  , board: freshPlayerBoard
  }

hpCount :: Board -> Int
hpCount board = Array.length $ Array.filter isHp (intactOrgans board)
  where
  isHp (Organ _ Hp) = true
  -- isHp _ = false

intactOrgans :: Board -> Array Organ
intactOrgans (Board board) =
  Tuple.fst <$> Array.filter noInjuries board.organs
  where
  noInjuries (Tuple (Organ (OrganSize w h) _) (BoardCoord (V {x, y}))) =
    let xmin = x
        xmax = w + x - 1
        ymin = y
        ymax = y + h - 1
    in any (\(BoardCoord (V i)) ->
       i.x >= xmin
       && i.x <= xmax
       && i.y >= ymin
       && i.y <= ymax) board.injuries

freshPlayerBoard :: Board
freshPlayerBoard = Board
  { organs: [ Tuple playerHpOrgan (BoardCoord (vec 2 2)) ]
  , injuries: Set.empty
  }

playerHpOrgan :: Organ
playerHpOrgan = Organ (OrganSize 2 2) Hp

step :: GameState -> GameAction -> Either FailedAction GameState
step (GameState gs) a@(Move dir) =
  let p' = move dir gs.p
   in if inBounds p'
      then Right (GameState gs {p = p'})
      else Left (FailedAction dir)
step _ _ = todo

inBounds :: Vector Int -> Boolean
inBounds (V{x,y}) =  -- TODO: fix this
  0 <= x && x <= 6 && 0 <= y && y <= 6

newtype GameState = GameState
  { p :: Vector Int
  , playerHealth :: Health
  , enemies :: Map EnemyId Enemy
  , terrain :: LinearIndex Terrain
  , rng :: R.Gen
  }

type EnemyId = Int
type WeaponId = Int

data GameAction =
  Move Direction
  | Attack BoardCoord EnemyId
  | SelectWeapon WeaponId
  | InstallOrgan Organ BoardCoord
  | RemoveOrgan Int

data FailedAction = FailedAction Direction

newtype Health = Health 
  { hpCount :: Int
  , board :: Board
  }

data OrganSize = OrganSize Int Int
data OrganType = Hp
data Organ = Organ OrganSize OrganType

type Enemy = { location :: Vector Int, health :: Health, tag :: EnemyTag }
data EnemyTag = Roomba

newtype BoardCoord = BoardCoord (Vector Int)
newtype Board = Board
  { organs :: Array (Tuple Organ BoardCoord)
  -- , organIndex :: Map BoardCoord Organ
  , injuries :: Set BoardCoord
  }

