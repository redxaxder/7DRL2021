module GameState where

import Extra.Prelude
import Framework.Direction (Direction, move)
import Data.Set as Set
import Data.Map as Map
import Data.LinearIndex (LinearIndex (..))
import Data.LinearIndex as LI
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.Position (Position (..))
import Data.Terrain (Terrain (..), demoTerrain, charToTerrain)
import Data.Ord (abs)
import Data.Board
  ( BoardCoord(..)
  , Board(..)
  , Clue
  , Organ(..)
  , OrganSize(..)
  , OrganType(..)
  , hpCount
  , injureBoard
  , getClue
  )
import Random.Gen as R

newState :: Effect GameState
newState = do
  random <- R.newGen
  pure $ GameState
   { p: V {x: 1, y:1}
   , playerHealth: freshPlayerHealth
   , enemies: Map.empty
   , terrain: fromMaybe (LI.fill 40 40 Floor) (freshTerrainFromString demoTerrain)
   , rng: random
   }

freshTerrainFromString :: String -> Maybe (LinearIndex Terrain)
freshTerrainFromString s = if String.length s == 40*40 then Just $ LinearIndex {width: 40, height: 40, values: t} else Nothing
  where
    t = map charToTerrain $ toCharArray s

freshPlayerHealth :: Health
freshPlayerHealth = Health
  { hpCount: hpCount freshPlayerBoard
  , board: freshPlayerBoard
  }

freshPlayerBoard :: Board
freshPlayerBoard = Board
  { organs: [ Tuple playerHpOrgan (BoardCoord (vec 2 2)) ]
  , injuries: Set.empty
  }

playerHpOrgan :: Organ
playerHpOrgan = Organ (OrganSize 2 2) Hp

isWall :: Vector Int -> LinearIndex Terrain -> Boolean
isWall v t = (==) Wall $ fromMaybe Floor (LI.index t (fromVector v))

fromVector :: Vector Int -> Position
fromVector (V v) = Position { x: v.x, y: v.y }

enemyOnSpace :: Vector Int -> Enemy -> Boolean
enemyOnSpace v (Enemy e) = e.location == v

step :: GameState -> GameAction -> Either FailedAction GameState
step (GameState gs) a@(Move dir) =
  let p' = move dir gs.p
   in if isImpassable p' (GameState gs)
      then Right $ enemyTurn (GameState gs {p = p'})
      else Left (FailedAction dir)
step (GameState gs) a@(Attack bc eid) =
  let menemy = Map.lookup eid gs.enemies
   in case menemy of
    Nothing -> Left FailedAttack
    Just enemy -> Right (GameState gs {enemies = Map.insert eid (injureEnemy bc enemy) gs.enemies})
step (GameState gs) _ = Right $ GameState gs

injureEnemy :: BoardCoord -> Enemy -> Enemy
injureEnemy bc (Enemy e) = 
  let health@(Health {board}) = injure bc e.health
      clue = getClue bc board
   in Enemy e { health = health, clueCache = Map.insert bc clue e.clueCache}

injure :: BoardCoord -> Health -> Health
injure (BoardCoord v) (Health h) =
  let board = injureBoard v h.board
   in Health { hpCount: hpCount board, board }

isImpassable :: Vector Int -> GameState -> Boolean
isImpassable p (GameState gs) = inWorldBounds p gs.terrain && not (isWall p gs.terrain) && not (any (enemyOnSpace p) gs.enemies)

enemyTurn :: GameState -> GameState
enemyTurn (GameState gs) = GameState gs {enemies = enemyMove gs.p (GameState gs) <$> gs.enemies}

enemyMove :: Vector Int -> GameState -> Enemy -> Enemy
enemyMove p gs (Enemy e) =
  let target = e.location + (intVecToDir $ p - e.location)
   in if isImpassable p gs then Enemy e else Enemy e { location = e.location + (intVecToDir $ p - e.location) }

intVecToDir :: Vector Int -> Vector Int
intVecToDir (V {x,y}) = V {x: abs' x, y: abs' y}
  where abs' n = if n == 0 then 0 else n / abs n

inWorldBounds :: Vector Int -> LinearIndex Terrain -> Boolean
inWorldBounds (V{x,y}) (LinearIndex t) =  -- TODO: fix this
  0 <= x && x <= t.width - 1 && 0 <= y && y <= t.height - 1

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

data FailedAction =
  FailedAction Direction
  | FailedAttack

newtype Health = Health
  { hpCount :: Int
  , board :: Board
  }

derive instance newtypeHealth :: Newtype Health _

newtype Enemy = Enemy
  { location :: Vector Int
  , health :: Health
  , clueCache :: Map BoardCoord Clue
  , tag :: EnemyTag
  }

data EnemyTag = Roomba

enemyName :: EnemyTag -> String
enemyName Roomba = "murderous vacuum robot"
