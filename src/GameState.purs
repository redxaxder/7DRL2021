module GameState where

import Extra.Prelude
import Framework.Direction (Direction, move)
import Data.Set as Set
import Data.Map as Map
import Data.LinearIndex (LinearIndex (..))
import Data.LinearIndex as LI
import Data.FoldableWithIndex (findWithIndex)
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
   , enemies: exampleEnemies
   , terrain: fromMaybe (LI.fill 40 40 Floor) (freshTerrainFromString demoTerrain)
   , rng: random
   }

exampleEnemies :: Map EnemyId Enemy
exampleEnemies =
  Map.fromFoldableWithIndex
  [ injureEnemyMulti exampleInjuries $ Enemy
    { location: V{x:5, y:5}
    , health: mkHealth exampleRoombaBoard
    , clueCache: Map.empty
    , tag: Roomba
    }
  ]

mkHealth :: Board -> Health
mkHealth board = Health
  { hpCount: hpCount board
  , board
  }

exampleInjuries :: Array BoardCoord
exampleInjuries = BoardCoord <$> [ vec 1 1, vec 2 3, vec 4 4 ]

exampleRoombaBoard :: Board
exampleRoombaBoard = Board
  { organs:
     [ Tuple hpOrgan1 (BoardCoord (vec 2 2))
     , Tuple hpOrgan1 (BoardCoord (vec 2 5))
     , Tuple hpOrgan1 (BoardCoord (vec 4 0))
     ]
  , injuries: Set.empty
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
  { organs:
      [ Tuple playerHpOrgan (BoardCoord (vec 1 1))
      , Tuple playerHpOrgan (BoardCoord (vec 3 1))
      , Tuple playerHpOrgan (BoardCoord (vec 2 3))
      ]
  , injuries: Set.empty
  }

hpOrgan1 :: Organ
hpOrgan1 = Organ (OrganSize 1 1) Hp

playerHpOrgan :: Organ
playerHpOrgan = Organ (OrganSize 2 2) PlayerHeartLarge

isWall :: Vector Int -> LinearIndex Terrain -> Boolean
isWall v t = (==) Wall $ fromMaybe Floor (LI.index t (fromVector v))

fromVector :: Vector Int -> Position
fromVector (V v) = Position { x: v.x, y: v.y }

enemyOnSpace :: Vector Int -> Enemy -> Boolean
enemyOnSpace v (Enemy e) = e.location == v

step :: GameState -> GameAction -> Either FailedAction GameState
step (GameState gs) a@(Move dir) =
  let p' = move dir gs.p
   in if isPassable p' (GameState gs)
      then Right $ enemyTurn (GameState gs {p = p'})
      else Left (FailedAction dir)
step (GameState gs) a@(Attack bc eid) =
  let menemy = Map.lookup eid gs.enemies
   in case menemy of
    Nothing -> Left FailedAttack
    Just enemy -> Right $ enemyTurn (GameState gs {enemies = Map.insert eid (injureEnemy bc enemy) gs.enemies})
step (GameState gs) _ = Right $ GameState gs

injureEnemy :: BoardCoord -> Enemy -> Enemy
injureEnemy bc (Enemy e) =
  let health@(Health {board}) = injure bc e.health
      clue = getClue bc board
   in Enemy e { health = health, clueCache = Map.insert bc clue e.clueCache}

injureEnemyMulti :: Array BoardCoord -> Enemy -> Enemy
injureEnemyMulti bcs e = foldr injureEnemy e bcs

injure :: BoardCoord -> Health -> Health
injure (BoardCoord v) (Health h) =
  let board = injureBoard v h.board
   in Health { hpCount: hpCount board, board }

isPassable :: Vector Int -> GameState -> Boolean
isPassable t (GameState gs) =
  inWorldBounds t gs.terrain
  && not (isWall t gs.terrain)
  && not (any (enemyOnSpace t) gs.enemies)
  && t /= gs.p

enemyTurn :: GameState -> GameState
enemyTurn (GameState gs) = GameState gs {enemies = enemyMove (GameState gs) <$> gs.enemies}

enemyMove :: GameState -> Enemy -> Enemy
enemyMove (GameState gs) (Enemy e) =
  let target = e.location + (intVecToDir $ gs.p - e.location)
   in if isPassable target (GameState gs) then Enemy e { location = e.location + (intVecToDir $ gs.p - e.location) } else Enemy e

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


data Target =
  TargetEnemy EnemyId
  | TargetTerrain Terrain

getTargetAtPosition :: Vector Int -> GameState -> Target
getTargetAtPosition p gs = case getEnemyAtPosition p gs of
  Just eid -> TargetEnemy eid
  Nothing -> TargetTerrain $ getTerrainAtPosition p gs

getEnemyAtPosition :: Vector Int -> GameState -> Maybe EnemyId
getEnemyAtPosition p (GameState gs) = do
  -- loop through all enemies and check if one of them is at position p
  -- if the slowness ends up mattering, we should maintain an index instead
  {index} <- findWithIndex (\eid (Enemy e) -> e.location == p) gs.enemies
  pure index

getTerrainAtPosition :: Vector Int -> GameState -> Terrain
getTerrainAtPosition p (GameState {terrain}) = fromMaybe Floor $ LI.index terrain (fromVector p)

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
