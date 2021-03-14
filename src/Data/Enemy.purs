module Data.Enemy where

import Extra.Prelude
import Data.Map as Map
import Data.Item
  ( WeaponShape (..)
  )
import Data.Board as Board
import Data.Board
  ( Health(..)
  , BoardCoord
  , Board (..)
  , Clue
  , injure
  , getClue
  )

enemyOnSpace :: Vector Int -> Enemy -> Boolean
enemyOnSpace v (Enemy e) = e.location == v

injureEnemy :: BoardCoord -> Enemy -> Enemy
injureEnemy bc (Enemy e) =
  let health@(Health {board}) = injure bc e.health
      clue = getClue bc board
   in Enemy e { health = health, clueCache = Map.insert bc clue e.clueCache}

recalculateClues :: Enemy -> Enemy
recalculateClues (Enemy e) =
  let (Health {board: board@(Board{injuries})}) = e.health
      addClue pos clues = Map.insert pos (getClue pos board) clues
      clueCache = foldr addClue Map.empty injuries
   in Enemy e { clueCache = clueCache }

injureEnemyMulti :: Array BoardCoord -> Enemy -> Enemy
injureEnemyMulti bcs e = foldr injureEnemy e bcs

newtype Enemy = Enemy
  { location :: Vector Int
  , health :: Health
  , clueCache :: Map BoardCoord Clue
  , tag :: EnemyTag
  }

isAlive :: Enemy -> Boolean
isAlive (Enemy e) = Board.isAlive e.health

data EnemyTag =
    Roomba
  | Drone
  | Bot
  | Fridge

allEnemies :: Array EnemyTag
allEnemies =
  [ Roomba
  , Drone
  , Bot
  , Fridge
  ]

{-
murderous vacuum robot  | 2    | 0     | 1x1    | 10     |
retired battlebot       | 5    | 3     | 1x3    | 5      |
kamikaze drone          | 1    | 0     | 6x6    | 8      | self destructs
mobile refrigerator     | 2    | 8     | 2x2    | 10     |
                        -}

name :: Enemy -> String
name (Enemy {tag}) = (stats tag).name

image :: Enemy -> String
image (Enemy {tag}) = (stats tag).image

type EnemyStats =
  { name :: String
  , armor :: Int
  , hp :: Int
  , injuries :: Int
  , minDepth :: Int
  , weapon :: WeaponShape
  , image :: String
  }

s :: Int -> Int -> Int -> Int -> WeaponShape -> String -> String -> EnemyStats
s hp armor injuries minDepth weapon name image =
  {name, hp, armor, injuries, minDepth, weapon, image}

stats :: EnemyTag -> EnemyStats
--               hp arm inj lvl weap                     name              image
stats Roomba = s  2   0  10  0  Point "murderous vacuum robot"      "roomba.png"
stats Bot    = s  3   3   8  1  Point      "retired battlebot" "placeholder.png"
stats Drone  = s  1   0   5  2  Point         "kamikaze drone"       "drone.png"
stats Fridge = s  2   8  10  3  Point    "mobile refrigerator" "placeholder.png"

type EnemyId = Int
