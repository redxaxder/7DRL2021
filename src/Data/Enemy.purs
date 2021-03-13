module Data.Enemy where

import Extra.Prelude
import Data.Map as Map
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

data EnemyTag =
    Roomba
  | Drone

allEnemies :: Array EnemyTag
allEnemies =
  [ Roomba
  , Drone
  ]

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
  , attack :: Attack
  , image :: String
  }

data Attack = Box Int Int

s :: Int -> Int -> Int -> Int -> Attack -> String -> String -> EnemyStats
s hp armor injuries minDepth attack name image =
  {name, hp, armor, injuries, minDepth, attack, image}

stats :: EnemyTag -> EnemyStats
stats Roomba = s  2  0  10  0  (Box 1 1) "murderous vacuum robot"  "roomba.png"
stats Drone =  s  1  0  15  2  (Box 6 6)         "kamikaze drone"   "drone.png"

type EnemyId = Int
