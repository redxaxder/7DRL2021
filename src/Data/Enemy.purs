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

data EnemyTag = Roomba

enemyName :: EnemyTag -> String
enemyName Roomba = "murderous vacuum robot"

type EnemyStats =
  { armor :: Int
  , hp :: Int
  , injuries :: Int
  }

s :: Int -> Int -> Int -> EnemyStats
s hp armor injuries = {hp, armor, injuries}
stats :: EnemyTag -> EnemyStats
stats Roomba = s 2 0 10

type EnemyId = Int
