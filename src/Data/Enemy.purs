module Data.Enemy where

import Extra.Prelude
import Data.Map as Map
import Data.Board
  ( Health(..)
  , BoardCoord
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

type EnemyId = Int
