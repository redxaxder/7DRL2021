module Data.Terrain where

import Extra.Prelude
import Data.Array as Array
import Data.Position (Position (..))
import Data.String
  ( replaceAll
  , Pattern(..)
  , Replacement(..)
  )

import Data.LinearIndex as L
import Data.LinearIndex (LinearIndex)

type Arena = LinearIndex Terrain

data Terrain = Wall | Floor | Exit | DoorClosed | DoorOpen
derive instance terrainEq :: Eq Terrain

arena :: {width :: Int, height :: Int}
arena = {width:20,height:20}

bareMap :: Arena
bareMap = L.fill arena.width arena.height Wall

charToTerrain :: Char -> Terrain
charToTerrain '.' = Floor
charToTerrain '>' = Exit
charToTerrain _ = Wall

flatten :: String -> String
flatten = replaceAll (Pattern "\n") (Replacement "")

type Block = { x:: Int, y:: Int, width:: Int, height:: Int }

set :: Position -> Terrain -> Arena -> Arena
set p terrain t = unsafeFromJust $ L.insertAt p terrain t

carveBlock :: Block -> Arena -> Arena
carveBlock b terrain = foldl (\t p -> set p Floor t) terrain positions
  where
  positions = do
     x <- Array.range b.x (b.width + b.x - 1)
     y <- Array.range b.y (b.height + b.y - 1)
     pure (V{x,y})

carveRoom :: Array Block -> Arena -> Arena
carveRoom blocks terrain = foldl (\t b -> carveBlock b t) terrain blocks

carveRooms :: Array (Array Block) -> Arena -> Arena
carveRooms rooms terrain = foldl (\t b -> carveRoom b t) terrain rooms

placeDoors :: forall e. Array { pos :: Vector Int | e } -> Arena -> Arena
placeDoors doors arena = foldl (\t {pos} -> set pos DoorClosed t) arena doors

demoTerrain :: String
demoTerrain = """
########################################
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#..................................>...#
#......................................#
#......................................#
########################################
"""
