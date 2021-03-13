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

data Terrain = Wall | Floor | Exit
derive instance terrainEq :: Eq Terrain

charToTerrain :: Char -> Terrain
charToTerrain '.' = Floor
charToTerrain '>' = Exit
charToTerrain _ = Wall

flatten :: String -> String
flatten = replaceAll (Pattern "\n") (Replacement "")

type Block = { x:: Int, y:: Int, width:: Int, height:: Int }

carve :: Position -> LinearIndex Terrain -> LinearIndex Terrain
carve p t = unsafeFromJust $ L.insertAt p Floor t

carveBlock :: Block -> LinearIndex Terrain -> LinearIndex Terrain
carveBlock b terrain =
  foldl (\t p -> carve p t) terrain positions
  where
  positions = do
     x <- Array.range b.x (b.width + b.x - 1)
     y <- Array.range b.y (b.height + b.x - 1)
     pure (V{x,y})

carveRoom :: Array Block -> LinearIndex Terrain -> LinearIndex Terrain
carveRoom blocks terrain = foldl (\t b -> carveBlock b t) terrain blocks

carveRooms :: Array (Array Block) -> LinearIndex Terrain -> LinearIndex Terrain
carveRooms rooms terrain = foldl (\t b -> carveRoom b t) terrain rooms

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
