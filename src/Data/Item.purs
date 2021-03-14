module Data.Item where

import Extra.Prelude

import Data.Board as Board
import Data.Board (BoardCoord, Health)
import Data.Int as Int
import Data.Array as Array

newtype Item = Item
  { location :: Vector Int
  , decay :: Int
  , tag :: ItemTag
  }

-- healthpickups restore n many injured squares on the player board when picked up
data ItemTag =
  HealthPickup Int
  | Weapon Weapon

type ItemId = Int

image :: Item -> String
image (Item {tag: HealthPickup _}) = "heal.png"
image (Item {tag: Weapon  _}) = "weapon.png"

strong :: Int
strong = 5

medium :: Int
medium = 3

weak :: Int
weak = 1

itemOnSpace :: Vector Int -> Item -> Boolean
itemOnSpace v (Item i) = v == i.location


type Weapon =
  { name :: String
  , range :: Int
  , shape :: WeaponShape
  }

w :: String -> Int -> WeaponShape -> Weapon
w name range shape = {name,range,shape}

pistol = w "pistol" 3 Point

allWeapons :: Array Weapon
allWeapons =
  [ pistol
  ]

data WeaponShape =
    Point
  | Box Int Int

affectedSpaces :: Vector Int -> WeaponShape -> Array BoardCoord
affectedSpaces pos Point = [pos]
affectedSpaces pos@(V p) (Box w h) =
  let V b = pos - V{x: Int.quot w 2, y: Int.quot h 2}
   in do
        x <- Array.range b.x (w + b.x - 1)
        y <- Array.range b.y (w + b.y - 1)
        pure (V{x,y})

weaponAttack :: Vector Int -> WeaponShape -> Health -> Health
weaponAttack pos weapon health =
  Board.injureMulti (affectedSpaces pos weapon) health

