module Data.Item where

import Extra.Prelude

newtype Item = Item
  { location :: Vector Int
  , decay :: Int
  , tag :: ItemTag
  }

-- healthpickups restore n many injured squares on the player board when picked up
data ItemTag = HealthPickup Int

type ItemId = Int

strong :: Int
strong = 5

medium :: Int
medium = 3

weak :: Int
weak = 1

itemOnSpace :: Vector Int -> Item -> Boolean
itemOnSpace v (Item i) = v == i.location
