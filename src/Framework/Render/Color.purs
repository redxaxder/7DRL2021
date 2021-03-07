module Framework.Render.Color where

import Data.Newtype

newtype Color = Color String

derive instance newtypeColor :: Newtype Color _

black :: Color
black = Color "#000000"

white :: Color
white = Color "#FFFFFF"

gray :: Color
gray = Color "#C0C0C0"

blue :: Color
blue = Color "#0000FF"

red :: Color
red = Color "#FF0000"
