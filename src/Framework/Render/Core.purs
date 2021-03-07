module Framework.Render.Core where

import Prelude

import Data.Maybe (Maybe)

import Control.Monad.Free (Free)
import Control.Monad.Free as Free

import Framework.Render.Color (Color, black)

type Rectangle = { height :: Number, width :: Number, x :: Number, y :: Number }

newtype Image = Image Int

data RenderF r =
    LoadImage String (Maybe Image -> r)
    | GetDimensions ({ width :: Number, height :: Number } -> r)
    | FillRect Color Rectangle r
    | DrawImage Image Rectangle r
    | DrawImageFull Image Rectangle Rectangle r
    | DrawLine LineConfig r

type Point = { x :: Number, y :: Number }

type LineConfig =
  { color :: Color
  , start :: Point
  , end :: Point
  , width :: Number
  }

derive instance renderF :: Functor RenderF

type RenderM a = Free RenderF a

type RenderConfig =
  { width :: Number
  , height :: Number
  }

getDimensions :: RenderM { width :: Number, height :: Number }
getDimensions = Free.liftF $ GetDimensions identity

fillRect :: Color -> Rectangle -> RenderM Unit
fillRect color rect = Free.wrap $ FillRect color rect (pure unit)

loadImage :: String -> RenderM (Maybe Image)
loadImage path = Free.liftF $ LoadImage path identity

drawImage :: Image -> Rectangle -> RenderM Unit
drawImage image rect = Free.wrap $ DrawImage image rect (pure unit)

drawImageFull :: Image -> Rectangle -> Rectangle -> RenderM Unit
drawImageFull image srcRect destRect = Free.wrap $
  DrawImageFull image srcRect destRect (pure unit)

drawLine :: LineConfig -> RenderM Unit
drawLine c = Free.wrap $ DrawLine c (pure unit)

clear :: RenderM Unit
clear = do
  { width, height } <- getDimensions
  fillRect black { x: 0.0, y: 0.0, width, height }

clearRegion :: {x :: Number, y :: Number, width :: Number, height :: Number} -> RenderM Unit
clearRegion = fillRect black
