module Graphics.Render where

import Extra.Prelude
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Map as Map
import Effect.Exception (error)
import Effect.Ref as Ref
import Graphics.Canvas as Canvas

import Framework.Render.Canvas as FCanvas

import GameState (GameState (..))
import UI (UIState (..), Element (..))
import Animation as A
import Framework.Render.Core (Rectangle, Image (..))

targetDimensions :: { width :: Number, height :: Number }
targetDimensions = { width: 640.0, height: 480.0}

tileSize :: Number
tileSize = 64.0

draw :: Instant -> UIState -> GameState -> FCanvas.Vars -> Effect Unit
draw t (UIState { element: Element {image, pos} }) (GameState gs) vars = do
  clear vars
  let V{x,y} = gridToScreen $ V{x:2.0,y:2.0} + (A.resolve t <$> pos)
      rect = { width: tileSize, height: tileSize, x, y }
  drawImage vars image rect

clear :: FCanvas.Vars -> Effect Unit
clear { canvas } = do
  ctx <- Canvas.getContext2D canvas
  width <- Canvas.getCanvasWidth canvas
  height <- Canvas.getCanvasHeight canvas
  Canvas.setFillStyle ctx "black"
  Canvas.fillRect ctx {x:0.0,y:0.0,width,height}

gridToScreen :: Vector Number -> Vector Number
gridToScreen p = (\x -> x * tileSize) <$> p


drawImage :: FCanvas.Vars -> String -> Rectangle -> Effect Unit
drawImage { canvas, imageData } path {x,y, width, height} = do
  ctx <- Canvas.getContext2D canvas
  imageMap <- Ref.read imageData.index
  images <- Ref.read imageData.array
  let im = do
       (Image i) <- Map.lookup path imageMap
       Array.index images i
  case im of
       Nothing -> throwError (error "Missing image!")
       Just img -> Canvas.drawImageScale ctx img x y width height
