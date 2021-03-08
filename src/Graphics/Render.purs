module Graphics.Render where

import Extra.Prelude
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Map as Map
import Effect.Exception (error)
import Effect.Ref as Ref
import Graphics.Canvas as Canvas
import Data.Set as Set

import Framework.Render.Canvas as FCanvas

import GameState
  ( GameState (..)
  , Board (..)
  , freshPlayerBoard
  , BoardCoord (..)
  , Health (..)
  , Terrain (..)
  , Enemy (..)
  , EnemyTag (..)
  )
import UI (UIState (..)) -- , Element (..))
--import Animation as A
import Framework.Render.Core (Rectangle, Image (..))
import Data.Char (toCharCode)
import Data.Position (Position (..))
import Data.Variant as V


targetDimensions :: { width :: Number, height :: Number }

targetDimensions = { width: totalWidth, height: totalHeight}

totalWidth :: Number
totalWidth = tileSize * (leftPaneTiles + centerPaneTiles + rightPaneTiles) + 2.0

totalHeight :: Number
totalHeight = tileSize * verticalTiles

leftPaneTiles :: Number
leftPaneTiles = 6.0

centerPaneTiles :: Number
centerPaneTiles = 40.0

rightPaneTiles :: Number
rightPaneTiles = 10.0

verticalTiles :: Number
verticalTiles = 40.0

tileSize :: Number
tileSize = 10.0

draw :: Instant -> UIState -> GameState -> FCanvas.Vars -> Effect Unit
draw t uis gs vars = do
  clear vars
  drawLeftPane t uis gs vars
  drawCenterPane t uis gs vars
  --drawRightPane t uis gs vars
  --drawPaneBorders t uis gs vars

drawLeftPane :: Instant -> UIState -> GameState -> FCanvas.Vars -> Effect Unit
drawLeftPane t uis (GameState {playerHealth}) vars = do
  let playerHp = (un Health playerHealth).hpCount
      playerBoard = (un Health playerHealth).board
  drawText vars (show playerHp) { x:10.0, y:0.0 }
  drawImage vars "heart.png" { x:0.0, y: 0.0, width: tileSize, height: tileSize }
  drawBoardBase vars playerBoard { x: 0.0, y: 11.0 }
  -- TODO draw player organs

drawCenterPane :: Instant -> UIState -> GameState -> FCanvas.Vars -> Effect Unit
drawCenterPane t uis (GameState gs) vars = do
  let x0 = 6.0 * tileSize + 1.0
      y0 = 0.0
  forWithIndex_ gs.terrain \pos terrain ->
     let Position p = pos
         x = x0 + toNumber p.x * tileSize
         y = y0 + toNumber p.y * tileSize
         image = case terrain of
                  Wall -> "wall.png"
                  Floor -> "ground.png"
                  Exit -> "placeholder.png"
     in drawImage vars image { x, y, width: tileSize, height: tileSize }
  let (V playerPos) = gs.p
      px = x0 + toNumber playerPos.x * tileSize
      py = y0 + toNumber playerPos.y * tileSize
  drawImage vars "player.png" { x: px, y: py, width: tileSize, height: tileSize }
  for_ gs.enemies \(Enemy e) ->
    let V p = e.location
        x = x0 + toNumber p.x * tileSize
        y = y0 + toNumber p.y * tileSize
        image = case e.tag of
                      Roomba -> "roomba.png"
    in drawImage vars image {x,y, width: tileSize, height: tileSize }

drawRightPane :: Instant -> UIState -> GameState -> FCanvas.Vars -> Effect Unit
drawRightPane t (UIState{rightPaneTarget}) (GameState gs) vars =
  V.match
  { none: \_ -> pure unit
  , floor: \_ -> pure unit
  , wall: \_ -> pure unit
  , enemy: \eid ->
     case Map.lookup eid gs.enemies of
          Nothing -> pure unit
          Just e -> pure unit
  } rightPaneTarget

clear :: FCanvas.Vars -> Effect Unit
clear { canvas } = do
  ctx <- Canvas.getContext2D canvas
  width <- Canvas.getCanvasWidth canvas
  height <- Canvas.getCanvasHeight canvas
  Canvas.setFillStyle ctx "black"
  Canvas.fillRect ctx {x:0.0,y:0.0,width,height}

gridToScreen :: Vector Number -> Vector Number
gridToScreen p = (\x -> x * tileSize) <$> p

textWidth :: Number
textWidth = 10.0

textHeight :: Number
textHeight = 10.0

charPosition :: Char -> {x :: Number, y :: Number }
charPosition c =
  let i = toCharCode c
   in { x: toNumber (i `mod` 16) * textHeight
      , y: toNumber (i `div` 16) * textWidth
      }

font :: String
font = "rexpaint_cp437_10x10.png"


exampleBoard :: Board
exampleBoard = freshPlayerBoard

drawBoardBase :: FCanvas.Vars -> Board -> {x :: Number, y :: Number} -> Effect Unit
drawBoardBase vars (Board {injuries}) {x,y} =
  for_ (Array.range 0 5) \px ->
    for_ (Array.range 0 5) \py ->
      let buttonImage = if Set.member (BoardCoord (V{x: px, y:py})) injuries
            then "ButtonPushed.png"
            else "ButtonUnpushed.png"
       in drawImage vars buttonImage
          { x: x + toNumber px * tileSize
          , y: y + toNumber py * tileSize
          , width: tileSize
          , height: tileSize
          }

drawText :: FCanvas.Vars -> String -> { x :: Number, y:: Number } -> Effect Unit
drawText vars string loc =
  drawColorText vars string (Color "white") loc

newtype Color = Color String
foreign import setTextBaselineHanging :: Canvas.Context2D -> Effect Unit

drawColorText :: FCanvas.Vars -> String -> Color -> { x :: Number, y:: Number } -> Effect Unit
drawColorText vars string (Color c)loc = do
  ctx <- Canvas.getContext2D vars.canvas
  Canvas.setFont ctx "10px CapitalHillMono"
  setTextBaselineHanging ctx
  Canvas.setFillStyle ctx c
  Canvas.fillText ctx string loc.x (loc.y + 2.0)

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

drawImageFull :: FCanvas.Vars -> String -> Rectangle -> Rectangle -> Effect Unit
drawImageFull { canvas, imageData } path s d = do
  ctx <- Canvas.getContext2D canvas
  imageMap <- Ref.read imageData.index
  images <- Ref.read imageData.array
  let im = do
       (Image i) <- Map.lookup path imageMap
       Array.index images i
  case im of
       Nothing -> throwError (error "Missing image!")
       Just img -> Canvas.drawImageFull ctx img
         s.x s.y s.width s.height
         d.x d.y d.width d.height
