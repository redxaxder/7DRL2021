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
  , BoardCoord (..)
  , Health (..)
  , Terrain (..)
  , Enemy (..)
  , EnemyTag (..)
  , enemyName
  )
import UI
  ( UIState (..)
  , centerPaneTiles
  , leftPaneBorder
  , leftPaneTiles
  , rightPaneBorder
  , rightPaneTiles
  , tileSize
  ) -- , Element (..))
--import Animation as A
import Framework.Render.Core (Rectangle, Image (..))
import Data.Position (Position (..))
import Data.Variant as V


--todo: remember time of previous draw call and use this for extra dirty flags
--      on animated stuff

draw :: Instant -> UIState -> GameState -> FCanvas.Vars -> Effect Unit
draw t uis@(UIState {dirty}) gs vars = do
  clear vars dirty
  drawLeftPane t uis gs vars
  drawCenterPane t uis gs vars
  drawRightPane t uis gs vars
  --drawPaneBorders t uis gs vars

drawLeftPane :: Instant -> UIState -> GameState -> FCanvas.Vars -> Effect Unit
drawLeftPane t (UIState{dirty}) (GameState {playerHealth}) vars = do
  let playerHp = (un Health playerHealth).hpCount
      playerBoard = (un Health playerHealth).board
  drawText vars (show playerHp) { x:10.0, y:0.0 }
  drawImage vars dirty "heart.png" { x:0.0, y: 0.0, width: tileSize, height: tileSize }
  drawBoardBase vars dirty playerBoard { x: 0.0, y: 11.0 }
  -- TODO draw player organs

centerPaneStart :: Number
centerPaneStart = leftPaneTiles * tileSize + leftPaneBorder

drawCenterPane :: Instant -> UIState -> GameState -> FCanvas.Vars -> Effect Unit
drawCenterPane t (UIState uis) (GameState gs) vars = do
  forWithIndex_ gs.terrain \pos terrain ->
     let Position p = pos
         x = centerPaneStart + toNumber p.x * tileSize
         y = toNumber p.y * tileSize
         image = case terrain of
                  Wall -> "wall.png"
                  Floor -> "ground.png"
                  Exit -> "placeholder.png"
     in drawImage vars uis.dirty image { x, y, width: tileSize, height: tileSize }
  let (V playerPos) = gs.p
      px = centerPaneStart + toNumber playerPos.x * tileSize
      py = toNumber playerPos.y * tileSize
  drawImage vars uis.dirty "player.png" { x: px, y: py, width: tileSize, height: tileSize }
  for_ gs.enemies \(Enemy e) ->
    let V p = e.location
        x = centerPaneStart + toNumber p.x * tileSize
        y = toNumber p.y * tileSize
        image = case e.tag of
                      Roomba -> "roomba.png"
    in drawImage vars uis.dirty image {x,y, width: tileSize, height: tileSize }

rightPaneStart :: Number
rightPaneStart = centerPaneStart + centerPaneTiles * tileSize + rightPaneBorder

rightPaneSize :: Number
rightPaneSize = rightPaneTiles * tileSize

drawRightPane :: Instant -> UIState -> GameState -> FCanvas.Vars -> Effect Unit
drawRightPane t (UIState{rightPaneTarget, dirty}) (GameState gs) vars =
  V.match
  { none: \_ -> pure unit
  , floor: \_ -> pure unit
  , wall: \_ -> pure unit
  , enemy: \eid ->
     case Map.lookup eid gs.enemies of
          Nothing -> pure unit
          Just (Enemy e) -> do
            let name = enemyName e.tag
                Health h = e.health
            wrapText vars name {x: rightPaneStart, y: 0.0} rightPaneSize
            drawImage vars dirty "heart.png"
              { x: rightPaneStart, y: 30.0, width: tileSize, height: tileSize }
            drawText vars (show h.hpCount) { x: rightPaneStart + 10.0, y: 30.0 }
            drawBoardBase vars dirty h.board { x: rightPaneStart, y: 50.0 }
  } rightPaneTarget

clear :: FCanvas.Vars -> Array Rectangle -> Effect Unit
clear { canvas } rects = do
  ctx <- Canvas.getContext2D canvas
  width <- Canvas.getCanvasWidth canvas
  height <- Canvas.getCanvasHeight canvas
  Canvas.setFillStyle ctx "black"
  for_ rects $ Canvas.fillRect ctx

gridToScreen :: Vector Number -> Vector Number
gridToScreen p = (\x -> x * tileSize) <$> p

drawBoardBase :: FCanvas.Vars -> Array Rectangle -> Board -> {x :: Number, y :: Number} -> Effect Unit
drawBoardBase vars dirty (Board {injuries}) {x,y} =
  for_ (Array.range 0 5) \px ->
    for_ (Array.range 0 5) \py ->
      let buttonImage = if Set.member (BoardCoord (V{x: px, y:py})) injuries
            then "ButtonPushed.png"
            else "ButtonUnpushed.png"
       in drawImage vars dirty buttonImage
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

wrapText
  :: FCanvas.Vars
  -> String
  -> { x :: Number, y:: Number }
  -> Number
  -> Effect Unit
wrapText vars string loc _maxWidth =
  --let splits = String.split (Pattern " ") string
  drawText vars string loc

dirtyCheck :: Array Rectangle -> Rectangle -> Boolean
dirtyCheck dirty target = any (intersectRect target) dirty
  where
    intersectInterval l1 r1 l2 r2 = r1 > l2 && r2 > l1
    intersectRect r1 r2 =
      intersectInterval
        r1.x (r1.x + r1.width)
        r2.x (r2.x + r2.width)
      &&
      intersectInterval
        r1.y (r1.y + r1.height)
        r2.y (r2.y + r2.height)


drawImage :: FCanvas.Vars -> Array Rectangle -> String -> Rectangle -> Effect Unit
drawImage { canvas, imageData } dirty path {x,y, width, height} = do
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
