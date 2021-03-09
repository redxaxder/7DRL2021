module Graphics.Render where

import Extra.Prelude
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Map as Map
import Effect.Exception (error)
import Effect.Ref as Ref
import Effect.Ref (Ref)
import Graphics.Canvas as Canvas
import Data.Set as Set
import Data.Board
  ( Board(..)
  , BoardCoord (..)
  , Clue (..)
  )
import Data.Terrain
  ( Terrain(..)
  )

import Framework.Render.Canvas as FCanvas

import GameState
  ( GameState (..)
  , Health (..)
  , Enemy (..)
  , EnemyTag (..)
  , enemyName
  )
import UI
  ( UIState (..)
  , tileSize
  , centerPaneRect
  , playerBoardRect
  --, leftPaneRect
  , rightPaneRect
  , targetBoardContainerRect
  , targetBoardRect
  )
--import Animation as A
import Framework.Render.Core (Rectangle, Image (..))
import Data.Position (Position (..))
import Data.Variant as V


import Effect.Console as C

{-



idea:
  each gamestate is timestamped
  draw function remembers the most recent timestamp it drew
  the static parts of a scene are drawn to a secondary buffer
  in an animated scene, the animation is cleaned up by copying from the secondary buffer
  parts of the draw are skipped



-}

newtype RendererState = RendererState
  { cvars :: FCanvas.Vars
  , gameStateId :: Ref (Maybe Instant)
  , uiStateId :: Ref (Maybe Instant)
  , prevDraw :: Ref (Maybe Instant)
  }

newRendererState :: FCanvas.Vars -> Effect RendererState
newRendererState cvars = do
  gameStateId <- Ref.new Nothing
  uiStateId <- Ref.new Nothing
  prevDraw <- Ref.new Nothing
  pure $ RendererState { cvars, gameStateId, uiStateId, prevDraw }

draw :: Instant -> UIState -> GameState -> RendererState -> Effect Unit
draw t uis@(UIState {timestamp, gsTimestamp}) gs rs@(RendererState r) = do
  prevGS <- Ref.read r.gameStateId
  prevUI <- Ref.read r.uiStateId
  let uiDirty = maybe true ((>) timestamp) prevUI
      gsDirty = maybe true ((>) gsTimestamp) prevGS
  when uiDirty $ do
    C.log "draw ui"
    drawPlayerBoard t uis gs rs
    drawRightPane t uis gs rs
    Ref.write (Just timestamp) r.uiStateId
  when gsDirty $ do
    C.log "draw gs"
    drawCenterPane t uis gs rs
    Ref.write (Just gsTimestamp) r.gameStateId

drawPlayerBoard :: Instant -> UIState -> GameState -> RendererState -> Effect Unit
drawPlayerBoard t uis (GameState {playerHealth}) vars = do
  let playerHp = (un Health playerHealth).hpCount
      playerBoard = (un Health playerHealth).board
  clear vars playerBoardRect
  drawText vars (show playerHp) { x:10.0, y:0.0 }
  drawImage vars "heart.png" { x:0.0, y: 0.0, width: tileSize, height: tileSize }
  drawBoardBase vars playerBoard playerBoardRect
  -- TODO draw player organs

drawCenterPane :: Instant -> UIState -> GameState -> RendererState -> Effect Unit
drawCenterPane t (UIState uis) (GameState gs) vars = do
  clear vars centerPaneRect
  forWithIndex_ gs.terrain \pos terrain ->
     let Position p = pos
         x = centerPaneRect.x + toNumber p.x * tileSize
         y = toNumber p.y * tileSize
         image = case terrain of
                  Wall -> "wall.png"
                  Floor -> "ground.png"
                  Exit -> "placeholder.png"
     in drawImage vars image { x, y, width: tileSize, height: tileSize }
  let (V playerPos) = gs.p
      px = centerPaneRect.x + toNumber playerPos.x * tileSize
      py = toNumber playerPos.y * tileSize
  drawImage vars "player.png" { x: px, y: py, width: tileSize, height: tileSize }
  for_ gs.enemies \(Enemy e) ->
    let V p = e.location
        x = centerPaneRect.x + toNumber p.x * tileSize
        y = toNumber p.y * tileSize
        image = case e.tag of
                      Roomba -> "roomba.png"
    in drawImage vars image {x,y, width: tileSize, height: tileSize }

drawRightPane :: Instant -> UIState -> GameState -> RendererState -> Effect Unit
drawRightPane t (UIState{rightPaneTarget}) (GameState gs) vars = do
  clear vars rightPaneRect
  V.match
    { none: \_ -> pure unit
    , terrain: \_ -> pure unit
    , enemy: \eid ->
       case Map.lookup eid gs.enemies of
            Nothing -> pure unit
            Just (Enemy e) -> do
              let name = enemyName e.tag
                  Health h = e.health
              wrapText vars name {x: rightPaneRect.x, y: 0.0} rightPaneRect.width
              drawImage vars "heart.png"
                { x: targetBoardContainerRect.x --TODO: move these outside
                , y: targetBoardContainerRect.y -- for column clues
                , width: tileSize, height: tileSize }
              drawText vars (show h.hpCount) -- TODO: move for column clues
                { x: targetBoardContainerRect.x + 10.0
                , y: targetBoardContainerRect.y }
              drawBoardBase vars h.board targetBoardRect
              drawEnemyClues vars e.clueCache
    } rightPaneTarget

clear :: RendererState -> Rectangle -> Effect Unit
clear (RendererState { cvars: c }) rect = do
  ctx <- Canvas.getContext2D c.canvas
  Canvas.setFillStyle ctx "black"
  Canvas.fillRect ctx rect

drawBoardBase :: RendererState -> Board -> Rectangle -> Effect Unit
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

drawEnemyClues :: RendererState -> Map BoardCoord Clue -> Effect Unit
drawEnemyClues rs m =
  forWithIndex_ m \(BoardCoord (V{x,y})) clue ->
    let cluePosition =
          { x: toNumber x * tileSize + targetBoardRect.x + 2.0
          , y: toNumber y * tileSize + targetBoardRect.y
          }
     in case clue of
        HpClue i -> drawColorText rs (show i) (Color "red") cluePosition
        EmptyClue -> drawColorText rs "0" (Color "#777") cluePosition
        _ -> drawColorText rs "?" (Color "#333") cluePosition



drawText :: RendererState -> String -> { x :: Number, y:: Number } -> Effect Unit
drawText vars string loc =
  drawColorText vars string (Color "white") loc

newtype Color = Color String
foreign import setTextBaselineHanging :: Canvas.Context2D -> Effect Unit

drawColorText :: RendererState -> String -> Color -> { x :: Number, y:: Number } -> Effect Unit
drawColorText (RendererState {cvars}) string (Color c)loc = do
  ctx <- Canvas.getContext2D cvars.canvas
  Canvas.setFont ctx "10px CapitalHillMono"
  setTextBaselineHanging ctx
  Canvas.setFillStyle ctx c
  Canvas.fillText ctx string loc.x (loc.y + 2.0)

wrapText
  :: RendererState
  -> String
  -> { x :: Number, y:: Number }
  -> Number
  -> Effect Unit
wrapText vars string loc _maxWidth =
  --let splits = String.split (Pattern " ") string
  drawText vars string loc

{-
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
-}

drawImage :: RendererState -> String -> Rectangle -> Effect Unit
drawImage (RendererState {cvars: { canvas, imageData }}) path {x,y, width, height} = do
  ctx <- Canvas.getContext2D canvas
  imageMap <- Ref.read imageData.index
  images <- Ref.read imageData.array
  let im = do
       (Image i) <- Map.lookup path imageMap
       Array.index images i
  case im of
       Nothing -> throwError (error "Missing image!")
       Just img -> Canvas.drawImageScale ctx img x y width height

drawImageFull :: RendererState -> String -> Rectangle -> Rectangle -> Effect Unit
drawImageFull (RendererState {cvars: { canvas, imageData }}) path s d = do
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
