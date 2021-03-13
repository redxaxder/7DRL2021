module Graphics.Render where

import Extra.Prelude

import Animation as A
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Map as Map
import Data.DateTime.Instant (unInstant)
import Data.Time.Duration (Milliseconds (..))
import Data.Int as Int
import Effect.Exception (error)
import Effect.Ref as Ref
import Math (round, sin)
import Effect.Ref (Ref)
import Graphics.Canvas as Canvas
import Data.Set as Set
import Data.LinearIndex as LI
import Data.Board
  ( Board(..)
  , Clue (..)
  , Health (..)
  , Organ (..)
  , OrganType (..)
  , OrganSize (..)
  , getOrganAtPosition
  , getOrgans
  , organArray
  )
import Data.Terrain
  ( Terrain(..)
  , blockPositions
  )

import Framework.Render.Canvas as FCanvas

import GameState
  ( GameState (..)
  , isSurgeryLevel
  )
import UI
  ( UIState (..)
  , RightPane (..)
  , OrganDrag
  , tileSize
  , screen
  , centerPaneRect
  , leftPaneRect
  , playerBoardRect
  --, leftPaneRect
  , rightPaneRect
  , targetBoardContainerRect
  , targetBoardRect
  )
import Framework.Render.Core (Rectangle, Image (..))
import Data.Position (Position (..))

import Data.Enemy
  ( Enemy(..)
  , EnemyTag (..)
  , EnemyId
  , enemyName
  )

import Data.Item
  ( Item(..)
  , ItemId
  , ItemTag(..)
  )

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
  , toRestore :: Ref (Array Rectangle)
  , screenCache :: Ref Canvas.ImageData
  , debug :: Ref Debug
  }

type Debug = Int
initDebug :: Debug
initDebug = 0

resetDebug :: RendererState -> Effect Unit
resetDebug (RendererState r) = Ref.write initDebug r.debug

debugInfo :: RendererState -> Effect Unit
debugInfo (RendererState rs) = do
  -- y <- Ref.read rs.debug
  -- let _ = spy "d" y
  pure unit

note :: RendererState -> Effect Unit
note (RendererState rs) = Ref.modify_ (\x -> x + 1) rs.debug

--------------------------------------------------------------------------------
-- Screen caching and restoring ------------------------------------------------
--------------------------------------------------------------------------------
screenImageData :: FCanvas.Vars -> Effect Canvas.ImageData
screenImageData {canvas} = do
  ctx <- Canvas.getContext2D canvas
  Canvas.getImageData ctx screen.x screen.y screen.width screen.height

cacheScreen :: RendererState -> Effect Unit
cacheScreen (RendererState {cvars, screenCache}) = do
  imageData <- screenImageData cvars
  Ref.write imageData screenCache

restore :: RendererState -> Rectangle -> Effect Unit
restore (RendererState rs) rect = do
  ctx <- Canvas.getContext2D rs.cvars.canvas
  id <- Ref.read rs.screenCache
  {-
    the documentation for the function `putImageData` is all dumb and misleading
    the emperically determined behavior in chrome and firefox is as follows:
    it takes 6 arguments: ctx, imageData, dx, dy, dirtyX, dirtyY, dirtyW, dirtyH
    a rectangle is copied from the imageData to the ctx
    both rectangles have the same width and height, given by dirtyW and dirtyH
    the position of the source rectagle in imageData is given by:
       dirtyX, dirtyY
    the position of the target rectangle in the ctx is given by:
       dx + dirtyX, dy + dirtyY
    -}
  let dx = 0.0
      dy = 0.0
      dirtyX = rect.x
      dirtyY = rect.y
  Canvas.putImageDataFull ctx id
    dx dy
    dirtyX dirtyY
    rect.width rect.height

restoreAll :: RendererState -> Effect Unit
restoreAll rs@(RendererState {toRestore}) = do
  rects <- Ref.read toRestore
  for_ rects (restore rs)
  Ref.write [] toRestore

queueRestore :: RendererState -> Rectangle -> Effect Unit
queueRestore (RendererState {toRestore}) rect =
  Ref.modify_ (Array.cons rect) toRestore

drawImageTemp :: RendererState -> String -> Rectangle -> Effect Unit
drawImageTemp rs img rect = do
  drawImage rs img rect
  queueRestore rs rect

fillRectTemp :: RendererState -> Rectangle -> Color -> Effect Unit
fillRectTemp rs rect color = do
  fillRect rs rect color
  queueRestore rs rect{width = rect.width + 1.0, height = rect.height + 1.0 }

--------------------------------------------------------------------------------

newRendererState :: FCanvas.Vars -> Effect RendererState
newRendererState cvars = do
  gameStateId <- Ref.new Nothing
  uiStateId <- Ref.new Nothing
  toRestore <- Ref.new []
  sid <- screenImageData cvars
  screenCache <- Ref.new sid
  debug <- Ref.new initDebug
  pure $ RendererState
    { cvars
    , gameStateId
    , uiStateId
    , toRestore
    , screenCache
    , debug
    }

draw :: Instant -> UIState -> GameState -> RendererState -> Effect Unit
draw t uis@(UIState {timestamp, gsTimestamp}) gs@(GameState g) rs@(RendererState r) = do
  prevUI <- Ref.read r.uiStateId
  resetDebug rs
  restoreAll rs
  let uiDirty = maybe true ((>) timestamp) prevUI
  drawCenterPane t uis gs rs
  when uiDirty $ do
    drawPlayerBoard t uis gs rs
    drawRightPane t uis gs rs
    Ref.write (Just timestamp) r.uiStateId
  drawHighlights t uis rs
  drawDraggedOrgan uis gs rs
  debugInfo rs


drawPlayerBoard :: Instant -> UIState -> GameState -> RendererState -> Effect Unit
drawPlayerBoard t uis (GameState {playerHealth}) rs = do
  let playerHp = (un Health playerHealth).hpCount
      playerBoard = (un Health playerHealth).board
      playerOrgans = getOrgans playerBoard
      anchor = let {x,y} = playerBoardRect in V{x,y}
  clear rs leftPaneRect
  drawText rs (show playerHp) (V{ x:tileSize, y:0.0 })
  drawImage rs "heart.png" { x:0.0, y: 0.0, width: tileSize, height: tileSize }
  drawBoardBase rs playerBoard playerBoardRect
  for_ playerOrgans.intact \(Tuple organ bc) ->
    drawOrgan true rs organ (fromGrid bc + anchor)
  for_ playerOrgans.injured \(Tuple organ bc) ->
    drawOrgan false rs organ (fromGrid bc + anchor)

drawCenterPane :: Instant -> UIState -> GameState -> RendererState -> Effect Unit
drawCenterPane
  t
  uis@(UIState {gsTimestamp})
  gs@(GameState {availableOrgans})
  rs@(RendererState r) = do
  prevGS <- Ref.read r.gameStateId
  let gsDirty = maybe true ((>) $ gsTimestamp) (prevGS)
  case isSurgeryLevel gs of
       false -> do
         when gsDirty $ do
           centerPaneMap t uis gs rs
           cacheScreen rs
           Ref.write (Just gsTimestamp) r.gameStateId
         drawCenterPaneAnimations t uis gs rs
       true -> do
         when gsDirty do
           clear rs centerPaneRect
           for_ (organArray availableOrgans) \(Tuple organ position) ->
             drawOrgan true rs organ (rectPos centerPaneRect + fromGrid position)
           let x = centerPaneRect.x
               y = centerPaneRect.y + centerPaneRect.height - tileSize * 4.0
           drawText rs "Drag parts into your health area to install them"
             (V{x,y})
           drawText rs "Click on parts in your health to destroy them"
             (V{x,y: y + tileSize})
           drawText rs "Press any key to go to the next level"
             (V{x,y: y + tileSize * 2.0})
           cacheScreen rs
  Ref.write (Just gsTimestamp) r.gameStateId

centerPaneMap :: Instant -> UIState -> GameState -> RendererState -> Effect Unit
centerPaneMap t (UIState uis) (GameState gs) rs = do
  clear rs centerPaneRect
  for_ gs.rooms \{perimeter, visible} -> when visible $
    for_ (blockPositions $ perimeter) \pos ->
      let V p = pos
          terrain = fromMaybe Floor $ LI.index gs.terrain pos
          x = centerPaneRect.x + toNumber p.x * tileSize
          y = toNumber p.y * tileSize
          image = case terrain of
                   Wall -> "wall.png"
                   Floor -> "ground.png"
                   Exit -> "placeholder.png"
                   DoorClosed -> "doorclosed.png"
                   DoorOpen -> "dooropen.png"
      in drawImage rs image { x, y, width: tileSize, height: tileSize }

drawCenterPaneAnimations
  :: Instant -> UIState -> GameState -> RendererState -> Effect Unit
drawCenterPaneAnimations
  t
  uis@(UIState {playerAnim})
  gs@(GameState g)
  r@(RendererState rs) = do
  drawImageTemp r "player.png" (animPlayerRect t uis gs)
  forWithIndex_ g.enemies \eid nme ->
    let rect = animEnemyRect t eid nme uis gs
        image = enemyImage nme
     in drawImageTemp r image rect
  forWithIndex_ g.items \iid item ->
    let rect = animItemRect t iid item uis gs
        image = itemImage item
     in drawImageTemp r image rect

drawDraggedOrgan :: UIState -> GameState -> RendererState -> Effect Unit
drawDraggedOrgan (UIState{draggingOrgan}) gs rs@(RendererState r) = do
  case draggingOrgan of
       Nothing -> pure unit
       Just dragging@{organ: Tuple organ@(Organ (OrganSize w h) otype) p, offset } -> do
         let originalPos@(V{x,y}) = rectPos centerPaneRect + fromGrid p
             width = toNumber w * tileSize
             height = toNumber h * tileSize
         -- plaster over that organ's original position
         fillRectTemp rs {x,y,width,height} (Color "black")
         -- draw the moved organ
         let drawTo@(V dt) = originalPos + offset
             (V v) = offset
         drawOrganTemp true rs organ drawTo
         --TODO: surgery completion button

drawHighlights :: Instant -> UIState -> RendererState -> Effect Unit
drawHighlights t (UIState {highlights}) rs = do
  let color = getHighlightColor t
  for_ highlights \rect -> fillRectTemp rs rect color

getHighlightColor :: Instant -> Color
getHighlightColor t = Color $ "#ffffff" <> opacity
  where
  ms = t # unInstant # un Milliseconds
  opacity = sin (ms / 100.0) * 30.0 + 120.0
    # Int.floor
    # Int.toStringAs Int.hexadecimal

enemyImage :: Enemy -> String
enemyImage (Enemy {tag: Roomba}) = "roomba.png"

itemImage :: Item -> String
itemImage (Item {tag: HealthPickup _}) = "heart.png"

rectPos :: Rectangle -> Vector Number
rectPos {x,y} = V {x,y}

animPlayerRect :: Instant -> UIState -> GameState -> Rectangle
animPlayerRect t (UIState {playerAnim}) (GameState gs) =
  let V{x,y} = round <$>
               rectPos centerPaneRect
               + fromGrid gs.p
               + A.resolve t playerAnim
   in { x, y, width: tileSize, height: tileSize }

animEnemyRect :: Instant -> EnemyId -> Enemy -> UIState -> GameState -> Rectangle
animEnemyRect t eid (Enemy {location}) (UIState {enemyAnim}) (GameState gs) =
  let shift = fromMaybe zero $
              A.resolve t <$> Map.lookup eid enemyAnim
      V{x,y} = round <$>
                rectPos centerPaneRect
                + fromGrid location
                + shift
   in { x, y, width: tileSize, height: tileSize }

animItemRect :: Instant -> ItemId -> Item -> UIState -> GameState -> Rectangle
animItemRect t iid (Item {location}) (UIState {itemAnim}) (GameState gs) =
  let  V{x,y} = round <$>
                rectPos centerPaneRect
                + fromGrid location
   in { x, y, width: tileSize, height: tileSize }

drawRightPane :: Instant -> UIState -> GameState -> RendererState -> Effect Unit
drawRightPane t (UIState{rightPaneTarget}) (GameState gs) vars = do
  clear vars rightPaneRect
  case rightPaneTarget of
       RPEnemy eid -> case Map.lookup eid gs.enemies of
              Nothing -> pure unit
              Just n@(Enemy e) -> do
                let name = enemyName e.tag
                    Health h = e.health
                    nameLoc = let {x,y} = rightPaneRect in V{x,y}
                wrapText vars name nameLoc rightPaneRect.width
                drawImage vars "heart.png"
                  { x: targetBoardContainerRect.x --TODO: move these outside
                  , y: targetBoardContainerRect.y -- for column clues
                  , width: tileSize, height: tileSize }
                drawText vars (show h.hpCount) -- TODO: move for column clues
                  (V { x: targetBoardContainerRect.x + tileSize
                  , y: targetBoardContainerRect.y
                  })
                drawBoardBase vars h.board targetBoardRect
                drawEnemyBoardDetails vars n
       _ -> pure unit

clear :: RendererState -> Rectangle -> Effect Unit
clear rs rect = fillRect rs rect (Color "black")

fillRect :: RendererState -> Rectangle -> Color -> Effect Unit
fillRect (RendererState { cvars }) rect (Color c) = do
  ctx <- Canvas.getContext2D cvars.canvas
  Canvas.setFillStyle ctx c
  Canvas.fillRect ctx rect

drawBoardBase :: RendererState -> Board -> Rectangle -> Effect Unit
drawBoardBase vars (Board {injuries}) {x,y} =
  for_ (Array.range 0 5) \px ->
    for_ (Array.range 0 5) \py ->
      let buttonImage = if Set.member (V{x: px, y:py}) injuries
            then "ButtonPushed.png"
            else "ButtonUnpushed.png"
       in drawImage vars buttonImage
          { x: x + toNumber px * tileSize
          , y: y + toNumber py * tileSize
          , width: tileSize
          , height: tileSize
          }

drawEnemyBoardDetails :: RendererState -> Enemy -> Effect Unit
drawEnemyBoardDetails rs (Enemy e) = do
  let Health {board} = e.health
      anchor = let {x,y} = targetBoardRect in V{x,y}
  for_ (un Board board).injuries \v ->
    let drawPos = fromGrid v + anchor
     in case getOrganAtPosition board v of
          Just organ -> drawInjury rs organ drawPos
          Nothing -> case Map.lookup v e.clueCache of
                          Just clue -> drawClue rs clue drawPos
                          Nothing -> pure unit

fromGrid :: Vector Int -> Vector Number
fromGrid p = p <#> \x -> toNumber x * tileSize

organImage :: OrganType -> Boolean -> String
organImage Hp true  = "heart.png"
organImage Hp false = "injuredheart.png"
organImage PlayerHeartLarge true = "Heart4.png"
organImage PlayerHeartLarge false = "Heart4injured.png"

drawOrgan :: Boolean -> RendererState -> Organ -> Vector Number -> Effect Unit
drawOrgan isIntact rs (Organ (OrganSize w h) organType) (V{x,y}) =
  drawImage rs (organImage organType isIntact) { x,y, width: tileSize * toNumber w, height: tileSize * toNumber h }

drawOrganTemp ::
  Boolean -> RendererState -> Organ -> Vector Number -> Effect Unit
drawOrganTemp isIntact rs (Organ (OrganSize w h) organType) (V{x,y}) =
    drawImageTemp rs (organImage organType isIntact)
      { x,y, width: tileSize * toNumber w, height: tileSize * toNumber h }

drawInjury :: RendererState -> Organ -> Vector Number -> Effect Unit
drawInjury = drawOrgan false

drawClue :: RendererState -> Clue -> Vector Number -> Effect Unit
drawClue rs (HpClue i) p =    drawColorText rs (show i) (Color "#a00") p
drawClue rs (ArmorClue i) p = drawColorText rs (show i) (Color "blue") p
drawClue rs (MixedClue i) p = drawColorText rs (show i) (Color "purple") p
drawClue rs EmptyClue p =     drawColorText rs "0" (Color "#777") p
drawClue rs ConcealedClue p = drawColorText rs "?" (Color "#333") p

drawText :: RendererState -> String -> Vector Number -> Effect Unit
drawText vars string loc =
  drawColorText vars string (Color "white") loc

newtype Color = Color String
foreign import setTextBaselineHanging :: Canvas.Context2D -> Effect Unit

drawColorText :: RendererState -> String -> Color -> Vector Number -> Effect Unit
drawColorText (RendererState {cvars}) string (Color c) (V loc) = do
  ctx <- Canvas.getContext2D cvars.canvas
  Canvas.setFont ctx (show tileSize <> "px CapitalHillMono")
  setTextBaselineHanging ctx
  Canvas.setFillStyle ctx c
  Canvas.fillText ctx string (loc.x + 5.0) (loc.y + 5.0)

wrapText
  :: RendererState
  -> String
  -> Vector Number
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
drawImage r@(RendererState {cvars: { canvas, imageData }}) path {x,y, width, height} = do
  note r
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
drawImageFull r@(RendererState {cvars: { canvas, imageData }}) path s d = do
  note r
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
