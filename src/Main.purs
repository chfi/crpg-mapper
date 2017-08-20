module Main where

import Prelude

import Color (Color, black, lighten, white)
import Color.Scheme.Clrs (gray, red)
import Control.Monad (ifM)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Random (randomBool)
import Control.Monad.Gen (class MonadGen, choose, elements)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import DOM.Classy.HTMLElement (getBoundingClientRect)
import DOM.HTML.Types (HTMLButtonElement, HTMLCanvasElement, HTMLInputElement)
import Data.Array (sortBy, (..))
import Data.Foldable (fold, foldMap)
import Data.Int (round, toNumber)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Monoid (mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Number as Number
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import FRP (FRP)
import FRP.Behavior (Behavior, animate, derivative', fixB, integral', step, unfold)
import FRP.Behavior.Mouse (buttons)
import FRP.Behavior.Mouse as Mouse
import FRP.Behavior.Time as Time
import FRP.Event (Event, create, mapMaybe)
import Global (infinity)
import Global.Unsafe (unsafeStringify)
import Graphics.Canvas (CANVAS, CanvasElement, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Drawing (Drawing, circle, fillColor, filled, lineWidth, outlineColor, outlined, path, rectangle, render, scale, text, translate)
import Graphics.Drawing as Drawing
import Graphics.Drawing.Font (font, sansSerif)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

type BoundingRect = { left :: Number
                    , top :: Number
                    , right :: Number
                    , bottom :: Number
                    , width :: Number
                    , height :: Number }

offsetPosition :: BoundingRect -> Behavior (Maybe {x :: Int, y :: Int})
offsetPosition br = (map <<< map) (\ {x,y} -> { x: x-left, y: y-top }) Mouse.position
  where left = round br.left
        top = round br.top


-- type IPoint = Tuple Int Int
newtype IPoint = IPoint {x :: Int, y :: Int}
derive instance eqIPoint :: Eq IPoint
derive instance ordIPoint :: Ord IPoint

type LevelMap a = Map IPoint a

data Tile a = Tile { wallN :: a
                   , wallE :: a
                   , wallS :: a
                   , wallW :: a
                   , floor :: a
                   }


neSet :: forall a. Set a -> Maybe (NonEmpty List a)
neSet s = case List.uncons (Set.toUnfoldable s) of
  Nothing -> Nothing
  Just {head, tail} -> Just (head :| tail)


type TileColors = { emptyFloor :: Color
                  , emptyWall :: Color
                  , blockedFloor :: Color
                  , blockedWall :: Color
                  }

generateTile :: TileColors -> Eff _ (Tile Color)
generateTile tileColors = do
  let choose = ifM randomBool
  let genWall = choose (pure tileColors.emptyWall) (pure tileColors.blockedWall)
      genFloor = choose (pure tileColors.emptyFloor) (pure tileColors.blockedFloor)
  wallN <- genWall
  wallE <- genWall
  wallS <- genWall
  wallW <- genWall
  floor <- genFloor
  pure $ Tile { wallN, wallE, wallS, wallW, floor }


generateMap :: Int -> Int -> Eff _ (LevelMap (Tile Color))
generateMap w h = do
  let colors = { emptyFloor: white
               , emptyWall: white
               , blockedFloor: gray
               , blockedWall: black
               }
      rows = (0 .. w)
      cols = (0 .. h)
      coords = do
        x <- rows
        y <- cols
        pure {x, y}
  levelMap <- traverse (\p -> Tuple (IPoint p) <$> generateTile colors) coords
  pure $ Map.fromFoldable levelMap


drawTile :: MapViewOpts -> Tile Color -> Drawing
drawTile opts (Tile tile) =
  let w = toNumber $ opts.pixelsWide / opts.tilesWide
      h = toNumber $ opts.pixelsHigh / opts.tilesHigh
      topL = {x:0.0, y:0.0 }
      topR = {x:w, y:0.0 }
      btmR = {x:w, y:h }
      btmL = {x:0.0, y:h }
      drawWall c p = outlined (lineWidth opts.wallWidth <> outlineColor c) (path p)
  in filled (fillColor tile.floor) (rectangle 0.0 0.0 w h) <>
     drawWall tile.wallN [topL, topR] <>
     drawWall tile.wallE [topR, btmR] <>
     drawWall tile.wallS [btmR, btmL] <>
     drawWall tile.wallW [btmL, topL]

emptyTile :: Tile Color
emptyTile = Tile { wallN: black
                 , wallE: black
                 , wallS: black
                 , wallW: black
                 , floor: white
                 }

drawMap :: MapViewOpts -> LevelMap (Tile Color) -> Drawing
drawMap opts level =
  let tileWidth = opts.pixelsWide / opts.tilesWide
      tileHeight = opts.pixelsHigh / opts.tilesHigh
      topL = {x:0.0, y:0.0 }
      topR = {x:toNumber tileWidth, y:0.0 }
      btmR = {x:toNumber tileWidth, y:toNumber tileHeight }
      btmL = {x:0.0, y:toNumber tileHeight }
      coords = do
        x <- (opts.offsetX .. (opts.offsetX + opts.tilesWide))
        y <- (opts.offsetY .. (opts.offsetY + opts.tilesHigh))
        pure {x, y}
      -- drawWall c p = outlined (lineWidth opts.wallWidth <> outlineColor c) (path p)
      -- drawWall c p1 p2 = outlined (lineWidth opts.wallWidth <> outlineColor c) (rectangle p1.x p1.y p2.x p2.y)
      tiles :: Array Drawing
      tiles = map (\p@{x, y} -> let (Tile tile) = fromMaybe emptyTile (Map.lookup (IPoint p) level)
                    in translate (toNumber $ tileWidth * (x - opts.offsetX))
                                 (toNumber $ tileHeight * (y - opts.offsetY)) $
                        (filled (fillColor tile.floor) $
                         rectangle 0.0 0.0 (toNumber tileWidth) (toNumber tileHeight))
                  ) coords
  in fold tiles


foreign import inputEvent :: HTMLInputElement -> Event String


inputBehaviorNumber :: HTMLInputElement -> Number -> Behavior Number
inputBehaviorNumber el n = step n (mapMaybe Number.fromString $ inputEvent el)

foreign import buttonEvent :: HTMLButtonElement -> Event Unit

buttonBehavior :: ∀ a.
                  HTMLButtonElement
               -> a
               -> (Unit -> a)
               -> Behavior a
buttonBehavior el a f = step a (f <$> buttonEvent el)


viewOpts :: ∀ r.
            {tilesWide :: HTMLInputElement, tilesHigh :: HTMLInputElement}
         -> {tilesWide :: Int, tilesHigh :: Int | r }
         -> Behavior {tilesWide :: Int, tilesHigh :: Int}
viewOpts els start = {tilesWide: _, tilesHigh: _} <$> tW <*> tH
  where tW = round <$> inputBehaviorNumber els.tilesWide (toNumber start.tilesWide)
        tH = round <$> inputBehaviorNumber els.tilesHigh (toNumber start.tilesHigh)


scrollBehavior :: ∀ r.
                  { scrollLeft :: HTMLButtonElement
                  , scrollRight :: HTMLButtonElement
                  , scrollDown :: HTMLButtonElement
                  , scrollUp :: HTMLButtonElement
                  }
               -> { offsetX :: Int, offsetY :: Int | r}
               -> Behavior { offsetX :: Int, offsetY :: Int }
scrollBehavior eles offset = {offsetX: _, offsetY: _} <$> map unwrap x <*> map unwrap y
  where l = unfold (\_ x -> x + 1) (buttonEvent eles.scrollLeft) 0
        r = unfold (\_ x -> x - 1) (buttonEvent eles.scrollRight) 0
        u = unfold (\_ y -> y + 1) (buttonEvent eles.scrollUp) 0
        d = unfold (\_ y -> y - 1) (buttonEvent eles.scrollDown) 0
        x = foldMap (map Additive) [l, r]
        y = foldMap (map Additive) [u, d]

combineOpts :: MapViewOpts
            -> Behavior { offsetX :: Int, offsetY :: Int }
            -> Behavior { tilesWide :: Int, tilesHigh :: Int }
            -> Behavior MapViewOpts
combineOpts opts offset tile = opts'
  where opts' = (\ {offsetX, offsetY} {tilesWide, tilesHigh} ->
                  opts { offsetX=offsetX
                       , offsetY=offsetY
                       , tilesWide=tilesWide
                       , tilesHigh=tilesHigh
                       }) <$> offset <*> tile



mapScene :: ∀ r.
            LevelMap (Tile Color)
         -> Behavior MapViewOpts
         -> Behavior Drawing
mapScene level opts = (drawMap <$> opts <*> (pure level)) <>
                      ((\o -> (text (font sansSerif 12 mempty) 25.0 25.0
                             (fillColor red) (show o.offsetX))) <$> opts)


data MapOptions = MapOptions MapViewOpts

-- TODO unsafe af
foreign import getElement :: ∀ eff. String -> Eff (dom :: DOM | eff) HTMLInputElement
foreign import getButton :: ∀ eff. String -> Eff (dom :: DOM | eff) HTMLButtonElement


-- Render map only on state update?
-- really just s/Behavior/Event/g,
-- and then subscribe to that with the drawing function

canvasElementToHTML :: CanvasElement -> HTMLCanvasElement
canvasElementToHTML = unsafeCoerce


type MapViewOpts = { pixelsWide :: Int
                   , pixelsHigh :: Int
                   , tilesWide :: Int
                   , tilesHigh :: Int
                   , offsetX :: Int
                   , offsetY :: Int
                   , wallWidth :: Number
                   }

-- main :: forall eff. Eff (canvas :: CANVAS, frp :: FRP | eff) Unit
-- main :: Eff _ Unit
main :: _
main = do
  mcanvas <- getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mcanvas)
  ctx <- getContext2D canvas
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  _ <- setCanvasWidth w canvas
  _ <- setCanvasHeight h canvas
  offset <- getBoundingClientRect $ canvasElementToHTML canvas
  log $ unsafeStringify offset
  level <- generateMap 16 16
  wEl <- getElement "tilesWide"
  hEl <- getElement "tilesHigh"
  -- wEl <- unsafePartial <<< fromJust <$> getElementById (ElementId "tileWidth")
  -- hEl <- unsafePartial <<< fromJust <$> getCanvasElementById "tileHeight"
  sL <- getButton "scrollLeft"
  sR <- getButton "scrollRight"
  sU <- getButton "scrollUp"
  sD <- getButton "scrollDown"

  let w' = round w
      h' = round h
      opts = { pixelsWide: w'
             , pixelsHigh: h'
             , tilesWide: 32
             , tilesHigh: 32
             , offsetX: 0
             , offsetY: 0
             , wallWidth: 4.0
             }
  -- Drawing.render ctx $ drawMap opts level
      vOpts = viewOpts {tilesWide: wEl, tilesHigh: hEl} opts
      scroll = scrollBehavior { scrollLeft: sL
                              , scrollRight: sR
                              , scrollUp: sU
                              , scrollDown: sD } opts
      behv = combineOpts opts scroll vOpts

  animate (pure (filled (fillColor white) (rectangle 0.0 0.0 w h)) <>
           mapScene level behv) (render ctx)
