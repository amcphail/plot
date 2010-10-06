{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Render.Types
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- Rendering 'Figure's
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Plot.Render.Types where

-----------------------------------------------------------------------------

--import Data.Either

--import Data.Packed.Vector
--import Numeric.LinearAlgebra.Linear

--import Data.Word

import Data.Maybe

import Data.Colour.SRGB
import Data.Colour.Names

--import qualified Data.Array.IArray as A

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as CM
import qualified Graphics.Rendering.Pango as P

import Control.Monad.Reader
import Control.Monad.State
--import Control.Monad.Trans

import Graphics.Rendering.Plot.Types
import Graphics.Rendering.Plot.Defaults

--import Graphics.Rendering.Plot.Figure.Text

--import qualified Text.Printf as Printf

--import Prelude hiding(min,max)
--import qualified Prelude(max)

-----------------------------------------------------------------------------
{-
newtype Render a = FR { runRender :: StateT BoundingBox C.Render a }
    deriving(Monad, MonadState BoundingBox, MonadTrans (StateT BoundingBox))
-}

data RenderEnv = RenderEnv {
                  _pangocontext    :: P.PangoContext
                  , _renderoptions :: Options
                 }

newtype BoundedT m a = BT { runRender :: ReaderT RenderEnv (StateT BoundingBox m) a }
    deriving(Monad, MonadState BoundingBox, MonadReader RenderEnv)

instance MonadTrans BoundedT where
    lift m = BT $ lift $ lift m

type Render = BoundedT C.Render

evalRender :: Render a -> RenderEnv -> BoundingBox -> C.Render a
evalRender m r = evalStateT (runReaderT (runRender m) r)

-----------------------------------------------------------------------------

cairo :: C.Render a -> Render a
cairo = lift

pango :: IO a -> C.Render a
pango = liftIO

-----------------------------------------------------------------------------

bbX, bbY, bbW, bbH :: Render Double
bbX = gets _bbX
bbY = gets _bbY
bbW = gets _bbW
bbH = gets _bbH
 
bbLeftWidth, bbCentreWidth, bbRightWidth, bbBottomHeight, bbCentreHeight, bbTopHeight :: Render Double
bbLeftWidth    = gets $ \(BoundingBox x _ _ _) -> x
bbCentreWidth  = gets $ \(BoundingBox x _ w _) -> x + w / 2
bbRightWidth   = gets $ \(BoundingBox x _ w _) -> x + w
bbBottomHeight = gets $ \(BoundingBox _ y _ h) -> y + h
bbCentreHeight = gets $ \(BoundingBox _ y _ h) -> y + h / 2
bbTopHeight    = gets $ \(BoundingBox _ y _ _) -> y

bbShiftLeft, bbShiftRight, bbLowerTop, bbRaiseBottom :: Double -> Render ()
bbShiftLeft   n = modify $ \(BoundingBox x y w h) -> BoundingBox (x+n) y     (w-n) h
bbShiftRight  n = modify $ \(BoundingBox x y w h) -> BoundingBox x     y     (w-n) h
bbLowerTop    n = modify $ \(BoundingBox x y w h) -> BoundingBox x     (y+n) w     (h-n)
bbRaiseBottom n = modify $ \(BoundingBox x y w h) -> BoundingBox x     y     w     (h-n)

applyPads :: Padding -> Render ()
applyPads (Padding l r b t) = modify (\(BoundingBox x y w h) -> BoundingBox (x+l) (y+t) (w-l-r) (h-t-b))

-----------------------------------------------------------------------------

clipBoundary :: Render ()
clipBoundary = do
               (BoundingBox x y w h) <- get
               cairo $ do
                       C.rectangle x y w h
                       C.clip

-----------------------------------------------------------------------------

-- | output file type
data OutputType = PNG | PS | PDF | SVG

-----------------------------------------------------------------------------

setColour :: Color -> C.Render ()
setColour c = let (RGB r g b) = toSRGB c
              in C.setSourceRGBA r g b 1 -- no transparent colours


setDashes :: [Dash] -> C.Render ()
setDashes [] = C.setDash [] 0
setDashes xs = do
               let xs' = map (\d -> case d of { Dot -> 1 ; Dash -> 3 }) xs
               C.setDash xs' 0
                     
-----------------------------------------------------------------------------

getDefaultTextOptions :: P.PangoContext -> IO TextOptions
getDefaultTextOptions pc = do
                 fd <- P.contextGetFontDescription pc
                 getTextOptionsFD fd

getTextOptionsFD :: P.FontDescription -> IO TextOptions
getTextOptionsFD fd = do
                     ff' <- P.fontDescriptionGetFamily fd
                     fs' <- P.fontDescriptionGetStyle fd
                     fv' <- P.fontDescriptionGetVariant fd
                     fw' <- P.fontDescriptionGetWeight fd
                     fc' <- P.fontDescriptionGetStretch fd
                     fz' <- P.fontDescriptionGetSize fd
                     let ff = fromMaybe defaultFontFamily ff'
                         fs = fromMaybe defaultFontStyle fs'
                         fv = fromMaybe defaultFontVariant fv'
                         fw = fromMaybe defaultFontWeight fw'
                         fc = fromMaybe defaultFontStretch fc'
                         fz = fromMaybe defaultFontSize fz'
                     return $ TextOptions (FontOptions ff fs fv fw fc) fz black

setTextOptions :: TextOptions -> P.PangoLayout -> C.Render ()
setTextOptions to lo = do
                       fd' <- pango $ P.layoutGetFontDescription lo
                       fd <- case fd' of
                                      Nothing   -> pango $ P.fontDescriptionNew
                                      Just fd'' -> return fd''
                       setTextOptionsFD to fd
                       pango $ P.layoutSetFontDescription lo (Just fd)

setTextOptionsFD :: TextOptions -> P.FontDescription -> C.Render ()
setTextOptionsFD (TextOptions (FontOptions ff fs fv fw fc) fz c) fd = do
                 pango $ do
                          P.fontDescriptionSetFamily fd ff
                          P.fontDescriptionSetStyle fd fs
                          P.fontDescriptionSetVariant fd fv
                          P.fontDescriptionSetWeight fd fw
                          P.fontDescriptionSetStretch fd fc
                          P.fontDescriptionSetSize fd fz
                 setColour c

-----------------------------------------------------------------------------

textPad :: Double
textPad = 2

data TextXAlign = TLeft | Centre | TRight
data TextYAlign = TBottom | Middle | TTop

-----------------------------------------------------------------------------

setLineOptions :: LineOptions -> C.Render ()
setLineOptions (LineOptions ds lw) = do
                                     setDashes ds
                                     C.setLineWidth lw

setLineStyle :: LineType -> C.Render ()
setLineStyle NoLine          = return ()
setLineStyle (ColourLine c)  = setColour c
setLineStyle (TypeLine lo c) = do
                               setLineOptions lo
                               setColour c

-----------------------------------------------------------------------------

setPointOptions :: PointOptions -> C.Render ()
setPointOptions (PointOptions pz c) = do
                                      setColour c
                                      C.scale pz pz

setPointStyle :: PointType -> C.Render Glyph
setPointStyle (FullPoint po g) = do
                                 setPointOptions po
                                 return g

-----------------------------------------------------------------------------

flipVerticalMatrix :: CM.Matrix
flipVerticalMatrix = CM.Matrix 1 0 0 (-1) 0 0

flipVertical :: C.Render ()
flipVertical = C.transform flipVerticalMatrix

-----------------------------------------------------------------------------




