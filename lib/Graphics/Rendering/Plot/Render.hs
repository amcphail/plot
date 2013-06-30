{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Render
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

module Graphics.Rendering.Plot.Render (
                                       -- * Rendering
                                       render
                                       -- ** Access to 'FigureState'
                                       , newFigureState
                                       , updateFigureState
                                       , renderFigureState
                                       -- ** Outputting to file
                                       , OutputType(..)
                                       , writeFigure
                                       , writeFigureState
                                       -- * Notes
                                       -- $notes
                                       ) where

-----------------------------------------------------------------------------
{- TODO

    store 'next colour' list in state
-}
-----------------------------------------------------------------------------

--import Data.Either

--import Data.Packed.Vector
--import Numeric.LinearAlgebra.Linear

--import Data.Word

--import Data.Maybe

--import Data.Colour.SRGB
--import Data.Colour.Names

--import qualified Data.Array.IArray as A

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Pango as P

--import Control.Monad.Reader
--import Control.Monad.State
--import Control.Monad.Trans

import Graphics.Rendering.Plot.Types
import Graphics.Rendering.Plot.Defaults

--import Graphics.Rendering.Plot.Figure.Text

import Graphics.Rendering.Plot.Render.Types
import Graphics.Rendering.Plot.Render.Text
import Graphics.Rendering.Plot.Render.Plot

--import qualified Text.Printf as Printf

--import Prelude hiding(min,max)
--import qualified Prelude(max)

-----------------------------------------------------------------------------

-- | render a 'Figure'
render :: Figure ()    -- ^ the figure to be rendered
       -> (Int,Int)    -- ^ (width,height)
       -> C.Render ()  -- ^ a Cairo operation
render g = (\(w,h) -> do
                      pc <- pango $ P.cairoCreateContext Nothing
                      to <- pango $ getDefaultTextOptions pc
                      let options' = Options defaultLineOptions defaultPointOptions defaultBarOptions to
                      let (FigureState options _ figure) = execFigure g (FigureState options' defaultSupply emptyFigure)
                      evalRender (renderFigure figure) (RenderEnv pc options) (BoundingBox 0 0 (fromIntegral w) (fromIntegral h)))

-----------------------------------------------------------------------------

-- | create 'FigureState' from a series of 'Figure' actions
newFigureState :: Figure () -> IO FigureState
newFigureState f = do
              pc <- P.cairoCreateContext Nothing
              to <- getDefaultTextOptions pc
              let options' = Options defaultLineOptions defaultPointOptions defaultBarOptions to
              return $ execFigure f (FigureState options' defaultSupply emptyFigure)
            
-- | modify a 'FigureState' with some new actions  
updateFigureState :: FigureState -> Figure () -> FigureState
updateFigureState s f = execFigure f s

-- | render a 'FigureState'
renderFigureState :: FigureState  -- ^ the figure state
                -> (Int,Int)    -- ^ (width,height)
                -> C.Render ()  -- ^ a Cairo operation
renderFigureState (FigureState options _ figure) = (\(w,h) -> do
                                                              pc <- pango $ P.cairoCreateContext Nothing
                                                              evalRender (renderFigure figure) (RenderEnv pc options) (BoundingBox 0 0 (fromIntegral w) (fromIntegral h)))    

-----------------------------------------------------------------------------

-- | output the 'Figure'
writeFigure :: OutputType    -- ^ output file type
            -> FilePath      -- ^ file path
            -> (Int,Int)     -- ^ (width,height)
            -> Figure ()     -- ^ the 'Figure' rendering operation
            -> IO ()
writeFigure PNG fn wh f = withImageSurface wh (writeSurfaceToPNG fn (render f wh))
writeFigure PS  fn wh f = writeSurface C.withPSSurface fn wh f
writeFigure PDF fn wh f = writeSurface C.withPDFSurface fn wh f
writeFigure SVG fn wh f = writeSurface C.withSVGSurface fn wh f

withImageSurface :: (Int,Int) -> (C.Surface -> IO ()) -> IO ()
withImageSurface (w,h) = C.withImageSurface C.FormatARGB32 w h

writeSurfaceToPNG :: FilePath -> C.Render () -> C.Surface -> IO ()
writeSurfaceToPNG fn r s = do
                           C.renderWith s r
                           C.surfaceWriteToPNG s fn

writeSurface :: (FilePath -> Double -> Double -> (C.Surface -> IO ()) -> IO ()) 
            -> FilePath -> (Int,Int) -> Figure () -> IO ()
writeSurface rw fn (w,h) f = rw fn (fromIntegral w) (fromIntegral h) (flip C.renderWith (render f (w,h))) 
 
-----------------------------------------------------------------------------

-- | output the 'FigureState'
writeFigureState :: OutputType    -- ^ output file type
            -> FilePath      -- ^ file path
            -> (Int,Int)     -- ^ (width,height)
            -> FigureState   -- ^ a FigureState
            -> IO ()
writeFigureState PNG fn wh f = withImageSurface wh (writeSurfaceToPNG fn (renderFigureState f wh))
writeFigureState PS  fn wh f = writeSurfaceFS C.withPSSurface fn wh f
writeFigureState PDF fn wh f = writeSurfaceFS C.withPDFSurface fn wh f
writeFigureState SVG fn wh f = writeSurfaceFS C.withSVGSurface fn wh f

writeSurfaceFS :: (FilePath -> Double -> Double -> (C.Surface -> IO ()) -> IO ()) 
            -> FilePath -> (Int,Int) -> FigureState -> IO ()
writeSurfaceFS rw fn (w,h) f = rw fn (fromIntegral w) (fromIntegral h) (flip C.renderWith (renderFigureState f (w,h))) 
 
-----------------------------------------------------------------------------

renderFigure :: FigureData -> Render ()
renderFigure (Figure b p t s d) = do
      cairo $ do
             C.save 
             setColour b
             C.paint
             C.restore

      applyPads p

      tx <- bbCentreWidth
      ty <- bbTopHeight
      (_,th) <- renderText t Centre TTop tx ty
      bbLowerTop (th+textPad)

      sx <- bbCentreWidth
      sy <- bbTopHeight
      (_,sh) <- renderText s Centre TTop sx sy
      bbLowerTop (sh+textPad)

      renderPlots d
                                                      
