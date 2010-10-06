-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Figure.Plot.Annotation
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- 'Annotation' operations
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Plot.Figure.Plot.Annotation (
                                                       Annote
                                                      , clearAnnotations
                                                      , arrow
                                                      , oval
                                                      , rect
                                                      , glyph
                                                      , text
                                                      , cairo
                                                ) where

-----------------------------------------------------------------------------

--import Data.Packed.Vector

import Control.Monad.State
import Control.Monad.Reader

import qualified Graphics.Rendering.Cairo as C

import Graphics.Rendering.Plot.Types
import Graphics.Rendering.Plot.Defaults

-----------------------------------------------------------------------------

lineInAnnote :: Line () -> Annote LineType
lineInAnnote m = do
  lo <- asks _lineoptions
  let l = execLine m lo defaultLineType
  return l

pointInAnnote :: Point () -> Annote PointType
pointInAnnote m = do
  po <- asks _pointoptions
  let p = execPoint m po defaultPointType
  return p

barInAnnote :: Bar () -> Annote BarType
barInAnnote m = do
  bo <- asks _baroptions
  let b = execBar m bo defaultBarType
  return b

textInAnnote :: Text () -> Annote TextEntry
textInAnnote m = do
  to <- asks _textoptions
  let t = execText m to NoText
  return t

-----------------------------------------------------------------------------

-- | clear annotations
clearAnnotations :: Annote ()
clearAnnotations = put []

-- | add an arrow
arrow :: Head -> Location -> Location -> Line () -> Annote ()
arrow h vs vf m = do
  l <- lineInAnnote m
  modify $ \s -> (AnnArrow h l vs vf) : s

-- | add an oval
oval :: Fill -> Location -> Location -> Bar () -> Annote ()
oval p c e m = do
  b <- barInAnnote m
  modify $ \s -> (AnnOval p b c e) : s

-- | add a rectangle
rect :: Fill -> Location -> Location -> Bar () -> Annote ()
rect p rs rf m = do
  b <- barInAnnote m
  modify $ \s -> (AnnRect p b rs rf) : s

-- | add a rectangle
glyph :: Location -> Point () -> Annote ()
glyph l m = do
  p <- pointInAnnote m
  modify $ \s -> (AnnGlyph p l) : s

-- | add text
text :: Location -> Text () -> Annote ()
text l m = do
  t <- textInAnnote m
  modify $ \s -> (AnnText t l) : s

-- | add a cairo render that takes the bounding box (in user coordinates)
--       as an argument
cairo :: (Double -> Double -> Double -> Double -> C.Render ()) -> Annote ()
cairo r = modify $ \s -> (AnnCairo r) : s

-----------------------------------------------------------------------------

