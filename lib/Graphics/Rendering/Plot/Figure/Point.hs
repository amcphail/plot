{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Figure.Point
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- 'Point' operations
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Plot.Figure.Point (
                                             Point, PointFormat(..)
                                            , PointSize
--                                            , clearPointFormat
                                            , setGlyph
                                            , setPointSize
                                            , setPointColour
                                            , getPointColour
                                            ) where

-----------------------------------------------------------------------------

--import Data.Word
import Data.Colour
--import Data.Colour.SRGB
--import Data.Colour.Names

--import qualified Graphics.Rendering.Cairo as C
--import qualified Graphics.Rendering.Pango as P

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Supply

import Graphics.Rendering.Plot.Types

-----------------------------------------------------------------------------

changePointSize :: PointSize -> PointOptions -> PointOptions
changePointSize sz (PointOptions _ c) = PointOptions sz c

changePointColour :: Color -> PointOptions -> PointOptions
changePointColour c (PointOptions sz _) = PointOptions sz c

getPointColour :: PointType -> Color
getPointColour (FullPoint (PointOptions _ c) _) = c

changePointGlyph :: Glyph -> PointType -> PointType
--changePointGlyph gt s (BarePoint _)    = BarePoint (Glyph gt s)
changePointGlyph g (FullPoint po _) = FullPoint po g

-----------------------------------------------------------------------------
{-
-- | clear the formatting of a point
clearPointFormat :: Point ()
clearPointFormat = do
                   pt <- get
                   case pt of
                           g@(BarePoint _) -> put g
                           (FullPoint _ g) -> put $ BarePoint g
-}

changePointOptions :: (PointOptions -> PointOptions) -> PointType -> Point ()
--changePointOptions o (BarePoint g)    = do
--                                        po <- ask
--                                        put $ FullPoint (o po) g
changePointOptions o (FullPoint po g) = put $ FullPoint (o po) g

-- | change the glyph of a point
setGlyph :: Glyph -> Point ()
setGlyph g = modify $ \s -> changePointGlyph g s

-- | change the size of a point
setPointSize :: PointSize -> Point ()
setPointSize sz = get >>= changePointOptions (changePointSize sz)

-- | change the colour of a point
setPointColour :: Color -> Point ()
setPointColour c = get >>= changePointOptions (changePointColour c)

-----------------------------------------------------------------------------

class PointFormat a where
    toPoint :: (MonadReader Options m, MonadSupply SupplyData m) => a -> m PointType
 
instance PointFormat Glyph                   where toPoint g       = do
                                                                po <- asks _pointoptions
                                                                c <- supply
                                                                return $ FullPoint (changePointColour c po) g
--instance PointFormat GlyphType               where toPoint g       = return $ BarePoint g
instance Real a => PointFormat (Colour a)    where toPoint c       = do
                                                                po <- asks _pointoptions
                                                                g <- supply
                                                                return $ FullPoint (changePointColour (colourConvert c) po) g
instance PointFormat (Glyph,PointSize)       where toPoint (g,s)   = do
                                                                po <- asks _pointoptions
                                                                c <- supply
                                                                return $ FullPoint (changePointSize s $ changePointColour c po) g
instance Real a => PointFormat (Glyph,Colour a) where toPoint (g,c)   = do
                                                                po <- asks _pointoptions
                                                                return $ FullPoint (changePointColour (colourConvert c) po) g
instance Real a => PointFormat (Glyph,PointSize,Colour a) where toPoint (g,s,c) = return $ FullPoint (PointOptions s (colourConvert c)) g

-----------------------------------------------------------------------------

{- TODO

  fix Glyph/GlyphType differences
  NoPoint option?
-}


