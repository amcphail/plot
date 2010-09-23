{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Figure.Bar
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- 'Bar' operations
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Plot.Figure.Bar (
                                            Bar, BarFormat(..)
                                           , clearBarFormat
                                           , setBarWidth
                                           , setBarColour
                                           , setBarBorderWidth
                                           , setBarBorderColour
                                           , getBarColour
                                           ) where

-----------------------------------------------------------------------------

--import Data.Word
import Data.Colour
--import Data.Colour.Names

--import qualified Graphics.Rendering.Cairo as C
--import qualified Graphics.Rendering.Pango as P

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Supply

import Graphics.Rendering.Plot.Types

-----------------------------------------------------------------------------

changeBarColour :: Color -> BarType -> BarType
changeBarColour c (ColourBar _)  = ColourBar c
changeBarColour c (TypeBar lo _) = TypeBar lo c

clearBarFormatting :: BarType -> BarType
clearBarFormatting l@(ColourBar _) = l
clearBarFormatting (TypeBar _ c)   = ColourBar c

getBarColour :: BarType -> Color
getBarColour (ColourBar c) = c
getBarColour (TypeBar _ c) = c

changeBarWidth :: Width -> BarOptions -> BarOptions
changeBarWidth w (BarOptions _ bw bc) = BarOptions w bw bc

changeBarBorderWidth :: LineWidth -> BarOptions -> BarOptions
changeBarBorderWidth bw (BarOptions w _ bc) = BarOptions w bw bc

changeBarBorderColour :: Color -> BarOptions -> BarOptions
changeBarBorderColour bc (BarOptions w bw _) = BarOptions w bw bc

-----------------------------------------------------------------------------

-- | clear the formatting of a line
clearBarFormat :: Bar ()
clearBarFormat = do
                  bt <- get
                  case bt of
                          c@(ColourBar _) -> put c
                          (TypeBar _ c)   -> put $ ColourBar c

changeBarOptions :: (BarOptions -> BarOptions) -> BarType -> Bar ()
changeBarOptions o (ColourBar c)  = do
                                      bo <- ask
                                      put $ TypeBar (o bo) c
changeBarOptions o (TypeBar bo c) = put $ TypeBar (o bo) c

-- | set the width of the bar
setBarWidth :: Width -> Bar ()
setBarWidth bw = get >>= changeBarOptions (changeBarWidth bw)

-- | set the colour of the bar
setBarColour :: Color -> Bar ()
setBarColour c = modify (changeBarColour c)

-- | set the width of the bar border
setBarBorderWidth :: LineWidth -> Bar ()
setBarBorderWidth bw = get >>= changeBarOptions (changeBarBorderWidth bw)

-- | set the colour of the bar border
setBarBorderColour :: Color -> Bar ()
setBarBorderColour c = get >>= changeBarOptions (changeBarBorderColour c)

-----------------------------------------------------------------------------

class BarFormat a where
    toBar :: (MonadReader Options m, MonadSupply SupplyData m) => a -> m BarType

instance BarFormat Width                      where toBar w          = do
                                                                    bo <- asks _baroptions
                                                                    c <- supply
                                                                    return $ TypeBar (changeBarWidth w bo) c
instance Real a => BarFormat (Colour a)           where toBar c         = return $ ColourBar $ colourConvert c
instance Real a => BarFormat (Width,Colour a) where toBar (w,c)    = do
                                                                    bo <- asks _baroptions
                                                                    return $ TypeBar (changeBarWidth w bo) $ colourConvert c
instance Real a => BarFormat (Width,Colour a,LineWidth) where toBar (bw,c,lw) = do
                                                                    bo <- asks _baroptions
                                                                    return $ TypeBar (changeBarWidth bw $ changeBarBorderWidth lw bo) $ colourConvert c
instance (Real a, Real b) => BarFormat (Width,Colour a,Colour b) where toBar (bw,c,bc) = do
                                                                                         bo <- asks _baroptions
                                                                                         return $ TypeBar (changeBarWidth bw $ changeBarBorderColour (colourConvert bc) bo) $ colourConvert c
instance (Real a, Real b) => BarFormat (Width,Colour a,LineWidth,Colour b) where toBar (bw,c,lw,bc) = return $ TypeBar (BarOptions bw lw (colourConvert bc)) $ colourConvert c

-----------------------------------------------------------------------------

