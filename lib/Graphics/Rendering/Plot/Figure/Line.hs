{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Figure.Line
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- 'Text' operations
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Plot.Figure.Line (
                                            Line, LineFormat(..)
                                           , DashStyle,Dash(..),LineWidth
                                           , clearLineFormat
                                           , setDashStyle
                                           , setLineWidth
                                           , setLineColour
                                           , getLineColour
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

changeDashStyle :: DashStyle -> LineOptions -> LineOptions
changeDashStyle ds (LineOptions _ lw) = LineOptions ds lw

changeLineWidth :: LineWidth -> LineOptions -> LineOptions
changeLineWidth lw (LineOptions ds _) = LineOptions ds lw

{-changeLineOptions :: (LineOptions -> LineOptions) -> LineType -> LineType
changeLineOptions f (LineType ls c) = LineType (f ls) c

changeDashStyle :: DashStyle -> LineType -> LineType
changeDashStyle ds = changeLineOptions (changeDashStyleStyle ds)

changeLineWidth :: LineWidth -> LineType -> LineType
changeLineWidth lw = changeLineOptions (changeLineWidthStyle lw)
-}
changeLineColour :: Color -> LineType -> LineType
changeLineColour c NoLine          = ColourLine c
changeLineColour c (ColourLine _)  = ColourLine c
changeLineColour c (TypeLine lo _) = TypeLine lo c

clearLineFormatting :: LineType -> LineType
clearLineFormatting NoLine           = NoLine
clearLineFormatting l@(ColourLine _) = l
clearLineFormatting (TypeLine _ c)   = ColourLine c

clearLine :: LineType -> LineType
clearLine _ = NoLine

getLineColour :: LineType -> Maybe Color
getLineColour NoLine         = Nothing
getLineColour (ColourLine c) = Just c
getLineColour (TypeLine _ c) = Just c

-----------------------------------------------------------------------------

-- | clear the formatting of a line
clearLineFormat :: Line ()
clearLineFormat = do
                  lt <- get
                  case lt of
                          NoLine           -> put NoLine
                          c@(ColourLine _) -> put c
                          (TypeLine _ c)   -> put $ ColourLine c

changeLineOptions :: (LineOptions -> LineOptions) -> LineType -> Line ()
changeLineOptions o NoLine          = do
                                      lo <- ask
                                      put $ TypeLine (o lo) black
changeLineOptions o (ColourLine c)  = do
                                      lo <- ask
                                      put $ TypeLine (o lo) c
changeLineOptions o (TypeLine lo c) = put $ TypeLine (o lo) c

-- | change the dash style of a line
setDashStyle :: DashStyle -> Line ()
setDashStyle ds = get >>= changeLineOptions (changeDashStyle ds)

-- | change the line width of a line
setLineWidth :: LineWidth -> Line ()
setLineWidth lw = get >>= changeLineOptions (changeLineWidth lw)

-- | change the line colour of a line
setLineColour :: Color -> Line ()
setLineColour c = modify (changeLineColour c)

-----------------------------------------------------------------------------

class LineFormat a where
    toLine :: (MonadReader Options m, MonadSupply SupplyData m) => a -> m LineType

instance Real a => LineFormat (Colour a)           where toLine c         = return $ ColourLine $ colourConvert c
instance LineFormat DashStyle                      where toLine ds        = do
                                                                    lo <- asks _lineoptions
                                                                    c <- supply
                                                                    return $ TypeLine (changeDashStyle ds lo) c
instance LineFormat LineWidth                      where toLine lw        = do
                                                                    lo <- asks _lineoptions
                                                                    c <- supply
                                                                    return $ TypeLine (changeLineWidth lw lo) c
instance Real a => LineFormat (DashStyle,Colour a) where toLine (ds,c)    = do
                                                                    lo <- asks _lineoptions
                                                                    return $ TypeLine (changeDashStyle ds lo) $ colourConvert c
instance Real a => LineFormat (LineWidth,Colour a) where toLine (lw,c)    = do
                                                                    lo <- asks _lineoptions
                                                                    return $ TypeLine (changeLineWidth lw lo) $ colourConvert c
instance LineFormat (DashStyle,LineWidth)          where toLine (ds,lw)   = do
                                                                    c <- supply
                                                                    return $ TypeLine (LineOptions ds lw) c
instance Real a => LineFormat (DashStyle,LineWidth,Colour a) where toLine (ds,lw,c) = return $ TypeLine (LineOptions ds lw) $ colourConvert c

-----------------------------------------------------------------------------

