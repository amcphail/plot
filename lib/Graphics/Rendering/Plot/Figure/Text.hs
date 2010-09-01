-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Figure.Text
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

module Graphics.Rendering.Plot.Figure.Text (
                                            Text
                                           , FontFamily,FontSize,Color
                                           -- | A text element must exist for formatting to work
                                           , clearText
                                           , clearTextFormat
                                           , setText
                                           , setFontFamily
                                           , setFontStyle
                                           , setFontVariant
                                           , setFontWeight
                                           , setFontStretch
                                           , setFontSize
                                           , setFontColour
                                           --
                                           , changeFontSize
                                           , changeFontColour
                                           --
                                           , scaleFontSize
                                           ) where

-----------------------------------------------------------------------------

import Control.Monad.State
import Control.Monad.Reader

import qualified Graphics.Rendering.Pango as P

import Graphics.Rendering.Plot.Types

-----------------------------------------------------------------------------

changeFontFamilyFont :: FontFamily -> FontOptions -> FontOptions
changeFontFamilyFont ff (FontOptions _ fs fv fw fc) = FontOptions ff fs fv fw fc

changeFontStyleFont :: P.FontStyle -> FontOptions -> FontOptions
changeFontStyleFont fs (FontOptions ff _ fv fw fc) = FontOptions ff fs fv fw fc

changeFontVariantFont :: P.Variant -> FontOptions -> FontOptions
changeFontVariantFont fv (FontOptions ff fs _ fw fc) = FontOptions ff fs fv fw fc

changeFontWeightFont :: P.Weight -> FontOptions -> FontOptions
changeFontWeightFont fw (FontOptions ff fs fv _ fc) = FontOptions ff fs fv fw fc

changeFontStretchFont :: P.Stretch -> FontOptions -> FontOptions
changeFontStretchFont fc (FontOptions ff fs fv fw _) = FontOptions ff fs fv fw fc

changeFontOptionsFont :: (FontOptions -> FontOptions) -> TextOptions -> TextOptions
changeFontOptionsFont f (TextOptions fo fz c) = TextOptions (f fo) fz c

changeFontFamily :: FontFamily -> TextOptions -> TextOptions
changeFontFamily ff = changeFontOptionsFont $ changeFontFamilyFont ff

changeFontStyle :: P.FontStyle -> TextOptions -> TextOptions
changeFontStyle fs = changeFontOptionsFont $ changeFontStyleFont fs

changeFontVariant :: P.Variant -> TextOptions -> TextOptions
changeFontVariant fv = changeFontOptionsFont $ changeFontVariantFont fv

changeFontWeight :: P.Weight -> TextOptions -> TextOptions
changeFontWeight fw = changeFontOptionsFont $ changeFontWeightFont fw

changeFontStretch :: P.Stretch -> TextOptions -> TextOptions
changeFontStretch fc = changeFontOptionsFont $ changeFontStretchFont fc

changeFontSize :: FontSize -> TextOptions -> TextOptions
changeFontSize fz (TextOptions fo _ c) = TextOptions fo fz c

scaleFontSize :: Double -> TextOptions -> TextOptions
scaleFontSize sc (TextOptions fo fz c) = TextOptions fo (sc*fz) c

changeFontColour :: Color -> TextOptions -> TextOptions
changeFontColour c (TextOptions fo fz _) = TextOptions fo fz c

changeFontTextSize :: FontSize -> TextEntry -> TextEntry
changeFontTextSize fz (FontText to s) = FontText (changeFontSize fz to) s
changeFontTextSize _ _ = error "changeFontTextSize"

changeFontTextColour :: Color -> TextEntry -> TextEntry
changeFontTextColour c (FontText to s) = FontText (changeFontColour c to) s
changeFontTextColour _ _ = error "changeFontTextColour"

changeText :: String -> TextEntry -> TextEntry
changeText s NoText            = BareText s 
changeText s (BareText _)      = BareText s
changeText s (SizeText fz c _) = SizeText fz c s
changeText s (FontText to _)   = FontText to s

clearTextEntryFormat :: TextEntry -> TextEntry
clearTextEntryFormat NoText           = NoText
clearTextEntryFormat t@(BareText _)   = t
clearTextEntryFormat (SizeText _ _ s) = BareText s
clearTextEntryFormat (FontText _ s)   = BareText s

-----------------------------------------------------------------------------

-- | clear the text entry 
clearText :: Text ()
clearText = put NoText

-- | set the text formatting to the default
clearTextFormat :: Text ()
clearTextFormat = modify clearTextEntryFormat

-- | set the value of a text entry
setText :: String -> Text ()
setText l = modify (changeText l)

changeFontOptions :: (TextOptions -> TextOptions) -> TextEntry -> Text ()
changeFontOptions _ NoText            = return ()
changeFontOptions o (BareText s)      = do
                                        to <- ask
                                        put $ FontText (o to) s
changeFontOptions o (SizeText fz c s) = do
                                        to <- ask
                                        let (TextOptions fo _ _) = o to
                                        put $ FontText (TextOptions fo fz c) s
changeFontOptions o (FontText to s)   = put $ FontText (o to) s
                                        

-- | set the font style of a text entry
setFontFamily :: FontFamily -> Text ()
setFontFamily ff = get >>= changeFontOptions (changeFontFamily ff)

-- | set the font style of a text entry
setFontStyle :: P.FontStyle -> Text ()
setFontStyle fs = get >>= changeFontOptions (changeFontStyle fs)
                          
-- | set the font variant of a text entry
setFontVariant :: P.Variant -> Text ()
setFontVariant fv = get >>= changeFontOptions (changeFontVariant fv)
                          
-- | set the font weight of a text entry
setFontWeight :: P.Weight -> Text ()
setFontWeight fw = get >>= changeFontOptions (changeFontWeight fw)

-- | set the font stretch of a text entry
setFontStretch :: P.Stretch -> Text ()
setFontStretch fc = get >>= changeFontOptions (changeFontStretch fc)

-- | set the font size of a text entry
setFontSize :: FontSize -> Text ()
setFontSize fz = do
                 t <- get
                 case t of
                        NoText                  -> return ()
                        (BareText s)            -> do
                                                   (TextOptions _ _ c) <- ask
                                                   put $ SizeText fz c s
                        (SizeText _ c s)        -> put $ SizeText fz c s
                        (FontText to s)         -> put $ FontText (changeFontSize fz to) s

-- | set the colour of a text entry
setFontColour :: Color -> Text ()
setFontColour c = do
                 t <- get
                 case t of
                        NoText                  -> return ()
                        (BareText s)            -> do
                                                   (TextOptions _ fz _) <- ask
                                                   put $ SizeText fz c s
                        (SizeText fz _ s)       -> put $ SizeText fz c s
                        (FontText to s)         -> put $ FontText (changeFontColour c to) s

-----------------------------------------------------------------------------

                          
