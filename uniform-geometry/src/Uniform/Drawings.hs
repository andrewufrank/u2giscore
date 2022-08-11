-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Point2d
-- | Poind2d with ID and V2 for coordinates and plain V2 
-- conversion to other formats as far as necessary 
-- done with classes 
--          and conversions general conversions
-------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric  #-}
-- {-# LANGUAGE TypeApplications     #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
{-# OPTIONS_GHC  -Wno-missing-home-modules #-}
    -- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.Drawings
    (module Uniform.Drawings
    , module Graphics.Gloss.Data.Color
    ) where
 
import UniformBase
import Uniform.Point2d 
import Uniform.Point2dData
import Uniform.GeometryFunctions
import Graphics.Gloss hiding (scale)
import qualified Graphics.Gloss.Data.Point.Arithmetic as A
import Graphics.Gloss.Data.Color
import qualified Graphics.Gloss as Gloss
import Numeric.Extra -- for conversion to gloss data type float 

import Control.Lens 
-- -- import GHC.Generics

-- | polygon with a function to close the line
-- and likely more 
class DrawPolygon a where 
    closeLine :: a -> a 

-- | a figure with a color and a path
-- add a lablel with a point 
data Figure a = Figure 
    { _figColor :: Color
    , _figPath :: [a]
    } deriving (Show,    Eq, Generic, Zeros)

-- instance (Zeros a) => Zeros (Figure a) where 
--     zero = Figure zero zero

instance Zeros (Color) where zero = greyN 0.5

makeLenses ''Figure   -- only available from here onwards 

instance DrawPolygon (Figure V2D) where 
    closeLine = over figPath  lineClose   


figure2picture' ::  Figure V2D -> Picture
figure2picture' f =  color (f ^. figColor) $ Line .  map toGloss $ _figPath f 

fig1 :: Figure V2D
fig1 = closeLine $ Figure blue fiveV2 


-- fig2 =  toListOf figPath fig1    -- beachter reihenfolge! 

lines2picture :: [Figure V2D] -> Picture
lines2picture = pictures . map figure2picture' --   map onex   
--         -- 

-- showFacePage2 :: MonadIO m => [([V2D], Color)] -> m ()
showFacePage2 :: [Figure V2D] -> ExceptT Text IO ()
showFacePage2 lines = do
    putIOwords [ "Lib.showFacePage  here"]
    liftIO (do 
        -- display window background drawing
        display window2 white (Gloss.translate (-20) (-100) .Gloss.scale 40 40. lines2picture $ lines)
        )
    return ()

showFacePage :: ErrIO ()
showFacePage = showFacePage2 [fig1]

-----support stuff
width, height, offset :: Int
width = 300
height = 300
offset = 100

-- static for pont 
window2 :: Display
window2 = InWindow "few Lines" (width, height) (offset, offset)



-- to concentrate all the gloss stuff here 
    
class ToGloss a where 
    toGloss :: a -> Gloss.Point 
    fromGloss :: Gloss.Point  -> a 

instance ToGloss V2D where 
    toGloss (V2 x y) = (doubleToFloat x, doubleToFloat y)
    fromGloss (x,y) = V2 (floatToDouble x) (floatToDouble y) 
    
instance ToGloss (Pnt2) where 
    toGloss (Pnt2d i v2) = toGloss v2
