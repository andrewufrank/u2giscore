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

-- example with fiveV2 
-- fiveGloss = map toGloss fiveV2 
-- list2glossPoint [x,y]= (x,y)
-- points = [[0.0,0.0],[2.0,0.0],[1.5,1.5]]


--         -- where lend = last ls

-- -- gpts0 = map (scaleG 40) $ map list2glossPoint points 
-- gpts pts = Line . closeline $ pts

class DrawPolygon a where 
    closeLine :: a -> a 

data Figure a = Figure 
    { _figColor :: Color
    , _figPath :: [a]
    } deriving (Show,    Eq, Generic, Zeros)

-- instance (Zeros a) => Zeros (Figure a) where 
--     zero = Figure zero zero

instance Zeros (Color) where zero = greyN 0.5

makeLenses ''Figure 

instance DrawPolygon (Figure V2D) where 
    closeLine = over figPath  lineClose   


figure2picture' :: ToGloss a => Figure a -> Picture
figure2picture' f =  color (f ^. figColor) $ Line .  map toGloss $ view figPath f
-- figure2picture' f =  color (f ^. figColor) $ Line .  map toGloss $ view figPath f

t33 :: [[V2D]]
t33 = toListOf figPath fig1
-- figure2picture (Figure c p) = color c . Line . map toGloss $ p 

-- map2figPath f a = a . traversed . f

fig1 :: Figure V2D
fig1 = closeLine $ Figure blue fiveV2 


-- fig2 =  toListOf figPath fig1    -- beachter reihenfolge! 

-- fig2' = fig1 {_figPath = map (10*) $ _figPath fig1}

-- scaleFig s = figPath . traversed . (s *)
-- lineClosedColor (l,c) = color c . Line . scloseline $ l 
-- scaleG s v@(x,y)= s A.* v -- (s*x, s*y)

lines2picture :: [Figure V2D] -> Picture
lines2picture = pictures . map figure2picture' --   map onex   
--         -- 
-- onex (l,c) = color c . Line .  map toGloss . closeLine . scaleFig 10  $ l 
-----------------------------in : 

-- drawing2 :: Picture
-- drawing2  =  translate (-20) (-100) . (Gloss.scale 10 10)    
    -- $ map lineClosedColor lines
        -- [ (fiveGloss, dark red)
        -- -- , (map toGloss fourV2, green)
        -- ]
    --  color ballColor . gpts $ pts
    
--   where
--     ballColor = dark red 
--     -- paddleColor = light (light blue) 

-- exampleLines :: [([V2D], Color)]
-- exampleLines = [ (fiveV2, dark red)
--         -- , (map toGloss fourV2, green)
--         ]

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
