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


module Uniform.Drawings where
 
import UniformBase
import Uniform.Point2d 
import Uniform.Point2dData
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as A
-- import Vector
-- import Linear.V2
-- import qualified Linear.Vector as Lin
import Control.Lens 
-- -- import GHC.Generics

-- import qualified Data.Geometry as H
-- import Data.Ext ( type (:+)(..) )

-- import           Uniform.Strings hiding ((</>), (<.>), S)
-- the figure to show 
-- type GlossPoint = (Float, Float)

-- example with fiveV2 

fiveGloss = map toGloss fiveV2 
-- list2glossPoint [x,y]= (x,y)
-- points = [[0.0,0.0],[2.0,0.0],[1.5,1.5]]

closeline ls = ls ++ [head ls]
        -- where lend = last ls

-- gpts0 = map (scaleG 40) $ map list2glossPoint points 
gpts pts = Line . closeline $ pts

lineClosedColor (l,c) = color c . Line . closeline $ l 
-- scaleG s v@(x,y)= s A.* v -- (s*x, s*y)

justlines = pictures . map onex  $
        [ (fiveGloss, dark red)
        , (map toGloss fourV2, green)
        ]
onex (l,c) = color c . Line . closeline $ l 
-----------------------------in : 

-- drawing2 :: Picture
drawing2  =  translate (-20) (-100) . (scale 10 10)    
    -- $ map lineClosedColor lines
        -- [ (fiveGloss, dark red)
        -- -- , (map toGloss fourV2, green)
        -- ]
    --  color ballColor . gpts $ pts
    
  where
    ballColor = dark red 
    -- paddleColor = light (light blue) 



showFacePage :: ErrIO ()
showFacePage = do
    putIOwords [ "Lib.showFacePage  here"]
    liftIO (do 
        -- display window background drawing
        display window2 background (drawing2 justlines)
        )
    return ()

-----support stuff
width, height, offset :: Int
width = 300
height = 300
offset = 100

-- static for pont 
window2 :: Display
window2 = InWindow "few Lines" (width, height) (offset, offset)

background :: Color
background = white  -- black 