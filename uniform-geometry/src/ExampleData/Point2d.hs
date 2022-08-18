-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Point2d
-- | Poind2d with ID and V2 for coordinates
-- used for testing
-----------------------------------------------------------------------------
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


{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}


module ExampleData.Point2d 
    ( module ExampleData.Point2d
    , module Uniform.Point2d
    -- , module Linear.V2
    -- , module Control.Lens
    -- , fourV2, fiveV2
        )  where

import UniformBase
import Uniform.Point2d

-- import qualified Data.Map as Map 

-- -- import Vector
-- import Linear.V2
-- import qualified Linear.Vector as Lin
-- import Control.Lens 
-- import GHC.Generics

-- import           Uniform.Strings hiding ((</>), (<.>), S)

-- purely local format ddn, do not export!

type DDN = (Double, Double, Integer)
ddn_pnt2d ::   (Double, Double, Integer) -> Pnt2
ddn_pnt2d (x,y,i)= Pnt2d (fromInteger i) (V2 x y)
ddn_v2 :: DDN -> V2D
ddn_v2 (x,y,i)=  (V2 x y)
-- pnt2d_ddn :: Integral a => Pnt2d a b -> (b, b, Integer)
-- pnt2d_ddn (Pnt2d i (V2 x y) )= (x,y, toInteger i)

-- local def of test points 
fourTup3 :: [(Double, Double, Integer)]
-- points to form two triangles
fourTup3 = [(0,0,11), (1.5, 1.5, 12), (0,2,13), (2,0,14)]

fourTup3coll :: [(Double, Double, Integer)]
-- points to form two triangles
-- three points are collinear (0,2), (1,1), (2,0)
fourTup3coll = [(0,0,11), (1.5, 1.5, 12), (0,2,13), (2,0,14)]

fiveTup3 :: [(Double, Double, Integer)]
-- to form 3 triangles
fiveTup3 = [(0,0,21), (2,0,22), (4,2,23), (3,5,24),(0,3,25)]

fiveTup3iso :: [(Double, Double, Integer)]
-- to form 3 triangles
-- form an isoceles triangle (0,0),(0,3), (3,0)
fiveTup3iso = [(0,0,21), (3,0,22), (4,2,23), (3,5,24),(0,3,25)]

fourteenTup3 = [(7,0,1), (12,0,2),(18,4,3), (20,10,4), (19,14,5), (14,19,6), (9,20,7), (4,17,8), (0,10,9),(3,3,10), (8,8,11), (14,6,12), (13,11,13), (8,12,14) ]
-- conversion to Pnt2 -- i.e. Int Double 
fourPnt2d = map (ddn_pnt2d  ) fourTup3 :: [Pnt2]
-- -- fourPnt2dInt = map (ddn_pnt2d . third3 fromInteger) fourTup3 :: [Pnt2int]
-- -- for tests five points with names

fourV2 :: [V2D]
fourV2 = map (toV2 . ddn_pnt2d )  fourTup3
fourV2coll = map (toV2 . ddn_pnt2d)  fourTup3coll

fivePnt2d :: [Pnt2]
fivePnt2d = map (ddn_pnt2d)  fiveTup3 
fourteenPnt2d = map (ddn_pnt2d)  fourteenTup3 
-- fivePnt2dint = map (ddn_pnt2d . third3 fromInteger)  fiveTup3 
-- -- | test points with names

-- -- for tests five points 
fiveV2 :: [V2D]
fiveV2 = map toV2 fivePnt2d
fiveV2iso = map (toV2 . ddn_pnt2d) fiveTup3iso
fourteenV2 = map (toV2 . ddn_pnt2d) fourteenTup3
-- -- fiveD = map (ddn_dd) fiveP2
fiveV2_31 = map ddn_v2 [(0,0,21), (3.1,0,22), (4,2,23), (3,5,24),(0,3,25)]
-- -- five less regular, no gleichschenklig dreieck 
-- -- fivemap :: Map.Map Integer [Double]-- 
-- -- fivemap = Map.fromList . fmap p2d_dd $ fiveP2


-- mainPoint2dData :: ErrIO () 
-- mainPoint2dData = do 
--     putIOwords ["point2d zero", showT (zero::Pnt2)]
--     putIOwords ["point2d fourPoint2d", showT (fourPnt2d)]
--     putIOwords ["point2d fivePoint2d", showT (fivePnt2d)]