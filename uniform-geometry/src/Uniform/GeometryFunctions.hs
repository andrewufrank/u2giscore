-----------------------------------------------------------------------------
--
-- Module      :  Uniform.GeometryFunctions
-- | Poind2d with ID and V2 for coordinates
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
-- {-# LANGUAGE TypeApplications     #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.GeometryFunctions
    ( module Uniform.GeometryFunctions
    , module Uniform.Point2d
    , module Linear.V2
    -- , module Linear.Metric
    , module Control.Lens
    , Pnt2, V2
        ) 
         where

import UniformBase
import Uniform.Point2d 
import Uniform.Point2dData
-- import Vector
import Linear.V2
import Linear.Vector 
import qualified Linear.Metric as Metric
import qualified Linear.Vector as Lin
import Control.Lens 
import GHC.Generics

import qualified Data.Geometry as H
import Data.Ext
import qualified Data.Geometry.Point as HP 
import qualified Data.List.NonEmpty as NE 
import Algorithms.Geometry.DelaunayTriangulation.Types
import Algorithms.Geometry.DelaunayTriangulation.Naive
import qualified Graphics.Gloss as Gloss

delaunay2 pnt2s =  delaunayTriangulation (NE.fromList $  map toHPointInt  pnt2s)
-- maps to HPoint whatever the type of name 
-- -- ^ calling delaunay with a list of V2
    -- delaunayTriangulation :: (Ord r, Fractional r) => NonEmpty (Point 2 r :+ p) -> Triangulation p r 
planarSubdiv2  = toPlanarSubdivision
-- voronoi2d tess = voronoi2 tess
-- -- fourV2 = map _v2   fourPnt2d 

planeGraph2 = toPlaneGraph

ccw_test :: (ToHPoint2 a1, ToHPoint2 a2, ToHPoint2 a3) => a1 -> a2 -> a3 -> Bool
-- | test for ccw, not include collinear case  
-- collinear case causes problems when extracting the faces in 
    -- constructing the HalfQuads
ccw_test a b c = HP.CW /= H.ccw (toHPoint a) (toHPoint b) (toHPoint c)

scale :: Num a => a -> V2 a -> V2 a 
scale = (*^)

distance :: (ToV2 a) => a -> a -> Double
distance b c =  (Metric.distance (toV2 b) (toV2 c))

-- scale s (V2 x y) = (V2 (s*x) (s*y))

circumCenter :: (ToGloss a) => a -> a -> a -> a 
circumCenter a b c = fromGloss 
        $ circumCenterGloss (toGloss a) (toGloss b) (toGloss c)

circumCenterGloss :: Gloss.Point  -> Gloss.Point  -> Gloss.Point  -> Gloss.Point 
circumCenterGloss (ax, ay) (bx, by) (cx, cy)
            =  (((ay**2+ax**2)*(by-cy)+(by**2+bx**2)*(cy-ay)+(cy**2+cx**2)*(ay-by))/d,
                ((ay**2+ax**2)*(cx-bx)+(by**2+bx**2)*(ax-cx)+(cy**2+cx**2)*(bx-ax))/d)
        where d = 2*(ax*(by-cy)+bx*(cy-ay)+cx*(ay-by))

-- circumCenter :: Point -> Point -> Point -> Point
-- > circumCenter (ax, ay) (bx, by) (cx, cy)
-- >     =  (((ay**2+ax**2)*(by-cy)+(by**2+bx**2)*(cy-ay)+(cy**2+cx**2)*(ay-by))/d,
-- >        ((ay**2+ax**2)*(cx-bx)+(by**2+bx**2)*(ax-cx)+(cy**2+cx**2)*(bx-ax))/d)
-- >        where d = 2*(ax*(by-cy)+bx*(cy-ay)+cx*(ay-by))

-- incenter :: (ToV2 a) => a -> a -> a -> V2 Double 
incenter v1 v2 v3 = V2 ((a * v1 ^. _x + b * v2 ^._x   + c * v3 ^._x) / abc)
                        ((a * v1 ^._y  + b * v2 ^._y + c * v3 ^._y) / abc)
    where 
        a = distance v2 v3 
        b = distance v3 v1 
        c = distance v1 v2 
        abc = a + b + c 

     
-- incenter:
-- float x1 = 2, x2 = 1, x3 = 3;
--     float y1 = 2, y2 = 1, y3 = 1;
--     float a = 2, b = 1, c = 1;
 
--     // Formula to calculate in-center
--     float x = (a * x1 + b *
--                    x2 + c * x3) / (a + b + c);
--     float y = (a * y1 + b *
--                    y2 + c * y3) / (a + b + c);

a33 :: V2 Double
a33 = V2 3 6 
t0 = distance a33 a33