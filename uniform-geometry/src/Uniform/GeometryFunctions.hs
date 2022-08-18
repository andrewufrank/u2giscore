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
    -- , module Control.Lens
    , Pnt2, V2
    , distance -- from Linear.Vector
        ) 
         where

import UniformBase
import Uniform.Point2d 
import ExampleData.Point2d
-- import Vector
import Linear.Vector ((*^))
import Linear.V2
-- import Linear.Vector 
import qualified Linear.Metric as Metric
import qualified Linear.Vector as Lin
import Control.Lens 
import GHC.Generics

import qualified Data.Geometry as H
import Data.Ext
import qualified Data.Geometry.Point as HP 
import qualified Data.Geometry.Polygon as Poly
import qualified Data.List.NonEmpty as NE 
import Data.PlaneGraph
import Data.Geometry.Triangle
import Data.Geometry.PlanarSubdivision
import Algorithms.Geometry.DelaunayTriangulation.Types
import Algorithms.Geometry.DelaunayTriangulation.Naive


-- from Hgeometry!

delaunay2 :: ToHPoint2 a => [a] -> Triangulation Int Double
delaunay2 pnt2s =  delaunayTriangulation (NE.fromList $  map toHPointInt  pnt2s)
-- -- ^ calling delaunay with a list of V2
    -- delaunayTriangulation :: (Ord r, Fractional r) => NonEmpty (Point 2 r :+ p) -> Triangulation p r 
-- maps to HPoint whatever the type of name 

planarSubdiv2 :: (Ord r, Fractional r) => Triangulation p r -> PlanarSubdivision s p () () r
planarSubdiv2 t = toPlanarSubdivision t

toPlaneGraph2 :: Triangulation p r -> PlaneGraph s p () () r
toPlaneGraph2 t = toPlaneGraph t

ccw_test :: (ToHPoint2 a1) => a1 -> a1 -> a1 -> Bool
-- | test for ccw, not include collinear case  

ccw_test a b c = HP.CCW == H.ccw (toHPoint a) (toHPoint b) (toHPoint c)

-- area3 :: a -> a -> a -> Double
area3 :: (ToHPoint2 a1) => a1 -> a1 -> a1 -> Double
area3 a b c =   area triangle1 
    where 
        triangle1 = Triangle (toHPoint' a) (toHPoint' b) (toHPoint' c)

-- areaPoly :: (ToHPoint2 a1) => [v2d] -> Double
-- | the area of a polygon, given as a sequence of points
--  some restrictions on the points (>= 3), no crossings
--  todo: I have the impression it does not give the sign correctly
-- see test_poly1
-- 
areaPoly vs =   Poly.signedArea poly 
    where 
        poly :: Poly.SimplePolygon () Double 
        poly = Poly.simpleFromPoints pts
        -- pts :: H.Point 2 Double Int 
        pts = map toHPoint'  $ vs 
-- scaleBy :: Num a => a -> V2 a -> V2 a   -- rather silly?
-- scaleBy a v = a * v

lineClose :: (Point2 a) =>  [a] -> [a]
-- close a line by adding the first point to the end 
-- restricted to Point2 
lineClose ls = ls ++ [head ls]


distance :: (ToV2 a) => a -> a -> Double
distance b c =  (Metric.distance (toV2 b) (toV2 c))

-- scale :: Double -> V2 Double  -> V2 Double 
-- scale s a = (s *^ a)

-- scale' :: Double -> Pnt2d Int Double  -> Pnt2d Int Double
scale :: (Functor f1, Functor f2, Num a) => a -> f1 (f2 a) -> f1 (f2 a)
scale s p = fmap (s *^) p 

circumCenter :: (ToPD a) => a -> a -> a -> a 
circumCenter a b c = fromPD 
        $ circumCenterPD (toPD a) (toPD b) (toPD c)


circumCenterPD :: PD -> PD -> PD -> PD 
circumCenterPD (ax, ay) (bx, by) (cx, cy)
            =  (((ay**2+ax**2)*(by-cy)+(by**2+bx**2)*(cy-ay)+(cy**2+cx**2)*(ay-by))/d,
                ((ay**2+ax**2)*(cx-bx)+(by**2+bx**2)*(ax-cx)+(cy**2+cx**2)*(bx-ax))/d)
        where d = 2*(ax*(by-cy)+bx*(cy-ay)+cx*(ay-by))

-- circumCenter :: Point -> Point -> Point -> Point
-- > circumCenter (ax, ay) (bx, by) (cx, cy)
-- >     =  (((ay**2+ax**2)*(by-cy)+(by**2+bx**2)*(cy-ay)+(cy**2+cx**2)*(ay-by))/d,
-- >        ((ay**2+ax**2)*(cx-bx)+(by**2+bx**2)*(ax-cx)+(cy**2+cx**2)*(bx-ax))/d)
-- >        where d = 2*(ax*(by-cy)+bx*(cy-ay)+cx*(ay-by))

-- incenter :: (ToV2 a) => a -> a -> a -> a
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

--------------- auxiliary conversions locally used from V2D
type PD = (Double,Double)
class ToPD a where
    toPD :: a -> PD
    fromPD :: PD -> a

instance ToPD V2D where
    toPD (V2 x y) = (x,y)
    fromPD (x,y) = V2 x y 

a33 :: V2 Double
a33 = V2 3 6 
t0 = distance a33 a33