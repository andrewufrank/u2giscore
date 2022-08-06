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
    , module Control.Lens
    , Pnt2, V2
        ) 
         where

import UniformBase
import Uniform.Point2d 
-- import Vector
import Linear.V2
import Linear.Vector 
import qualified Linear.Vector as Lin
import Control.Lens 
import GHC.Generics

import qualified Data.Geometry as H
import Data.Ext
import qualified Data.Geometry.Point as HP 
import qualified Data.List.NonEmpty as NE 
import Algorithms.Geometry.DelaunayTriangulation.Types
import Algorithms.Geometry.DelaunayTriangulation.Naive

delaunay2 v2s =  delaunayTriangulation (NE.fromList . map ext . map toHPoint $ v2s)
-- -- ^ calling delaunay with a list of V2
    -- delaunayTriangulation :: (Ord r, Fractional r) => NonEmpty (Point 2 r :+ p) -> Triangulation p r 
planarSubdiv2  = toPlanarSubdivision
-- voronoi2d tess = voronoi2 tess
-- -- fourV2 = map _v2   fourPnv2d 

planeGraph2 = toPlaneGraph

ccw_test :: (ToHPoint2 a1, ToHPoint2 a2, ToHPoint2 a3) => a1 -> a2 -> a3 -> Bool
-- | test for ccw, not include collinear case  
-- collinear case causes problems when extracting the faces in 
    -- constructing the HalfQuads
ccw_test a b c = HP.CW /= H.ccw (toHPoint a) (toHPoint b) (toHPoint c)

scale :: Num a => a -> V2 a -> V2 a 
scale = (*^)
-- scale s (V2 x y) = (V2 (s*x) (s*y))

a33 :: V2 Double
a33 = V2 3 6 