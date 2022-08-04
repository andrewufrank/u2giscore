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
    , Point2d
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

ccw_test :: (ToHPoint2 a1, ToHPoint2 a2, ToHPoint2 a3) => a1 -> a2 -> a3 -> Bool
ccw_test a b c = HP.CCW == H.ccw (toHPoint a) (toHPoint b) (toHPoint c)

scale :: Num a => a -> V2 a -> V2 a 
scale = (*^)
-- scale s (V2 x y) = (V2 (s*x) (s*y))

a33 :: V2 Double
a33 = V2 3 6 