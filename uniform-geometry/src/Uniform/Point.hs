-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Point
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


module Uniform.Point 
    ( module Uniform.Point
    , module Linear.V2
    , module Control.Lens
    , P2
        )  where

import UniformBase
-- import Vector
import Linear.V2
import qualified Linear.Vector as Lin
import Control.Lens 
import GHC.Generics

import qualified Data.Geometry as H
import Data.Ext

-- import           Uniform.Strings hiding ((</>), (<.>), S)

-- | a 2d point (constructed from V2 from Linear) with a name
data Point2d i v = Point2d {_p2id:: i, _v2:: V2 v}
    deriving (Show, Read, Ord, Eq, Generic)
instance (Zeros i, Zeros v, Num v) => Zeros (Point2d i v) where zero = Point2d zero zero 
instance (Zeros a, Num a) => Zeros (V2 a) where zero = Lin.zero 
instance Zeros Double where zero = 0.0
makeLenses ''Point2d

type P2 = Point2d Integer Double

-------------- the conversion to the Hgeometry point
-- from P2 to H.Point
p2toHPoint :: Point2d Integer Double -> H.Point 2 Double :+ Integer 
p2toHPoint (Point2d i (V2 x y)) = H.Point2 x y :+ i

-- from P to V2 
p2toV2 p = p ^. v2 

-- from [Double] to V2 
fromList2V2 :: [Double] -> V2 Double
fromList2V2 [x,y] = V2 x y 

-- fromV2 to HPoint 
v2toHPoint (V2 x y) = H.Point2 x y  
v2toHPoint' (V2 x y) = H.Point2 x y :+ () 

