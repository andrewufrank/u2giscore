-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Point2d
-- | Poind2d with ID and V2 for coordinates
--          and conversions 
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


module Uniform.Point2d 
    ( module Uniform.Point2d
    , module Linear.V2
    , module Control.Lens
    , P2, V2d
        )  where

import UniformBase
-- import Vector
import Linear.V2
import qualified Linear.Vector as Lin
import Control.Lens 
-- import GHC.Generics

import qualified Data.Geometry as H
import Data.Ext

-- import           Uniform.Strings hiding ((</>), (<.>), S)


instance Zeros Integer where zero = 0
instance Zeros Double where zero = 0.0
-----------------------
-- types specialized for Double
type P2 = P2d Integer Double
type List2 a = [a]
type V2d = V2 Double 

-- | a 2d point (constructed from V2 from Linear) with a name
data P2d i v = P2d {_p2id:: i, _v2:: V2 v}
    deriving (Show, Read, Ord, Eq, Generic)
instance (Zeros i, Zeros v, Num v) => Zeros (P2d i v) where zero = P2d zero zero 
instance (Zeros a, Num a) => Zeros (V2 a) where zero = Lin.zero 
makeLenses ''P2d

-------------- the conversion to the Hgeometry point
-- from P2 to H.Point
p2toHPoint :: P2d Integer Double -> H.Point 2 Double :+ Integer 
p2toHPoint (P2d i (V2 x y)) = H.Point2 x y :+ i

-- from P to V2 
p2toV2 :: P2d i v -> V2 v
p2toV2 p = p ^. v2 

v2toList2 :: V2 a -> [a]
v2toList2 (V2 x y) = [x,y]

-- from [Double] to V2 
fromList2V2 :: [Double] -> V2 Double
fromList2V2 [x,y] = V2 x y 

-- -- fromV2 to HPoint 
-- v2toHPoint :: V2 r -> H.Point 2 r
-- v2toHPoint (V2 x y) = H.Point2 x y 

-- -- | conversion to keep the name  
-- v2toHPoint' :: V2 r -> H.Point 2 r :+ ()
-- v2toHPoint' (V2 x y) = H.Point2 x y :+ () 

instance  (Show a)=> NiceStrings (V2 a) where 
    shownice (V2 x y) = "(" <> showT x <> "/" <> showT y <> ")"
--------------------------
-- | conversion to H.Point (without a point name)
-- needs instances for all Point types considered 
-- or conversion to V2 
class ToHPoint2 a where 
    toHPoint :: a -> H.Point 2 Double

instance ToHPoint2 (V2 Double) where 
    toHPoint (V2 x y) = H.Point2 x y 
    -- = v2toHPoint 
        -- | conversion to set the name  ()
-- v2toHPoint' :: V2 r -> H.Point 2 r :+ ()
-- v2toHPoint' (V2 x y) = H.Point2 x y :+ () 

-- instance ToHPoint2 (P2) where 
--     toHPoint = v2toHPoint  . p2toV2
-- instance ToHPoint2 [Double] where 
    -- not required, done via v2
--     toHPoint = v2toHPoint  . fromList2V2

-- creates undecidable instances 
instance (ToV2 (List2 Double)) => ToHPoint2 (List2 Double) where 
    toHPoint = toHPoint  . toV2


-- conversion to V2 
class ToV2 a where 
    toV2 :: a -> V2 Double 

instance ToV2 (List2 Double) where 
    toV2 = fromList2V2 
instance ToV2 (P2) where 
    toV2 = p2toV2 
