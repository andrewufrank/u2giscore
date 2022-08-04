-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Point2d
-- | Poind2d with ID and V2 for coordinates and plain V2 
-- conversion to other formats as far as necessary 
-- done with classes 
--          and conversions general conversions
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
    , Pnv2, V2d
    -- , Point2d
    -- other conversions
    , ddn_pnv2d
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

-----------------------  the two "uniform" types
-- types specialized for Double
type Pnv2 = Pnv2d Text Double -- often pnv2d_   -- change later to better name
-- type List2 a = [a]      -- ofte dd_
type V2d = V2 Double  -- from linear V2



-- | a 2d point (constructed from V2 from Linear) with a name
data Pnv2d i v = Pnv2d {_p2id:: i, _v2:: V2 v}
    deriving (Show, Read, Ord, Eq, Generic)
instance (Zeros i, Zeros v, Num v) => Zeros (Pnv2d i v) where zero = Pnv2d zero zero 

instance (Zeros a, Num a) => Zeros (V2 a) where zero = Lin.zero 
makeLenses ''Pnv2d

-- from P2d to V2 - drops the name, no inverse
pnv2d_v2 :: Pnv2d i v -> V2 v
pnv2d_v2 p = p ^. v2 


-------------- the conversions

-- from P2 to H.Point -- Hgeometry point
p2_HPoint :: Pnv2d Text Double -> H.Point 2 Double :+ Text 
p2_HPoint (Pnv2d i (V2 x y)) = H.Point2 x y :+ i

-- data Point2d = Point2d Double Double 
--     deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- -- | used in Schema, the data type to represent 2 coordinate pairs 
-- instance NiceStrings Point2d



v2_dd :: V2 a -> [a]
v2_dd (V2 x y) = [x,y]

pnv2d_dd= v2_dd . pnv2d_v2  -- drops name
-- pnv2d_point2d= v2_point2d . pnv2d_v2  -- drops name

-- from [Double] to V2 
dd_v2 :: [Double] -> V2 Double
dd_v2 [x,y] = V2 x y 

ddn_pnv2d (x,y,i)= Pnv2d i (V2 x y)
pnv2d_ddn (Pnv2d i (V2 x y) )= (x,y,i)
-- pnv2d_dd (Pnv2d i (V2 x y) )= [x,y]


-- v2_point2d :: V2d -> Point2d 
-- v2_point2d (V2 x y) = Point2d x y 


-- point2d_v2 :: Point2d -> V2d 
-- point2d_v2 (Point2d x y) = V2 x y 

-- -- fromV2 to HPoint 
-- v2toHPoint :: V2 r -> H.Point 2 r
-- v2toHPoint (V2 x y) = H.Point2 x y 

-- -- | conversion to keep the name  
-- v2toHPoint' :: V2 r -> H.Point 2 r :+ ()
-- v2toHPoint' (V2 x y) = H.Point2 x y :+ () 

instance  (Show a)=> NiceStrings (V2 a) where 
    shownice (V2 x y) = "(" <> showT x <> "/" <> showT y <> ")"
--------------------------
-- | conversion to H.Point (without a point name) -- required for calls to hgeometry
-- needs instances for all Point types considered 
-- or conversion to V2 
class ToHPoint2 a where 
    toHPoint :: a -> H.Point 2 Double  
    toHPointTextName :: a -> H.Point 2 Double :+ Text

instance ToHPoint2 (V2 Double) where 
    toHPoint (V2 x y) = H.Point2 x y 
--     -- = v2toHPoint 
--         -- | conversion to set the name  ()
-- -- v2toHPoint' :: V2 r -> H.Point 2 r :+ ()
-- -- v2toHPoint' (V2 x y) = H.Point2 x y :+ () 

instance ToHPoint2 (Pnv2) where 
    toHPointTextName = p2_HPoint
    toHPoint = toHPoint . pnv2d_v2 
    
instance ToHPoint2 [Double] where 
    toHPoint = toHPoint . dd_v2
    
    -- toHPoint (Pnv2d i (V2 x y))= H.Point2 x y :+ i

-- instance ToHPoint2 V2 where 
--     -- not required, done via v2
--     toHPoint = toHPoint . dd_v2

-- -- creates undecidable instances 
-- instance (ToV2 (List2 Double)) => ToHPoint2 (List2 Double) where 
--     toHPoint = toHPoint  . toV2


-- conversion to V2 
class ToV2 a where 
    toV2 :: a -> V2 Double 

instance ToV2 [Double] where 
    toV2 = dd_v2
instance ToV2 (Pnv2) where 
    toV2 p = (_v2 p)
-- conversion to [Double] for hgeometry 
class ToDD a where 
    todd :: a -> [Double] 

instance ToDD V2d where 
    todd = v2_dd
instance ToDD (Pnv2) where 
    todd = pnv2d_dd

-- conversion to (x,y)for gloss 
class ToGloss a where 
    togloss :: a -> (Double,Double)

instance ToGloss V2d where 
    togloss (V2 x y) = (x,y)
instance ToGloss (Pnv2) where 
    togloss (Pnv2d i (V2 x y)) = (x,y)
