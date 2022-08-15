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
{-# LANGUAGE DeriveFunctor  #-}
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


module Uniform.Point2d 
    (
    Pnt2, Pnt2d(..), v2, p2id -- with the lenses
    , V2D (..), V2 (..)
    -- , Gloss.Point 
    , NamedPoint2 (..)
    , Point2 (..)
    , ToV2 (..)
    -- , ToGloss (..)  -- localized at Drawings
    , ToHPoint2 (..)  -- move to concentrat hgeometry
    -- , H.Point
    , module Uniform.Point2d
    -- , module Linear.V2
    -- , module Control.Lens
    , 
    -- , Point2d
    -- other conversions
    -- , ddn_pnt2d
        ) 
         where

import UniformBase
    -- ( Generic,
    --   NiceStrings(showNice),
    --   Zeros(zero),
    --   showT,
    --   CharChains(unwords') )
-- import Vector
import Linear.V2
import qualified Linear.Vector as Lin
-- import qualified  Graphics.Gloss as Gloss
-- import    Graphics.Gloss  
import Control.Lens 
-- -- import GHC.Generics
-- import GHC.Float   -- move somewhere and systematize
-- import Numeric.Extra 
import qualified Data.Geometry as H
import Data.Ext ( type (:+)(..) )

-- import           Uniform.Strings hiding ((</>), (<.>), S)


instance Zeros Integer where zero = 0
instance Zeros Double where zero = 0.0
instance NiceStrings V2D where 

class NamedPoint2 a where 
-- ^ class of 2d points (double) 
    getName :: a -> Int
    unName ::  a -> V2D 
    putName :: Int -> V2D -> a  

class Point2 a where   -- this is the start of a generic Point2 class 
    x, y :: a -> Double 

-----------------------  the two "uniform" prefered types Pnt2 and VD2 

-- | a named type (a name and a V2)
--  name restricted to Int to allow copy paste from output to code (without escaping "")
type Pnt2 = Pnt2d Int Double -- often pnt2d_   -- change later to better name
-- type List2 a = [a]      -- ofte dd_
type V2D = V2 Double  -- from linear V2  

instance Point2 V2D where 
    x (V2 x y) = x
    y (V2 x y) = y

-- type Pnt2int = Pnt2d Int Double
-- a special type for input in hgeometry delaunay 

-- | a 2d point (constructed from V2 from Linear) with a name
data Pnt2d i v = Pnt2d {_p2id:: i, _v2:: V2 v}
    deriving (Show, Read, Ord, Eq, Generic, Functor)
instance (Zeros i, Zeros v, Num v) => Zeros (Pnt2d i v) where zero = Pnt2d zero zero 

instance (Zeros a, Num a) => Zeros (V2 a) where zero = Lin.zero 
instance (Show i, Show v) => NiceStrings (Pnt2d i v) where
    showNice p =  unwords'   ["Pnt2d (", showT . _p2id $ p, " ", showT . _v2 $ p, ")"] 
    -- result must be readalbe as Haskell code! 

makeLenses ''Pnt2d  -- brings in scope the lenses (only later)

-- | apply functions to coords    
-- instance Functor (Pnt2d i) where 
--     fmap f (Pnt2d i v) = Pnt2d i (f v)
    -- fmap f = over v2 f

fmap' :: (V2 v -> V2 v1) -> Pnt2d i v -> Pnt2d i v1
fmap' f (Pnt2d i v) = Pnt2d i (f v)


instance NamedPoint2 Pnt2 where 
    getName = _p2id 
    unName a = a ^. v2 
    putName n v = Pnt2d n v 
instance Point2 Pnt2 where 
    x a= a ^. v2 . _x 
    y a = a ^. v2 . _y     

--------- H.Point  -- move to hgeometry dependent module
instance (Show a, Read a, NamedPoint2 (Pnt2d a Double)) => NamedPoint2 (H.Point 2 Double :+ a) where 
    getName = getName .  hpointToPnt2

    unName = unName . hpointToPnt2
    putName n v = p2_HPoint (putName n v)

instance (Show a, Read a, NamedPoint2 (Pnt2d a Double)
        , Point2 (Pnt2d a Double)) => Point2 (H.Point 2 Double :+ a) where 
    x = x . hpointToPnt2
    y = y . hpointToPnt2 


 -- do not use other than these conversions 
hpointToPnt2 (H.Point2 x y :+ i) = Pnt2d i (V2 x y)
hpointToPnt2zero (H.Point2 x y :+ ()) = Pnt2d zero (V2 x y)
-- -- from P2 to H.Point -- Hgeometry point
p2_HPoint :: Pnt2d a Double -> H.Point 2 Double :+ a 
p2_HPoint (Pnt2d i (V2 x y)) = H.Point2 x y :+ i

hpointToV2 (H.Point2 x y ) =   (V2 x y)

--------------V2 -- -- conversion to V2 
class ToV2 a where 
    toV2 :: a -> V2 Double 
    -- fromV2 :: V2 Double -> a 
instance ToV2 (V2 Double) where 
    toV2 = id 

instance ToV2 [Double] where 
    toV2 [x,y] = V2 x y 
 

instance ToV2 (Pnt2) where 
    toV2 p = (_v2 p)  -- works?

--------------------------
-- | conversion to H.Point (without a point name) -- required for calls to hgeometry
-- needs instances for all Point types considered 
-- or conversion to V2 
class ToHPoint2 a where 
    toHPoint :: a -> H.Point 2 Double  
    toHPoint' :: a -> H.Point 2 Double :+ () 
    -- toHPointWithID  :: a i d-> H.Point 2 Double :+ i
    -- toHPointText  :: a -> H.Point 2 Double :+ Text
    -- toHPointText = ext . toHPoint
    toHPointInt  :: a -> H.Point 2 Double :+ Int
    fromHPoint :: H.Point 2 Double -> a

instance ToHPoint2 (V2 Double) where 
    toHPoint (V2 x y) = H.Point2 x y
    toHPoint' (V2 x y) = H.Point2 x y :+ ()
    fromHPoint = hpointToV2  
 
instance ToHPoint2 (Pnt2) where 
    -- toHPointText (Pnt2d i (V2 x y)) =H.Point2 x y :+ ( i)
    -- toHPoint (Pnt2d i (V2 x y)) = H.Point2 x y :+ i
    toHPointInt (Pnt2d i (V2 x y)) = H.Point2 x y :+ i
                -- ((read . t2s $ i) :: Int)
    toHPoint (Pnt2d i (V2 x y)) = H.Point2 x y  
