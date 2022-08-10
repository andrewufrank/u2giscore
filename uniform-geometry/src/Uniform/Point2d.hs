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


module Uniform.Point2d 
    (
    Pnt2, Pnt2d(..), v2, p2id -- with the lenses
    , V2D (..), V2 (..)
    , GlossPoint
    , NamedPoint2 (..)
    , Point2 (..)
    , ToV2 (..)
    , ToGloss (..)
    , ToHPoint2 (..)
    -- , H.Point
    -- , module Uniform.Point2d
    -- , module Linear.V2
    -- , module Control.Lens
    , 
    -- , Point2d
    -- other conversions
    -- , ddn_pnt2d
        )  where

import UniformBase
    ( Generic,
      NiceStrings(showNice),
      Zeros(zero),
      showT,
      CharChains(unwords') )
-- import Vector
import Linear.V2
import qualified Linear.Vector as Lin
import Control.Lens 
-- -- import GHC.Generics
-- import GHC.Float   -- move somewhere and systematize
import Numeric.Extra 
import qualified Data.Geometry as H
import Data.Ext ( type (:+)(..) )

-- import           Uniform.Strings hiding ((</>), (<.>), S)


instance Zeros Integer where zero = 0
instance Zeros Double where zero = 0.0

-----------------------  the two "uniform" prefered types
class NamedPoint2 a where 
-- | class of 2d points (double) 
    name :: a -> Int
    unName ::  a -> V2D 
    putName :: Int -> V2D -> a  

class Point2 a where 
    x, y :: a -> Double 


-- | a named type (a name and a V2)
type Pnt2 = Pnt2d Int Double -- often pnt2d_   -- change later to better name
-- type List2 a = [a]      -- ofte dd_
type V2D = V2 Double  -- from linear V2  

-- type Pnt2int = Pnt2d Int Double
-- a special type for input in hgeometry delaunay 

-- | a 2d point (constructed from V2 from Linear) with a name
data Pnt2d i v = Pnt2d {_p2id:: i, _v2:: V2 v}
    deriving (Show, Read, Ord, Eq, Generic)
instance (Zeros i, Zeros v, Num v) => Zeros (Pnt2d i v) where zero = Pnt2d zero zero 

instance (Zeros a, Num a) => Zeros (V2 a) where zero = Lin.zero 
instance (Show i, Show v) => NiceStrings (Pnt2d i v) where
    showNice p =  unwords'   ["Pnt2d (", showT . _p2id $ p, " ", showT . _v2 $ p, ")"] 
    -- result must be readalbe as Haskell code

makeLenses ''Pnt2d  -- brings in scope the lenses (only later)

instance NamedPoint2 Pnt2 where 
    name = _p2id 
    unName a = a ^. v2 
    putName n v = Pnt2d n v 
instance Point2 Pnt2 where 
    x a= a ^. v2 . _x 
    y a = a ^. v2 . _y     

--------- H.Point
instance (Show a, Read a, NamedPoint2 (Pnt2d a Double)) => NamedPoint2 (H.Point 2 Double :+ a) where 
    name = name .  hpointToPnt2

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
                -- = dd_v2
-- -- from [Double] to V2 
-- dd_v2 :: [Double] -> V2 Double
-- dd_v2 [x,y] = V2 x y 

instance ToV2 (Pnt2) where 
    toV2 p = (_v2 p)  -- works?

-- -- from P2d to V2 - drops the name, no inverse
-- pnt2d_v2 :: Pnt2d i v -> V2 v
-- pnt2d_v2 p = p ^. v2 



-- -------------- the conversions
type GlossPoint = (Float,Float)



-- v2_dd :: V2 a -> [a]
-- v2_dd (V2 x y) = [x,y]

-- pnt2d_dd= v2_dd . pnt2d_v2  -- drops name
-- -- pnt2d_point2d= v2_point2d . pnt2d_v2  -- drops name





-- instance  (Show a)=> NiceStrings (V2 a) where 
--     shownice (V2 x y) = "(" <> showT x <> "/" <> showT y <> ")"

--------------------------
-- | conversion to H.Point (without a point name) -- required for calls to hgeometry
-- needs instances for all Point types considered 
-- or conversion to V2 
class ToHPoint2 a where 
    toHPoint :: a -> H.Point 2 Double  
    -- toHPointWithID  :: a i d-> H.Point 2 Double :+ i
    -- toHPointText  :: a -> H.Point 2 Double :+ Text
    -- toHPointText = ext . toHPoint
    toHPointInt  :: a -> H.Point 2 Double :+ Int

instance ToHPoint2 (V2 Double) where 
    toHPoint (V2 x y) = H.Point2 x y 
 
instance ToHPoint2 (Pnt2) where 
    -- toHPointText (Pnt2d i (V2 x y)) =H.Point2 x y :+ ( i)
    toHPointInt (Pnt2d i (V2 x y)) = H.Point2 x y :+ i
                -- ((read . t2s $ i) :: Int)
    toHPoint (Pnt2d i (V2 x y)) = H.Point2 x y  

-- -- instance ToHPoint2 (Pnt2int) where 
-- --     toHPointInt  = p2_HPoint
-- --     toHPoint = toHPoint . pnt2d_v2  -- cancels number 
    
-- instance ToHPoint2 [Double] where 
--     toHPoint = toHPoint . dd_v2

        

-- -- conversion to [Double] for hgeometry 
-- class ToDD a where 
--     todd :: a -> [Double] 

-- instance ToDD V2D where 
--     todd = v2_dd
-- instance ToDD (Pnt2) where 
--     todd = pnt2d_dd

-- conversion to (x,y)for gloss 
class ToGloss a where 
    toGloss :: a -> GlossPoint
    fromGloss :: GlossPoint -> a 

instance ToGloss V2D where 
    toGloss (V2 x y) = (doubleToFloat x, doubleToFloat y)
    fromGloss (x,y) = V2 (floatToDouble x) (floatToDouble y) 
    
-- instance ToGloss (Pnt2) where 
--     toGloss (Pnt2d i (V2 x y)) = (x,y)

-- -- conversion to Pnt2
-- class ToPnt2 a where 
--     toPnt2 :: a -> Pnt2
-- -- instance ToPnt2 (H.Point2 Double Int) where
-- -- hpointToPnt2 (H.Point2 x y :+ i) = Pnt2d i (V2 x y)

-- instance ToPnt2 GlossPoint where 
--     toPnt2 (x,y) = Pnt2d zero (V2 x y)  
-- instance ToPnt2 (V2D) where 
--     toPnt2  v  = Pnt2d zero v