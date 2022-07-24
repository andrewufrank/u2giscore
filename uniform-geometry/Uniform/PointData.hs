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


{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.PointData 
    ( module Uniform.PointData
    , module Uniform.Point
    , module Linear.V2
    , module Control.Lens
        )  where

import UniformBase
import Uniform.Point
-- import Vector
import Linear.V2
import qualified Linear.Vector as Lin
import Control.Lens 
import GHC.Generics

-- import           Uniform.Strings hiding ((</>), (<.>), S)

-- fourP :: [PtTuple Int]
fourPoints :: [(Double, Double, Int)]
-- points to form two triangles
fourPoints = [(0,0,11), (1.5, 1.5, 12), (0,2,13), (2,0,14)]

fivePoints :: [(Double, Double, Int)]
-- to form 3 triangles
fivePoints = [(0,0,21), (3,0,22), (4,2,23), (3,5,24),(0,3,25)]

fourP2 = map tup2P2 fourPoints

tup2P2 :: (Double, Double, Int) -> P2 
tup2P2 (x,y,i)= Point2d i (V2 x y)
p2_tup_id :: P2 -> (Double, Double, Int)
p2_tup_id p7 = (p7 ^. v2._x, p7 ^. v2._y, p7 ^. p2id)
    -- with pattern matching simpler?
-- p2_tup_id (Point2d i (V2 x y)) = (x,y,i)

p2_tup :: P2 -> [Double]
p2_tup (Point2d i (V2 x y)) = [x, y]


fiveP2 :: [P2]
fiveP2 = map tup2P2 fivePoints 



-- data Point2d i v = Point2d {_p2id:: i, _v2:: V2 v}
--     deriving (Show, Read, Ord, Eq, Generic)
-- instance (Zeros i, Zeros v, Num v) => Zeros (Point2d i v) where zero = Point2d zero zero 
-- instance (Zeros a, Num a) => Zeros (V2 a) where zero = Lin.zero 
-- instance Zeros Double where zero = 0.0
-- makeLenses ''Point2d

-- type P2 = Point2d Int Double

