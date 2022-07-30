-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Point2d
-- | Poind2d with ID and V2 for coordinates
-- used for testing
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


module Uniform.Point2dData 
    ( module Uniform.Point2dData
    , module Uniform.Point2d
    , module Linear.V2
    , module Control.Lens
        )  where

import UniformBase
import Uniform.Point2d
import qualified Data.Map as Map 

-- import Vector
import Linear.V2
import qualified Linear.Vector as Lin
import Control.Lens 
-- import GHC.Generics

-- import           Uniform.Strings hiding ((</>), (<.>), S)


fourTup3 :: [(Double, Double, Integer)]
-- points to form two triangles
fourTup3 = [(0,0,11), (1.5, 1.5, 12), (0,2,13), (2,0,14)]

fivePoints :: [(Double, Double, Integer)]
-- to form 3 triangles
fivePoints = [(0,0,21), (3,0,22), (4,2,23), (3,5,24),(0,3,25)]

fourP2 = map tup2P2 fourTup3 :: [P2]

tup2P2 :: (Double, Double, Integer) -> P2 
tup2P2 (x,y,i)= P2d i (V2 x y)
p2_tup_id :: P2 -> (Double, Double, Integer)
p2_tup_id p7 = (p7 ^. v2._x, p7 ^. v2._y, p7 ^. p2id)
    -- with pattern matching simpler?
-- p2_tup_id (Point2d i (V2 x y)) = (x,y,i)

p2_tup :: P2 -> (Integer, [Double])
p2_tup (P2d i (V2 x y)) =(i, [x, y])


fiveP2 :: [P2]
fiveP2 = map tup2P2 fivePoints 

fiveD = map (snd . p2_tup) fiveP2

fivemap :: Map.Map Integer [Double]-- 
fivemap = Map.fromList . fmap p2_tup $ fiveP2


mainPoint2dData :: ErrIO () 
mainPoint2dData = do 
    putIOwords ["point2d zero", showT (zero::P2)]
    putIOwords ["point2d two", showT (fourP2)]
    putIOwords ["point2d two", showT (fourTup3)]