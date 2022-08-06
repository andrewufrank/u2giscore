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
    , fourV2, fiveV2
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

fourTup3coll :: [(Double, Double, Integer)]
-- points to form two triangles
-- three points are collinear (0,2), (1,1), (2,0)
fourTup3coll = [(0,0,11), (1.5, 1.5, 12), (0,2,13), (2,0,14)]

fiveTup3 :: [(Double, Double, Integer)]
-- to form 3 triangles
fiveTup3 = [(0,0,21), (2,0,22), (4,2,23), (3,5,24),(0,3,25)]

fiveTup3iso :: [(Double, Double, Integer)]
-- to form 3 triangles
-- form an isoceles triangle (0,0),(0,3), (3,0)
fiveTup3iso = [(0,0,21), (3,0,22), (4,2,23), (3,5,24),(0,3,25)]

fourPnv2d = map (ddn_pnv2d . third3 showT) fourTup3 :: [Pnv2]
-- for tests five points 
fourV2 :: [V2d]
fourV2 = map (pnv2d_v2 . ddn_pnv2d . third3 showT)  fourTup3
fourV2coll = map (pnv2d_v2 . ddn_pnv2d . third3 showT)  fourTup3coll

fivePnv2d :: [Pnv2]
fivePnv2d = map (ddn_pnv2d . third3 showT)  fiveTup3 

-- for tests five points 
fiveV2 :: [V2d]
fiveV2 = map pnv2d_v2 fivePnv2d
fiveV2iso = map (pnv2d_v2 . ddn_pnv2d . third3 showT) fiveTup3iso

-- fiveD = map (ddn_dd) fiveP2
fiveV2_31 = map ddn_v2 [(0,0,21), (3.1,0,22), (4,2,23), (3,5,24),(0,3,25)]
-- five less regular, no gleichschenklig dreieck 
-- fivemap :: Map.Map Integer [Double]-- 
-- fivemap = Map.fromList . fmap p2d_dd $ fiveP2


mainPoint2dData :: ErrIO () 
mainPoint2dData = do 
    putIOwords ["point2d zero", showT (zero::Pnv2)]
    putIOwords ["point2d fourPoint2d", showT (fourPnv2d)]
    putIOwords ["point2d fivePoint2d", showT (fivePnv2d)]