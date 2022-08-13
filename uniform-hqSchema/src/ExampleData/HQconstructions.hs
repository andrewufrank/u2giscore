 -----------------------------------------------------------------------------
--
-- Module      : geometric constructions 

--          adding HQ half_length and center 
--              circumcenter and incircle (for label placement)

--

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards
    , UndecidableInstances     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -w #-}

module ExampleData.HQconstructions
    where

import           Test.Framework hiding (scale, (.&.))

import UniformBase  
import ExampleData.HQexampleShort
import ExampleData.HQschemaShort
-- import Control.Exception
import Uniform.GeometryFunctions
-- import Uniform.Point2d ()
-- import Uniform.Point2dData
-- import Uniform.TesselationHalfQuads
  
import Uniform.TripleRels
import Data.List.Extra
-- import Uniform.Drawings
import Control.Monad.State  


-- coords2faces :: (MonadState m) => CatStoreTessShort -> m [(IDtype, [V2D])]
coords2faces :: StateT CatStoreTessShort Identity [(IDtype, [V2D])]
coords2faces = do 
    f <- inv2 HqFace 
    n <- rel2 HqNode 
    xy <- rel2 XY 
    let fp3 =  (f .&. n .&. xy)
    return $ map onef . groupSort $ fp3 

onef (Face i, pts) = (i, map (unName . unPointTag) pts)

 
-- points12 :: StateT CatStoreTessShort Identity (Rel2 ObjTessShort, Rel2 ObjTessShort)
points12 :: StateT
  CatStoreTessShort
  Identity
  [(ObjTessShort, (ObjTessShort, ObjTessShort))]
points12 = do 
    hqn <- rel2 HqNode 
    xy <- rel2 XY 
    twin <- rel2 Twin 
    return (compRelZip (hqn .&. xy) (twin .&. hqn .&. xy))

-- dist12 :: [(ObjTessShort, Double)]
-- dist12 = map dist12one point1s

dist12one :: (ObjTessShort, (ObjTessShort, ObjTessShort)) -> (ObjTessShort, Double) 
dist12one (a,(p1,p2)) = (a, dist2pts (p1,p2))
-- | distance from 2 tagged points 
dist2pts (p1,p2) = distance (unName . unPointTag $ p1) (unName . unPointTag $ p2)

-- | the coordinates of the halfway point in HQ 
hqPoint (a,(p1,p2)) = (/2) . (uncurry (+)) . (cross . dup $ (unName . unPointTag)) $ (p1,p2)

dup a = (a,a)

-- | format as triple to store : length of HQ 
lengthHQ :: (ObjTessShort, (ObjTessShort, ObjTessShort)) -> StoreTessShortElement
lengthHQ inp@(a,(p1,p2)) = (a, Dist, LengthTag . Length $ (dist2pts (p1,p2))/2 )

distanceOfHQ = fmap (map dist12one) points12
lengthHQasTriple = fmap (map lengthHQ) points12
midpointHQ = fmap (map hqPoint) points12

instance NiceStrings Float where shownice = showT 

