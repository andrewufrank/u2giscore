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
import ExampleData.HQschemaShort (ObjTessShort(HalfQuad))

-- | find the points with xy coord to a face
--   gives list of points, as [V2D] 
-- coords2faces :: (MonadState m) => CatStoreTessShort -> m [(IDtype, [V2D])]
-- coords2faces :: StateT CatStoreTessShort Identity [(IDtype, [V2D])]
coords2faces = do 
    f <- inv2 HqFace 
    n <- rel2 HqNode 
    xy <- rel2 XY 
    let fp3 =  (f .&. n .&. xy)
    return $ map onef . groupSort $ fp3 

onef (Face i, pts) = (Face i, map (unName . unPointTag) pts)


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

dist12one :: ((a1, b1) -> b2) -> (a2, (a1, b1)) -> (a2, b2)
dist12one f (a,(p1,p2)) = (a, f (p1,p2))
-- ^ a generic function to unpack and apply 
-- is essentially second 


-- | distance from 2 tagged points 
dist2pts :: (ObjTessShort, ObjTessShort) -> Double
dist2pts (p1,p2) = distance (unName . unPointTag $ p1) (unName . unPointTag $ p2)

-- | the coordinates of the halfway point in HQ 
midpoint :: (ObjTessShort, ObjTessShort) -> V2D
midpoint (p1,p2) = (/2) . (uncurry (+)) . (cross . dup $ (unName . unPointTag)) $ (p1,p2)

dup a = (a,a)
unHalfQuad (HalfQuad i) = i

area2faces :: [V2D] -> Double 
area2faces [a,b,c] = area3 a b c 
area2faces s = errorT ["not three points - not a triangle?", showT s]

area2facesM :: StateT CatStoreTessShort Identity [(ObjTessShort, Double)]
area2facesM  = fmap (map (second area2faces)) coords2faces 



distanceOfHQ :: StateT CatStoreTessShort Identity [(ObjTessShort, Double)]
distanceOfHQ = fmap (map (second dist2pts)) points12


midpointHQ :: StateT CatStoreTessShort Identity [(ObjTessShort, V2D)]
midpointHQ = fmap (map (dist12one midpoint)) points12

lengthHQtriple :: (a, (ObjTessShort, ObjTessShort)) -> (a, MorphTessShort, ObjTessShort)
lengthHQtriple inp@(a,(p1,p2)) = (a, Dist, LengthTag . Length $ (dist2pts (p1,p2))/2 )


lengthHQasTriple :: StateT
  CatStoreTessShort
  Identity
  [(ObjTessShort, MorphTessShort, ObjTessShort)]
lengthHQasTriple = fmap (map lengthHQtriple) points12


midpointHQtriple :: (ObjTessShort, (ObjTessShort, ObjTessShort)) -> (ObjTessShort, MorphTessShort, ObjTessShort)
midpointHQtriple (a,(p1,p2)) = (a, XY, PointTag . putName (unHalfQuad a) $ (midpoint (p1,p2))/2 )

midpointHQasTriple :: StateT
  CatStoreTessShort
  Identity
  [StoreTessShortElement]
midpointHQasTriple = fmap (map midpointHQtriple) points12

instance NiceStrings Float where shownice = showT 

