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


-- step 1 get the data from the store, leave tags!

-- | find the points with xy coord to a face
--   gives list of points, as [V2D] 
-- coords2faces :: StateT CatStoreTessShort Identity [(ObjTessShort, [V2D])]
coords2faces :: StateT CatStoreTessShort Identity [(ObjTessShort, [ObjTessShort])]
coords2faces = do 
    f <- inv2 HqFace 
    n <- rel2 HqNode 
    xy <- rel2 XY 
    let fp3 =  (f .&. n .&. xy)
    return $ map onef . groupSort $ fp3  -- drop onef
    -- return $ map onef . groupSort $ fp3 

onef (Face i, pts) = (Face i,  pts)
-- onef (Face i, pts) = (Face i, map (unName . unPointTag) pts)


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

--- step 2: collect the geometry operations 
--  - with same signature as result from data retrieved 



-- | distance from 2 tagged points 
dist2pts :: (ObjTessShort, ObjTessShort) -> Double
dist2pts (p1,p2) = distance (unName . unPointTag $ p1) (unName . unPointTag $ p2)

-- | the coordinates of the halfway point in HQ 
midpoint :: (ObjTessShort, ObjTessShort) -> V2D
midpoint (p1,p2) = (/2) . (uncurry (+)) . (cross . dup $ (unName . unPointTag)) $ (p1,p2)


-------------------- helpers 

-- dist12one :: ((a1, b1) -> b2) -> (a2, (a1, b1)) -> (a2, b2)
-- dist12one f (a,(p1,p2)) = (a, f (p1,p2))
-- -- ^ a generic function to unpack and apply 
-- -- is essentially second - todo drop 

dup a = (a,a)
unHalfQuad (HalfQuad i) = i
unFace :: ObjTessShort -> IDtype
unFace (Face i) = i
-- isTriangle :: (ObjTessShort, [V2D]) -> Maybe (ObjTessShort, (V2D, V2D, V2D))
-- isTriangle (i, [a,b,c]) = Just (i,(a,b,c))
-- isTriangle _ = Nothing 

--- 

-- step 2 geometry for faces  
-- for points12 
lengthHQtriple :: (a, (ObjTessShort, ObjTessShort)) -> (a, MorphTessShort, ObjTessShort)
lengthHQtriple inp@(a,(p1,p2)) = (a, Quant 1, LengthTag . Length $ (dist2pts (p1,p2))/2 )





midpointHQtriple :: (ObjTessShort, (ObjTessShort, ObjTessShort)) -> (ObjTessShort, MorphTessShort, ObjTessShort)
midpointHQtriple (a,(p1,p2)) = (a, XY, PointTag . putName (unHalfQuad a) $ (midpoint (p1,p2))/2 )


-- unpack to [V2D]
unTagPoints2V = map (unName . unPointTag) 

-- isTriangle2 :: ( [V2D]) -> Maybe (  (V2D, V2D, V2D))
-- isTriangle2 ( [a,b,c]) = Just ( (a,b,c))
-- isTriangle2 _ = Nothing 

-- area2faces :: [V2D] -> Double 
area2faces vs = areaPoly . unTagPoints2V $ vs 
-- area2faces s = errorT ["not three points - not a triangle?", showT s]

-- circumCenter7 :: ToPD V2D => (ObjTessShort, (V2D, V2D, V2D)) -> (ObjTessShort, V2D)
-- circumCenter7 (i,(a,b,c)) = (i,circumCenter a b c)
-- circumCenter7x2 :: ToPD a => (a, a, a) -> a
-- circumCenter7x2 ( (a,b,c)) = ( circumCenter a b c)

-- inCenter7 :: (V2D, V2D, V2D) -> V2D
-- inCenter7   (a,b,c) = inCenter a b c 

-- circumCenter72 ( (a,b,c)) = (circumCenter a b c)
-- circumCenter7 :: (a,[V2D])-> Maybe (a,V2D) 
-- -- | calculate the circumcenter for a triangle, Nothing for other polygons
-- circumCenter7 (a,vs) = do 
--         (a,b,c) <- isTriangle vs
--         let r = circumCenter a b c
--         return (a,r)

-- circum72 :: Maybe (ObjTessShort,(V2D, V2D, V2D)) -> Maybe (ObjTessShort,V2D)
-- circum72 Nothing = Nothing 
-- circum72 (Just abbb) = Just $ circumCenter7 abbb
-- circum722 (Just bbb) = Just $ circumCenter7x2 bbb
-- circum722 Nothing = Nothing 


-- step 3 build the specific function into state monad   xxx M 

-- for points12

lengthHQasTriple :: StateT
  CatStoreTessShort
  Identity
  [(ObjTessShort, MorphTessShort, ObjTessShort)]
lengthHQasTriple = fmap (map lengthHQtriple) points12

midpointHQasTriple :: StateT
  CatStoreTessShort
  Identity
  [StoreTessShortElement]
midpointHQasTriple = fmap (map midpointHQtriple) points12

distanceOfHQ :: StateT CatStoreTessShort Identity [(ObjTessShort, Double)]
distanceOfHQ = fmap (map (second dist2pts)) points12


midpointHQ :: StateT CatStoreTessShort Identity [(ObjTessShort, V2D)]
midpointHQ = fmap (map (second midpoint)) points12

-- for coord2faces 

area2facesM :: StateT CatStoreTessShort Identity [(ObjTessShort, Double)]
area2facesM  = fmap (map (second area2faces)) coords2faces 

-- circum2facesM ::StateT CatStoreTessShort Identity [ (ObjTessShort, Maybe V2D)]
-- circum2facesM =  fmap (map (second (fmap circumCenter7x2 . isTriangle2)))    coords2faces

-- incenter2facesM = fmap (map (second (fmap inCenter7 . isTriangle2))) coords2faces

-- incenter2facesTriple :: (ObjTessShort, [V2D]) -> Maybe (ObjTessShort, MorphTessShort, ObjTessShort)
-- incenter2facesTriple (i,(vs)) = 
--         case isTriangle2 vs of 
--             Nothing  -> Nothing 
--             Just (a,b,c) -> Just (i, Incenter, (PointTag $ putName (unFace i)  (inCenter7 (a,b,c))))

-- circumcenter2facesTriple :: (ObjTessShort, [V2D]) -> Maybe (ObjTessShort, MorphTessShort, ObjTessShort)
-- circumcenter2facesTriple (i,(vs)) = 
--         case isTriangle2 vs of 
--             Nothing  -> Nothing 
--             Just (a,b,c) -> Just (i, XY, (PointTag $ putName (unFace i)  (circumCenter7x2 (a,b,c))))


-- x3 :: Maybe (a1, (b, b, b)) -> Maybe (a1, b)
-- x3 = maybe Nothing circumCenter7







instance NiceStrings Float where shownice = showT 

