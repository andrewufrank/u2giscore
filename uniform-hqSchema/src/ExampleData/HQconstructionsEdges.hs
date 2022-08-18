 -----------------------------------------------------------------------------
--
-- Module      : geometric constructions 

--          adding 
--              HQ half_length and center 
            --  circumcenter and incircle (for label placement)

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

module ExampleData.HQconstructionsEdges
    where

import           Test.Framework hiding (scale, (.&.))
import UniformBase  
import ExampleData.HQexampleShort
import ExampleData.HQschemaShort
-- import Control.Exception
import Uniform.GeometryFunctions
import Uniform.TripleRels
import Data.List.Extra
import Numeric.Extra
-- import Uniform.Drawings
import Control.Monad.State  

-- step 1 get the data from the store, leave tags!

-- points12 :: StateT CatStoreTessShort Identity (Rel2 ObjTessShort, Rel2 ObjTessShort)
points12 :: StateT
  CatStoreTessShort
  Identity
  [(ObjTessShort, (ObjTessShort, ObjTessShort))]
points12 = do 
    hqn <- rel2 HqNode 
    xy <- rel2 XY 
    twin <- rel2 Twin 
    return (relPair (hqn .&. xy) (twin .&. hqn .&. xy))
-------------------- helpers 

dup a = (a,a)

 
-- for points12 
-- | distance from 2 tagged points 
dist2pts :: (ObjTessShort, ObjTessShort) -> Double
dist2pts (p1,p2) = distance (unName . unPointTag $ p1) (unName . unPointTag $ p2)

-- | the coordinates of the halfway point in HQ 
midpoint :: (ObjTessShort, ObjTessShort) -> V2D
midpoint (p1,p2) = (/2) . (uncurry (+)) . (cross . dup $ (unName . unPointTag)) $ (p1,p2)


lengthHQtriple :: (a, (ObjTessShort, ObjTessShort)) -> (a, MorphTessShort, ObjTessShort)
lengthHQtriple inp@(a,(p1,p2)) = (a, Quant 1, LengthTag . Length $ (dist2pts (p1,p2))/2 )

midpointHQtriple :: (ObjTessShort, (ObjTessShort, ObjTessShort)) -> (ObjTessShort, MorphTessShort, ObjTessShort)
midpointHQtriple (a,(p1,p2)) = (a, XY, PointTag . putName (unHalfQuad a) $ (midpoint (p1,p2)) )


-- step 3 build the specific function into state monad   xxx M 

-- for points12


distanceOfHQ :: StateT CatStoreTessShort Identity [(ObjTessShort, Double)]
distanceOfHQ = fmap (map (second dist2pts)) points12


midpointHQ :: StateT CatStoreTessShort Identity [(ObjTessShort, V2D)]
midpointHQ = fmap (map (second midpoint)) points12

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


