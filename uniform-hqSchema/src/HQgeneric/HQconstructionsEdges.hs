 -----------------------------------------------------------------------------
--
-- Module      : geometric constructions 

--          adding 
--              HQ half_length and center 
            --  circumcenter and incircle (for label placement)

-----------------------------------------------------------------------------
-- {-# OPTIONS_GHC -F -pgmF htfpp #-}
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

module HQgeneric.HQconstructionsEdges
    where

-- import           Test.Framework hiding (scale, (.&.))
import UniformBase 
import Uniform.SchemaFoundation

-- -- import HQschema.HQexampleShort
-- -- import HQschema.HQschemaShort
-- -- import Control.Exception
-- import Country.Schema
-- import Country.Store 
-- import Uniform.GeometryFunctions
-- import Uniform.Rels2
-- import Data.List.Extra
import Numeric.Extra
-- import Uniform.Drawings
import Control.Monad.State  

-- step 1 get the data from the store, leave tags!

-- points12 :: StateT CatStoreTessShort Identity (Rel2 ObjTessShort, Rel2 ObjTessShort)
points12 :: State 
  (Store ObjCountry MorphCountry)
  [(ObjCountry, Tup2 ObjCountry)]
-- points12 :: ghc-prim-0.7.0:GHC.Types.Any [(ObjCountry, Tup2 ObjCountry)]
points12 = do 
    hqn <- rel3 HqNode 
    xy <- rel3 XY 
    twin <- rel3 Twin 
    return (rel2pair (hqn .&. xy) (twin .&. hqn .&. xy))
-------------------- helpers 



 
-- for points12 
-- | distance from 2 tagged points 
-- dist2pts :: (ObjTessShort, ObjTessShort) -> Double
dist2pts :: (ObjCountry, ObjCountry) -> Double
dist2pts (p1,p2) = distance (unName . unPointTag $ p1) (unName . unPointTag $ p2)

-- | the coordinates of the halfway point in HQ 
-- midpoint :: (ObjTessShort, ObjTessShort) -> V2D
midpoint :: (ObjCountry, ObjCountry) -> V2D
midpoint (p1,p2) = (/2) . (uncurry (+)) . (cross . dup $ (unName . unPointTag)) $ (p1,p2)


-- lengthHQtriple :: (a, (ObjTessShort, ObjTessShort)) -> (a, MorphTessShort, ObjTessShort)
lengthHQtriple :: (a1, (ObjCountry, ObjCountry)) -> (MorphCountry, (a1, ObjCountry))
lengthHQtriple inp@(a,(p1,p2)) = reorg214  (a, Quant 1, LengthTag . Length $ (dist2pts (p1,p2))/2 )

-- midpointHQtriple :: (ObjTessShort, (ObjTessShort, ObjTessShort)) -> StoreTessShortElement
midpointHQtriple :: (ObjCountry, (ObjCountry, ObjCountry)) -> (MorphCountry, (ObjCountry, ObjCountry))
midpointHQtriple (a,(p1,p2)) = reorg214 (a, XY, PointTag . putName (unHalfQuad a) $ (midpoint (p1,p2)) )


-- step 3 build the specific function into state monad   xxx M 

-- for points12


-- distanceOfHQ :: State  CatStoreTessShort   [(ObjTessShort, Double)]
distanceOfHQ = fmap (map (second dist2pts)) points12


-- midpointHQ :: State  CatStoreTessShort   [(ObjTessShort, V2D)]
midpointHQ = fmap (map (second midpoint)) points12

-- lengthHQasTriple :: State 
--   CatStoreTessShort
   
--   [(ObjTessShort, MorphTessShort, ObjTessShort)]
lengthHQasTriple = fmap (map lengthHQtriple) points12

-- midpointHQasTriple :: State  CatStoreTessShort [StoreTessShortElement]
midpointHQasTriple = fmap (map midpointHQtriple) points12


