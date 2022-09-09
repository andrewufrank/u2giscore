 -----------------------------------------------------------------------------
--
-- Module      : geometric constructions 

--          adding 
--              HQ half_length and center 
--              circumcenter and incircle (for label placement)

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

module HQgeneric.HQconstructions4graphics
    where

import           Test.Framework hiding (scale, (.&.))
import UniformBase 
import Uniform.SchemaFoundation
    ( BinRels((.&.), rel2pair),
      Tup2,
      MorphsHQ(hqXY, hqFace, hqNode),
      ObjectsHQ(unPointTag),
      BinRels3Monadic(rel3),
      Store )
import Uniform.GeometryFunctions

-- import HQschema.HQexampleShort
-- import Country.Schema
-- import Country.Store 
-- import Control.Exception
import Data.List.Extra
-- import Uniform.Drawings
import Control.Monad.State  
-- import HQgeneric.HQconstructionsFaces (evalTrans4query2cat)
-- import Control.Monad.Identity (Identity)


-- step 1 get the data from the store, leave tags!

-- | construct relation with all the edges of the hq triangles  
-- try groupSort 
hqTriangles ::forall rel obj  . (MorphsHQ rel, Ord obj, Ord rel)  => State (Store obj rel)  [(obj, [obj])]
hqTriangles = do 
    f <- rel3 hqFace 
    n <- rel3 hqNode 
    xy <- rel3 hqXY 
    let fp3 =  xy ++ (f .&. xy) ++ (n .&. xy) 
    return $ groupSort  fp3  
    -- return $ map onef . groupSort $ fp3 

-- points2v2 = second (map  (unName . unPointTag))  
-- pointsPairsv2 = second (both  (unName . unPointTag))  

hqVoro ::forall rel obj  . (MorphsHQ rel, Ord obj, Ord rel)  => State (Store obj rel)      [(obj, (obj, obj))]
hqVoro = do 
    f <- rel3 hqFace 
    -- n <- rel3 HqNode 
    xy <- rel3 hqXY 
    let fp3 =  rel2pair (f .&. xy) xy 
    return    fp3  

hqDela  ::forall rel obj  . (MorphsHQ rel, Ord obj, Ord rel)  => State (Store obj rel)      [(obj, Tup2 (obj))]
hqDela = do 
    -- f <- rel3 HqFace 
    n <- rel3 hqNode 
    xy <- rel3 hqXY 
    let fp3 =  rel2pair (n .&. xy) xy 
    return    fp3  
-- hqTriangles2 :: StateT CatStoreTessShort Identity [(ObjTessShort, [ObjTessShort])]
-- hqTriangles2 = do 
--     f <- rel3 HqFace 
--     n <- rel3 HqNode 
--     xy <- rel3 XY 
--     let fp3 =    (f .&. xy) 
--     return $ groupSort  fp3  

-- hqTriangles3 :: StateT CatStoreTessShort Identity [(ObjTessShort, [ObjTessShort])]
-- hqTriangles3 = do 
--     f <- rel3 HqFace 
--     n <- rel3 HqNode 
--     xy <- rel3 XY 
--     let fp3 =    (n .&. xy) 
--     return $ groupSort  fp3  


-- oints12 :: StateT
--   CatStoreTessShort
--   Identity
--   [(ObjTessShort, (ObjTessShort, ObjTessShort))]
-- points12 = do 
--     hqn <- rel3 HqNode 
--     xy <- rel3 XY 
--     twin <- rel3 Twin 
--     return (compRelZip (hqn .&. xy) (twin .&. hqn .&. xy))
-- -------------------- helpers 
