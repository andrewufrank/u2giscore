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

module HQschema.HQconstructions4graphics
    where

import           Test.Framework hiding (scale, (.&.))
import UniformBase  
import HQschema.HQexampleShort
import HQschema.HQschemaShort
-- import Control.Exception
import Uniform.GeometryFunctions
import Uniform.TripleRels
import Data.List.Extra
-- import Uniform.Drawings
import Control.Monad.State  
import HQschema.HQconstructionsFaces (evalTrans4query2cat)
import Control.Monad.Identity (Identity)


-- step 1 get the data from the store, leave tags!

-- | construct relation with all the edges of the hq triangles  
-- try groupSort 
hqTriangles :: State  CatStoreTessShort   [(ObjTessShort, [ObjTessShort])]
hqTriangles = do 
    f <- rel2 HqFace 
    n <- rel2 HqNode 
    xy <- rel2 XY 
    let fp3 =  xy ++ (f .&. xy) ++ (n .&. xy) 
    return $ groupSort  fp3  
    -- return $ map onef . groupSort $ fp3 

points2v2 = second (map  (unName . unPointTag))  
pointsPairsv2 = second (both  (unName . unPointTag))  

hqVoro :: StateT  CatStoreTessShort Identity  [(ObjTessShort, (ObjTessShort,ObjTessShort))]
hqVoro = do 
    f <- rel2 HqFace 
    -- n <- rel2 HqNode 
    xy <- rel2 XY 
    let fp3 =  relPair (f .&. xy) xy 
    return    fp3  

hqDela :: StateT  CatStoreTessShort Identity  [(ObjTessShort, (ObjTessShort,ObjTessShort))]
hqDela = do 
    -- f <- rel2 HqFace 
    n <- rel2 HqNode 
    xy <- rel2 XY 
    let fp3 =  relPair (n .&. xy) xy 
    return    fp3  
-- hqTriangles2 :: StateT CatStoreTessShort Identity [(ObjTessShort, [ObjTessShort])]
-- hqTriangles2 = do 
--     f <- rel2 HqFace 
--     n <- rel2 HqNode 
--     xy <- rel2 XY 
--     let fp3 =    (f .&. xy) 
--     return $ groupSort  fp3  

-- hqTriangles3 :: StateT CatStoreTessShort Identity [(ObjTessShort, [ObjTessShort])]
-- hqTriangles3 = do 
--     f <- rel2 HqFace 
--     n <- rel2 HqNode 
--     xy <- rel2 XY 
--     let fp3 =    (n .&. xy) 
--     return $ groupSort  fp3  


-- oints12 :: StateT
--   CatStoreTessShort
--   Identity
--   [(ObjTessShort, (ObjTessShort, ObjTessShort))]
-- points12 = do 
--     hqn <- rel2 HqNode 
--     xy <- rel2 XY 
--     twin <- rel2 Twin 
--     return (compRelZip (hqn .&. xy) (twin .&. hqn .&. xy))
-- -------------------- helpers 
