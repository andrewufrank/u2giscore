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

module HQgeneric.ConstructionsFaces
    where

import           Test.Framework hiding (scale, (.&.))
import UniformBase 
import Uniform.SchemaFoundation
-- import ExampleHQ.SchemaInstances

-- import Uniform.TripleStore
-- import Country.Schema
-- import Country.Store 
-- import HQschema.HQexampleShort
-- import HQschema.HQschemaShort
-- import Control.Exception
import Uniform.GeometryFunctions
-- import Uniform.Rels2
import Data.List.Extra
-- import Uniform.Drawings
import Control.Monad.State  
-- import Data.Functor.Identity
-- import Control.Monad.Trans.Identity

-- step 1 get the data from the store, leave tags!

-- | find the points with xy coord to a face
--   gives list of points, as [V2D] 
-- coords2faces :: StateT CatStoreTessShort Identity [(ObjTessShort, [ObjTessShort])]
-- coords2faces :: m [(k, [k])]
coords2faces  :: forall rel obj . (MorphsHQ rel, Eq rel, Eq obj,   Ord obj)  => State (Store obj rel)  [(obj, [obj])]
coords2faces = do 
    f <- inv3 (hqFace :: rel)
    n <- rel3 hqNode 
    xy <- rel3 hqXY 
    let fp3 =  (f .&. n .&. xy)  -- :: [(k, k)]
    return $ groupSort  fp3  
    -- return $ map onef . groupSort $ fp3 

unTagPoints2V:: (ObjectsHQ obj) => obj -> V2 Double 
unTagPoints2V = unName . unPointTag

-- area2faces :: [ObjTessShort] -> Double
area2faces vs = areaPoly . map unTagPoints2V $ vs 

-- area2triples ::(ObjTessShort, [ObjTessShort]) -> StoreTessShortElement
area2triples :: (MorphsHQ a2, ObjectsHQ b, ObjectsHQ obj) => (a1, [obj]) -> (a2, (a1, b))
area2triples (a,vds) = reorg214 (a, hqQuant 2, areaTag . Area . area2faces $ vds )
 
-- incenter2faces :: [obj] -> Maybe V2D
-- incenter2faces = incenter8 . map unTagPoints2V 

-- incenter8 :: [V2D] -> Maybe V2D
-- -- checks for triangle before!
-- incenter8 [a,b,c] = Just $ incenter a b c
-- incenter8 vds = Nothing 

-- for triples include the first obj

-- incenter2triple :: (ObjTessShort, [ObjTessShort]) -> Maybe StoreTessShortElement
incenter2triple :: forall rel obj . (ObjectsHQ obj, MorphsHQ rel, Eq rel, Eq obj,   Ord obj)  => (obj, [obj]) -> Maybe (rel, (obj, obj))
incenter2triple (i,vds) = case incenter2faces vds of
        Just vd -> Just . reorg214 $ (i, hqIncenter, (pointTag ( putName (unFace i)  vd)))
        Nothing -> Nothing
    where
        -- incenter2faces :: (ObjectsHQ obj) => [obj] -> Maybe V2D
        incenter2faces = incenter8 . map unTagPoints2V 

        -- incenter8 :: [V2D] -> Maybe V2D
        -- checks for triangle before!
        incenter8 [a,b,c] = Just $ incenter a b c
        incenter8 vds = Nothing 

-- incenter2triple (i,obj) = 
        -- maybe Nothing (\vd -> Just (i, Incenter, (PointTag $ putName (unFace i)  vd))) (incenter2faces obj)

-- unTagPoints2V = unName . unPointTag


 
-- circumcenter2triple :: (ObjTessShort, [ObjTessShort]) -> Maybe StoreTessShortElement
circumcenter2triple (i,vds) = case (circumCenter2faces vds) of
    Just vd -> Just . reorg214 $ (i, hqXY, (pointTag $ putName (unFace i) vd ))
    Nothing -> Nothing

    where 
        -- circumCenter2faces :: [ObjTessShort] -> Maybe V2D 
        -- checks for triples
        circumCenter2faces = circumCenter8 . map unTagPoints2V 

        circumCenter8 [a,b,c] = Just $ circumCenter a b c 
        circumCenter8 _ = Nothing 


-- for coord2faces 

-- area2facesM :: State  CatStoreTessShort   [(ObjTessShort, Double)]
-- area2facesM :: StateT
--   (Store obj ghc-prim-0.7.0:GHC.Types.Any)
--   Identity
--   [(obj, Double)]
area2facesM ::forall rel obj . (ObjectsHQ obj, MorphsHQ rel, Ord obj, Ord rel)  => State (Store obj rel)    [(obj, Double)]
area2facesM  = fmap (map (second area2faces)) coords2faces 

-- circum2facesM ::State  CatStoreTessShort   [ (ObjTessShort, Maybe V2D)]
circum2facesM::forall rel obj . (ObjectsHQ obj, MorphsHQ rel, Ord obj, Ord rel)  => State (Store obj rel)     [(obj, Maybe V2D)]
circum2facesM =  fmap (map (second (circumCenter2faces)))    coords2faces
    where
        -- circumCenter2faces :: [ObjTessShort] -> Maybe V2D 
        -- checks for triples
        circumCenter2faces = circumCenter8 . map unTagPoints2V 

        circumCenter8 [a,b,c] = Just $ circumCenter a b c 
        circumCenter8 _ = Nothing 


-- incenter2facesM :: State  CatStoreTessShort   [(ObjTessShort, Maybe V2D)]
incenter2facesM :: forall rel obj . (ObjectsHQ obj, MorphsHQ rel, Ord obj, Ord rel)  => State (Store obj rel)    [(obj, Maybe V2D)]
incenter2facesM = fmap (map (second ( (incenter2faces)))) coords2faces
    where
                -- incenter2faces :: (ObjectsHQ obj) => [obj] -> Maybe V2D
        incenter2faces = incenter8 . map unTagPoints2V 

        -- incenter8 :: [V2D] -> Maybe V2D
        -- checks for triangle before!
        incenter8 [a,b,c] = Just $ incenter a b c
        incenter8 vds = Nothing 

incenter2facesTriples  ::forall rel obj . (ObjectsHQ obj, MorphsHQ rel, Ord obj, Ord rel)  => State (Store obj rel)     [Maybe (rel, (obj, obj))]
incenter2facesTriples = fmap (map  ( incenter2triple)) coords2faces 

incircumCenter2facesTriples ::forall rel obj . (ObjectsHQ obj, MorphsHQ rel, Ord obj, Ord rel)  => State (Store obj rel)    [Maybe (Tup3 obj rel)]
incircumCenter2facesTriples = fmap (map  ( circumcenter2triple)) coords2faces 



-- | evaluate a transformation to a queryresult against a catStore 
-- questionalbe shortcut - may be difficult to debug?? 
-- evalTrans4query2cat :: (a -> b) -> Store obj MorphCountry   -> [(MorphCountry, (obj, obj))] -> [b]
evalTrans4query2cat trans query cat = evalState ((fmap (map trans )) query) cat 




