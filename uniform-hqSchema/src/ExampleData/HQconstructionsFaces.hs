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

module ExampleData.HQconstructionsFaces
    where

import           Test.Framework hiding (scale, (.&.))
import UniformBase  
import ExampleData.HQexampleShort
import ExampleData.HQschemaShort
-- import Control.Exception
import Uniform.GeometryFunctions
import Uniform.TripleRels
import Data.List.Extra
-- import Uniform.Drawings
import Control.Monad.State  


-- step 1 get the data from the store, leave tags!

-- | find the points with xy coord to a face
--   gives list of points, as [V2D] 
coords2faces :: StateT CatStoreTessShort Identity [(ObjTessShort, [ObjTessShort])]
coords2faces = do 
    f <- inv2 HqFace 
    n <- rel2 HqNode 
    xy <- rel2 XY 
    let fp3 =  (f .&. n .&. xy)
    return $ groupSort  fp3  
    -- return $ map onef . groupSort $ fp3 

-------------------- helpers 

dup a = (a,a)
unHalfQuad (HalfQuad i) = i
unFace :: ObjTessShort -> IDtype
unFace (Face i) = i
-- isTriangle :: (ObjTessShort, [V2D]) -> Maybe (ObjTessShort, (V2D, V2D, V2D))
-- isTriangle (i, [a,b,c]) = Just (i,(a,b,c))
-- isTriangle _ = Nothing 


--- step 2: collect the geometry operations 
--  - with same signature as result from data retrieved 

-- for faces

-- unpack to [V2D]
unTagPoints2V :: [ObjTessShort] -> [V2D]
unTagPoints2V = map (unName . unPointTag) 

isTriangle2 :: ( [V2D]) -> Maybe (  (V2D, V2D, V2D))
isTriangle2 ( [a,b,c]) = Just ( (a,b,c))
isTriangle2 _ = Nothing 

isTriangle1 :: [ObjTessShort] -> Maybe [ObjTessShort]
isTriangle1 a = if 3 == length a then Just a else Nothing
-- isTriangle1 _ = Nothing 

-- area2faces :: [V2D] -> Double 
area2faces :: [ObjTessShort] -> Double
area2faces vs = areaPoly . unTagPoints2V $ vs 
-- area2faces s = errorT ["not three points - not a triangle?", showT s]

-- circumCenter7 :: ToPD V2D => (ObjTessShort, (V2D, V2D, V2D)) -> (ObjTessShort, V2D)
-- circumCenter7 (i,(a,b,c)) = (i,circumCenter a b c)
-- circumCenter7x2 :: ToPD a => (a, a, a) -> a
-- circumCenter7x2 ( (a,b,c)) = ( circumCenter a b c)

incenter2faces :: [ObjTessShort] -> Maybe V2D
incenter2faces = incenter8 . unTagPoints2V 

incenter8 :: [V2D] -> Maybe V2D
-- must be checked for triangle before!
-- inCenter8    =  inCenter7 . fromJust . isTriangle2  
incenter8 [a,b,c] = Just $ inCenter a b c
incenter8 vds = Nothing 

-- for triples include the first obj
incenter2triple :: (ObjTessShort, [ObjTessShort]) -> Maybe StoreTessShortElement
incenter2triple (i,vds) = case incenter2faces vds of
    Just vd -> Just (i, Incenter, (PointTag $ putName (unFace i)  vd))
    Nothing -> Nothing

circumCenter2faces :: [ObjTessShort] -> Maybe V2D 
-- bombs if not triangle! check first 
circumCenter2faces = circumCenter8 . unTagPoints2V 

circumCenter8 [a,b,c] = Just $ circumCenter a b c 
circumCenter8 _ = Nothing 

-- incenter2triple :: (ObjTessShort, [ObjTessShort]) -> Maybe StoreTessShortElement
circumcenter2triple :: (ObjTessShort, [ObjTessShort]) -> Maybe StoreTessShortElement
circumcenter2triple (i,vds) = case (circumCenter2faces vds) of
    Just vd -> Just (i, XY, (PointTag $ putName (unFace i) vd ))
    Nothing -> Nothing
-- errorT ["inCenter8 not triangle, must check before!", showT vds]


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



-- for coord2faces 

area2facesM :: StateT CatStoreTessShort Identity [(ObjTessShort, Double)]
area2facesM  = fmap (map (second area2faces)) coords2faces 

circum2facesM ::StateT CatStoreTessShort Identity [ (ObjTessShort, Maybe V2D)]
circum2facesM =  fmap (map (second (circumCenter2faces)))    coords2faces

incenter2facesM :: StateT CatStoreTessShort Identity [(ObjTessShort, Maybe V2D)]
incenter2facesM = fmap (map (second ( (incenter2faces  )))) coords2faces

incenter2facesTriples = fmap (map  ( incenter2triple)) coords2faces 
incircumCenter2facesTriples = fmap (map  ( circumcenter2triple)) coords2faces 

-- incenter2facesM = fmap (map (second (fmap (incenter2faces  )))) coords2faces
-- incenter2facesM = fmap (map (second (fmap (inCenter7 . isTriangle2 . )))) coords2faces

-- incenter2facesTriple :: (ObjTessShort, [V2D]) -> Maybe (ObjTessShort, MorphTessShort, ObjTessShort)
-- incenter2facesTriple (i,(vs)) = 
--         case isTriangle2 vs of 
--             Nothing  -> Nothing 
--             Just (a,b,c) -> Just (i, Incenter, (PointTag $ putName (unFace i)  (inCenter7 (a,b,c))))

-- circumcenter2facesTriple :: (ObjTessShort, [V2D]) -> Maybe (ObjTessShort, MorphTessShort, ObjTessShort)
-- circumcenter2facesTriple (i,(vs)) = 
--         case isTriangle2 vs of 
--             Nothing  -> Nothing 
--             Just (a,b,c) -> Just 


-- x3 :: Maybe (a1, (b, b, b)) -> Maybe (a1, b)
-- x3 = maybe Nothing circumCenter7





instance NiceStrings Float where shownice = showT 

