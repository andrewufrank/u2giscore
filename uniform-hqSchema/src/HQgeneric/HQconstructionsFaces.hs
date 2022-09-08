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

module HQgeneric.HQconstructionsFaces
    where

import           Test.Framework hiding (scale, (.&.))
import UniformBase  
import Uniform.TripleStore
import Country.Schema
import Country.Store 
-- import HQschema.HQexampleShort
-- import HQschema.HQschemaShort
-- import Control.Exception
import Uniform.GeometryFunctions
import Uniform.Rels2
import Data.List.Extra
-- import Uniform.Drawings
import Control.Monad.State  
import Data.Functor.Identity
-- import Control.Monad.Trans.Identity

-- step 1 get the data from the store, leave tags!

-- | find the points with xy coord to a face
--   gives list of points, as [V2D] 
-- coords2faces :: StateT CatStoreTessShort Identity [(ObjTessShort, [ObjTessShort])]
-- coords2faces :: m [(k, [k])]
coords2faces  :: State CatCountry  [(ObjCountry, [ObjCountry])]
coords2faces = do 
    f <- inv3 HqFace 
    n <- rel3 HqNode 
    xy <- rel3 XY 
    let fp3 =  (f .&. n .&. xy)  -- :: [(k, k)]
    return $ groupSort  fp3  
    -- return $ map onef . groupSort $ fp3 


-- area2faces :: [ObjTessShort] -> Double
area2faces vs = areaPoly . map unTagPoints2V $ vs 

-- area2triples ::(ObjTessShort, [ObjTessShort]) -> StoreTessShortElement
area2triples (a,vds) = reorg214 (a, Quant 2, AreaTag . Area . area2faces $ vds )
 
-- incenter2faces :: [ObjTessShort] -> Maybe V2D
incenter2faces = incenter8 . map unTagPoints2V 

incenter8 :: [V2D] -> Maybe V2D
-- checks for triangle before!
incenter8 [a,b,c] = Just $ incenter a b c
incenter8 vds = Nothing 

-- for triples include the first obj

-- incenter2triple :: (ObjTessShort, [ObjTessShort]) -> Maybe StoreTessShortElement
incenter2triple (i,vds) = case incenter2faces vds of
        Just vd -> Just . reorg214 $ (i, Incenter, (PointTag $ putName (unFace i)  vd))
        Nothing -> Nothing
-- incenter2triple (i,obj) = 
        -- maybe Nothing (\vd -> Just (i, Incenter, (PointTag $ putName (unFace i)  vd))) (incenter2faces obj)

-- circumCenter2faces :: [ObjTessShort] -> Maybe V2D 
-- checks for triples
circumCenter2faces = circumCenter8 . map unTagPoints2V 

circumCenter8 [a,b,c] = Just $ circumCenter a b c 
circumCenter8 _ = Nothing 

 
-- circumcenter2triple :: (ObjTessShort, [ObjTessShort]) -> Maybe StoreTessShortElement
circumcenter2triple (i,vds) = case (circumCenter2faces vds) of
    Just vd -> Just . reorg214 $ (i, XY, (PointTag $ putName (unFace i) vd ))
    Nothing -> Nothing

-- for coord2faces 

-- area2facesM :: State  CatStoreTessShort   [(ObjTessShort, Double)]
area2facesM  = fmap (map (second area2faces)) coords2faces 

-- circum2facesM ::State  CatStoreTessShort   [ (ObjTessShort, Maybe V2D)]
circum2facesM =  fmap (map (second (circumCenter2faces)))    coords2faces

-- incenter2facesM :: State  CatStoreTessShort   [(ObjTessShort, Maybe V2D)]
incenter2facesM = fmap (map (second ( (incenter2faces  )))) coords2faces

-- incenter2facesTriples :: State  CatStoreTessShort  [Maybe StoreTessShortElement]
incenter2facesTriples = fmap (map  ( incenter2triple)) coords2faces 
-- incircumCenter2facesTriples :: State  CatStoreTessShort  [Maybe StoreTessShortElement]
incircumCenter2facesTriples = fmap (map  ( circumcenter2triple)) coords2faces 



-- | evaluate a transformation to a queryresult against a catStore 
-- questionalbe shortcut - may be difficult to debug?? 
evalTrans4query2cat trans query cat = evalState ((fmap (map trans )) query) cat 




