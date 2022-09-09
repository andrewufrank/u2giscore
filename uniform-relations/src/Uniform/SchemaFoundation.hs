-----------------------------------------------------------------------------
--
-- Module      : Schema foundatation

--              The code include in evey schema

-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Uniform.SchemaFoundation
    ( module Uniform.SchemaFoundation
    , module Uniform.TripleStore
    , module Uniform.Rels2)

    where


import UniformBase  
import Uniform.TripleStore
import Uniform.Rels2
import Uniform.Point2d
-- | the morphism necessary for HQ construction

type IDtype = Int 

data ObjCountry = O | P 
data MorphCountry = A | B 

class MorphsHQ a where 
    hqFace :: a 
    hqNode :: a 
    hqXY :: a 
    hqTwin :: a
    hqQuant :: Int -> a 
    hqIncenter :: a 

-- | the object types for HQ construction
class ObjectsHQ a where 
    nodeObj :: IDtype -> a 
    edgeObj :: IDtype -> a
    faceObj :: IDtype -> a 
    halfQuadObj :: IDtype -> a 
    -- pointObj :: a
    unHalfQuad :: a -> IDtype
    unFace :: a -> IDtype 
    unPointTag :: a -> Pnt2 
    pointTag :: Pnt2 -> a 
    unLengthTag :: a -> LengthD
    lengthTag :: LengthD -> a 
    areaTag :: AreaD -> a
    unAreaTag :: a -> AreaD 
    


data HQtriples obj rel = HQtriples 
    { _NodesTrip :: [Tup3 obj rel]
    , _FacesTrip :: [Tup3 obj rel]
    , _HQtrips   :: [Tup3 obj rel]
    }
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- reorganisationm o,p,o -> p, (o,o)  TODO 
reorg214 :: (a1, a2, b) -> (a2, (a1, b))
reorg214 (a,b,c) = (b, (a,c))

data Length a = Length a  
    deriving (Show, Read, Ord, Eq, Generic, Zeros, Functor)
instance (Show a) => NiceStrings (Length a) where
--   showNice = showT   
type LengthD = Length Double 

data Area a = Area a  
    deriving (Show, Read, Ord, Eq, Generic, Zeros, Functor)
instance (Show a) => NiceStrings (Area a) where
--   showNice = showT   
type AreaD = Area Double 

-- unHalfQuad :: ObjCountry -> IDtype
-- unHalfQuad (HalfQuad i) = i
-- unHalfQuad x = errorT ["unHalfQuad - not a HalfQuad", showT x]

-- unFace :: ObjCountry -> IDtype
-- unFace (Face i) = i
-- unFace x = errorT ["unFace - not a Face", showT x]

-- unTagPoints2V = unName . unPointTag

-- unPointTag :: ObjCountry -> Pnt2
-- unPointTag (PointTag t) = t 
-- unPointTag x = errorT ["unPointTag - not a Point", showT x]
-- -- unCostTag :: ObjCountry -> Cost
-- -- unCostTag (CostTag t) = t 
-- -- unCostTag x = errorT ["unCostTag -  not a Cost", showT x]
-- unLengthTag :: ObjCountry -> LengthD
-- unLengthTag (LengthTag t) = t 
-- unLengthTag x = errorT ["unLengthTag - not a Length", showT x]
