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

-- | the morphism necessary for HQ construction
class MorphsHQ a where 
    hqFace :: a 
    hqNode :: a 
    hqXY :: a 

-- | the object types for HQ construction
class ObjectsHQ a where 
    nodeObj :: a 
    edgeObj :: a
    faceObj :: a 
    halfQuadObj :: a 
    pointObj :: a

data HQtriples obj rel = HQtriples 
    { _NodesTrip :: [Tup3 obj rel]
    , _FacesTrip :: [Tup3 obj rel]
    , _HQtrips   :: [Tup3 obj rel]
    }
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- reorganisationm o,p,o -> p, (o,o)  TODO 
reorg214 :: (a1, a2, b) -> (a2, (a1, b))
reorg214 (a,b,c) = (b, (a,c))
