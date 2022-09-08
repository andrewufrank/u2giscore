-----------------------------------------------------------------------------
--
-- Module      :  The definitions to include to use the 
--                  Country schema
--
-----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveFunctor  #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}


module Country.Store
    (module Country.Store
    , module Country.Schema
    , module Uniform.TripleStore
    , module Uniform.Rels2
    ) where

import Uniform.Point2d
import Uniform.TripleStore
import Uniform.Rels2
-- import Uniform.Point2d
import UniformBase
import Control.Monad.State
import Country.HQtypes  -- data types defined for HQ 
import Country.Schema  -- data types defined for HQ 



    
instance Zeros ObjCountry where zero = ZZpoint

instance NiceStrings ObjCountry
    -- where shownice = showT
    -- would need a shownice for pointTag to propagate the shownice to the inside data 
-- instance NiceStrings Pnv2 where 
--     shownice = showT 

unHalfQuad :: ObjCountry -> IDtype
unHalfQuad (HalfQuad i) = i
unHalfQuad x = errorT ["unHalfQuad - not a HalfQuad", showT x]

unFace :: ObjCountry -> IDtype
unFace (Face i) = i
unFace x = errorT ["unFace - not a Face", showT x]

unTagPoints2V = unName . unPointTag

unPointTag :: ObjCountry -> Pnt2
unPointTag (PointTag t) = t 
unPointTag x = errorT ["unPointTag - not a Point", showT x]
-- unCostTag :: ObjCountry -> Cost
-- unCostTag (CostTag t) = t 
-- unCostTag x = errorT ["unCostTag -  not a Cost", showT x]
unLengthTag :: ObjCountry -> LengthD
unLengthTag (LengthTag t) = t 
unLengthTag x = errorT ["unLengthTag - not a Length", showT x]

-- p2dToV2 :: Point2d -> V2d 
-- p2dToV2 (Point2d x y) = V2 x y 


    
instance Zeros MorphCountry  where zero = ZZm
instance NiceStrings MorphCountry
    -- where shownice = showT  


data HQtriples = HQtriples 
    { _NodesTrip :: [CountryElement]
    , _FacesTrip :: [CountryElement]
    , _HQtrips   :: [CountryElement]
    }
    deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- reorganisationm o,p,o -> p, (o,o)

reorg214 :: (a1, a2, b) -> (a2, (a1, b))
reorg214 (a,b,c) = (b, (a,c))

type CountryElement = (MorphCountry, (ObjCountry,  ObjCountry))

type CatCountry = CatStore ObjCountry MorphCountry
type CatCountryState = State  CatCountry [CountryElement]
