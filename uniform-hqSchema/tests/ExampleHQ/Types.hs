-----------------------------------------------------------------------------
--
-- Module      :  The schema for the minimal country data set
-- based on and uses the code for the HQ schema 
-- for handling the geometry of tesselations 

-- this is the short schema style 
-- (with sumtype tag is Obj constructor)
-- using IDtype = Int
-- hq objects are not tagged (because no additional data)
-- country objects are tagged (and separately defined records)   
--      Points  
--      Persons, Places 
-- value data types are tagged 
--      Length, Area
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


module ExampleHQ.Types
    -- (module Country.CountrySchema
    -- , module Uniform.TripleStore
    -- , module Uniform.Rels2
    -- ) 
    where

import Uniform.Point2d
import Uniform.TripleStore
import Uniform.Rels2
-- import Uniform.Point2d
import UniformBase
import Control.Monad.State


-- type IDtype = Int


-- data Length a = Length a  
--     deriving (Show, Read, Ord, Eq, Generic, Zeros, Functor)
-- instance (Show a) => NiceStrings (Length a) where
-- --   showNice = showT   
-- type LengthD = Length Double 

data Person   = Person  
        { persName :: Text
        , persFirstName :: Text 
        }
    deriving (Show, Read, Ord, Eq, Generic, Zeros)
instance ( ) => NiceStrings (Person ) where
--   showNice = showT   
-- type AreaD = Area Double 

data Place = Place 
        { placeName :: Text 
        }
        deriving (Show, Read, Ord, Eq, Generic, Zeros)
instance ( ) => NiceStrings (Place ) where


