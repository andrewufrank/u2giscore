-----------------------------------------------------------------------------
--
-- Module      :  Uniform.TempTripleStore

-- | the time wrapper around the triple store 
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
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric  #-}
-- {-# LANGUAGE TypeApplications     #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.TempTripleStore
         where

import Control.Monad.State
import UniformBase
import Uniform.Time
import Uniform.Point2d 
import Uniform.TripleStore
-- import HQschema.HQschemaTop
-- import Uniform.Raster
-- import Uniform.Field
-- import Data.Complex
 
-- import ExampleData.TerrainLike
-- import Uniform.Fourier 
-- import Uniform.Raster
type Time = UTCTime
-- type Storage = CatStoreTessShort
-- type StorageElement = StoreTessShortElement

-- copied from TripleStore
-- type StoreTessShortElement = (ObjTessShort, MorphTessShort, ObjTessShort)

-- type CatStoreTessShort = CatStore ObjTessShort MorphTessShort
-- type CatStoreState = State  CatStoreTessShort [StoreTessShortElement]

type Tup4 o p t = (t, Tup3 o p)

class (Eq p, Eq o, Eq t) => Rels4 t p o where 
-- | process one triple relation (cascading the getNrel - replace with lenses)
    get4Rel :: t -> [(t,Tup3 o p)] -> [Tup3 o p]
    -- get the 3 real for a time 
    -- doTo4Rel :: t -> ([Tup3 o p] -> [Tup3 o p]) -> [(t,(Tup3 o p))] -> [Tup3 o p]
    -- get the 3 real for a time and operate on it, but how to write back?

    -- converse4Rel :: (BinRels o) => t -> p -> [(t,Tup3 o p)] -> [(o, o)]
    -- -- filterRel3 :: (Eq p, Eq o) => p -> [(p, (o, o))] -> [(o, o)]
    -- filter4Rel :: (BinRels o) => t -> p -> ((o, o) -> Bool) -> [(t,Tup3 o p)] -> [(o, o)]


instance (Eq p, Eq o, Eq t) => Rels4 t p o where 
    get4Rel t = map snd . filter ((t==).fst) 
    -- doTo4Rel t f = f .  get4Rel t 
    -- should replace!
    -- converse4Rel t p = doTo4Rel t p converse2Rel  
-- filterRel3 :: (Eq p, Eq o) => p -> [(p, (o, o))] -> [(o, o)]
    -- filter4Rel t p cond = doTo4Rel p (filter2Rel cond)

class TStores  st o p t where
    tStoreEmpty :: st o p t
    tStoreInsert :: Tup4 o p t -> st o p t -> st o p t
    tStoreDel :: Tup4 o p t -> st o p t -> st o p t
--     tStoreFind :: (Maybe o, Maybe p, Maybe o) -> st o p  -> [Tup4 o p]
    tStoreBatch :: [Action  (Tup4 o p t)] -> st o p t -> st o p t
    tStoreBatch [] ts = ts
    tStoreBatch ((Ins e) : as) es = tStoreInsert e . tStoreBatch as $ es
    tStoreBatch ((Del e) : as) es = tStoreDel e . tStoreBatch as $ es

    unTStore :: st o p t -> [Tup4 o p t]  
    -- unStore (StoreK as) = as
    wrapTStore :: ([Tup4 o p t] -> [Tup4 o p t]) -> st o p t -> st o p t
    -- wrapStore f = StoreK . f . unStore  -- not a functor!"\n\t]"

newtype TStore o p t = TStore  [Tup4 o p t] 
                    --  deriving (Show, Read, Eq)

instance (Eq o, Eq p, Eq t,  BinRels o) => TStores  TStore o p t where
    tStoreEmpty =(TStore []) 
    tStoreInsert e  = wrapTStore  ((:) e)  
    -- tStoreDel t = wrapStore (del2rel t) 
--     tStoreFind t = tsfind t . unStore
    -- unStore :: Store o p -> [Tup4 o p t]  
    unTStore (TStore as) = as
    -- wrapStore :: ([Tup4 o p t] -> [Tup4 o p t]) -> Store o p-> Store o p
    wrapTStore f = TStore . f . unTStore  

class Rels3monadic m p o where 
-- | monadic operatios to get relations and process
-- copied from Rels2monadic
    rel3 :: p -> m [Tup2 o]
    -- | get binary relation for a property
    inv3 :: p -> m [Tup2 o]
    -- | get inverse relation for a property

instance (TStores st o p t, MonadState m
        , Eq p, Eq o, Eq t, Rel2s o
        , StateType m ~ st o p t) => Rels3monadic m p o where 
 
    rel3 morph1 = do 
        c <- get 
        return $ get4Rel morph1 . unTStore $ c
 
    inv3 morph1 = do 
        c <- get 
        return . map swap $ get4Rel morph1 . unSTtore $  c

---------- old 



pageTemporal2 :: ErrIO ()
pageTemporal2 = do 
    putIOwords ["start pageTemporal2 experiment"]
    -- putIOwords ["g88", showT . toList $ g88]

    return ()

