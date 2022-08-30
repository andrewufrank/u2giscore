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

class (Eq t, Ord t) => TimedTripleStore t trp where 
-- | a wrapper around the triples 
-- perhaps I should have created a class triple and one store
    ttempty :: [(t,trp)]
    ttinsert :: t -> trp -> [(t,trp)] -> [(t,trp)]
    -- ttdel 
    ttfind ::   t -> t -> [(t,trp)] -> [(t,trp)]
    -- provide lower and upper bound on time 

instance (Ord t) => TimedTripleStore t trp where 
    ttempty = []
    ttinsert t trp = ((t,trp) :)
    ttfind t1 t2 = filter (\tt -> ((t1 <=).fst $ tt) && ((t2 >). fst $ tt))

-- a conversion from triple to timed triple would be easy, but not required


pageTemporal2 :: ErrIO ()
pageTemporal2 = do 
    putIOwords ["start pageTemporal2 experiment"]
    -- putIOwords ["g88", showT . toList $ g88]

    return ()

{-

type Tup3 o p = (p,Tup2 o)

class (Eq p, Eq o) => Rels3 p o where 
    get3Rel :: p -> [(p,Tup2 o)] -> [Tup2 o]
    doTo3Rel :: p -> ([Tup2 o] -> [Tup2 o]) -> [(p,Tup2 o)] -> [Tup2 o]
    
instance (Eq p, Eq o) => Rels3 p o where 
    get3Rel p = map snd . filter ((p==).fst) 
    doTo3Rel p f = f . get3Rel p 

    -- then compose with all ops from Rels

-- instance (Eq p, Eq o) => Rel2s (p,(o,o)) where

converse3Rel :: (Eq p, Eq o, Rel2s o) => p -> [(p, (o, o))] -> [(o, o)]
converse3Rel p = doTo3Rel p converse2Rel  
-- filterRel3 :: (Eq p, Eq o) => p -> [(p, (o, o))] -> [(o, o)]
filter3Rel :: (Eq p, Eq o,  Rel2s o) => p -> ((o, o) -> Bool) -> [(p, (o, o))] -> [(o, o)]
filter3Rel p cond = doTo3Rel p (filter2Rel cond)


-- | not wrapped
newtype PlainStore o p = Store  [Tup3 o p] 
                    --  deriving (Show, Read, Eq)

-- | wrapped
newtype CatStore o p = CatStoreK [Tup3 o p] 
                     deriving (Show, Read, Eq)

instance (Show o, Show p) =>  NiceStrings (CatStore o p) where 
    shownice (CatStoreK oms) = (s2t "\nCatStoreK [\n") <> (showAsLines) oms <> "]\n"



class Stores  st o p where
    storeEmpty :: st o p
    storeInsert :: Tup3 o p -> st o p  -> st o p
    storeDel :: Tup3 o p -> st o p  -> st o p 
--     storeFind :: (Maybe o, Maybe p, Maybe o) -> st o p  -> [Tup3 o p]
    storeBatch :: [Action  (Tup3 o p)] -> st o p  -> st o p 
    storeBatch [] ts = ts
    storeBatch ((Ins t) : as) ts = storeInsert t . storeBatch as $ ts
    storeBatch ((Del t) : as) ts = storeDel t . storeBatch as $ ts

    unStore :: st o p -> [Tup3 o p]  
    -- unStore (StoreK as) = as
    wrapStore :: ([Tup3 o p] -> [Tup3 o p]) -> st o p-> st o p
    -- wrapStore f = StoreK . f . unStore  -- not a functor!"\n\t]"


instance (Eq o, Eq p, Rel2s o) => Stores  PlainStore o p  where
    storeEmpty =(Store []) 
    storeInsert t  = wrapStore  ((:) t)  
    -- storeDel t = wrapStore (del2rel t) 
--     storeFind t = tsfind t . unStore
    -- unStore :: Store o p -> [Tup3 o p]  
    unStore (Store as) = as
    -- wrapStore :: ([Tup3 o p] -> [Tup3 o p]) -> Store o p-> Store o p
    wrapStore f = Store . f . unStore  


instance (Eq o, Eq p, Rel2s o) => Stores CatStore o p where
    storeEmpty =(CatStoreK [])  
    storeInsert t  = wrapStore  ((:) t)  
    -- storeDel t = wrapStore (del2rel t) 
--     storeFind t = tsfind t . unStore
    -- unStore :: Store o p -> [Tup3 o p]  
    unStore (CatStoreK as) = as
    -- wrapStore :: ([Tup3 o p] -> [Tup3 o p]) -> Store o p-> Store o p
    wrapStore f = CatStoreK . f . unStore  -- not a functor!"\n\t]"


-- --- monadic versions -----------------------------
-- should be class to work for unwrapped and wrapped 

class Rels2monadic m p o where 
-- | monadic operatios to get relations and process
    rel2 :: p -> m [Tup2 o]
    -- | get binary relation for a property
    inv2 :: p -> m [Tup2 o]
    -- | get inverse relation for a property

instance (Stores st o p, MonadState m, Eq p, Eq o, Rel2s o, StateType m ~ st o p) => Rels2monadic m p o where 
 
    rel2 morph1 = do 
        c <- get 
        return $ get3Rel morph1 . unStore $ c
 
    inv2 morph1 = do 
        c <- get 
        return . map swap $ get3Rel morph1 . unStore $  c
-}