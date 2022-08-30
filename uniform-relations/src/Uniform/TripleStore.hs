-----------------------------------------------------------------------------
--
-- Module      :  Triple Relations
-- base for queries is the compostion of relations

-- the triple store is typed, thus the relations are typed as well
-- (o,p,v) -- here (p,(o,o)) in the triplestore is interpreted as a relation p(o,v)
--
--  
-- 
-- a category consists of Objects (CObj) and Morphism (Morph)
-- they are typed for typed functions and points in the CObj

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

module Uniform.TripleStore

    where

import Control.Monad.State
import UniformBase  
    -- ( Generic, fst3, trd3, errorT, putIOwords, showT, Zeros(zero) )
import Uniform.Rels2

 
type CPoint o m =  (m,(o,o)) -- a relation line (m - is predicate (aka morph), o is an subject/value, here object)
type Rel3 o m = (m,Rel2 o)

class (Eq m, Eq o) => Rels3 m o where 
    getRel :: m -> [(m,(o,o))] -> [(o,o)]
    doToRel :: m -> ([(o,o)] -> [(o,o)]) -> [(m,(o,o))] -> [(o,o)]
    
instance (Eq m, Eq o) => Rels3 m o where 
    getRel m = map snd . filter ((m==).fst) 
    doToRel m f = f . getRel m 

    -- then compose with all ops from Rels

instance (Eq m, Eq o) => Rels (m,(o,o)) where

converseRel3 :: (Eq m, Eq o) => m -> [(m, (o, o))] -> [(o, o)]
converseRel3 m = doToRel m converseRel  
-- filterRel3 :: (Eq m, Eq o) => m -> [(m, (o, o))] -> [(o, o)]
filterRel3 :: (Eq m, Eq o) => m -> ((o, o) -> Bool) -> [(m, (o, o))] -> [(o, o)]
filterRel3 m cond = doToRel m (filterRel cond)


-- wrapping 

newtype CatStore o m = CatStoreK [(m,(o,o))] 
                     deriving (Show, Read, Eq)

instance (Show o, Show m) =>  NiceStrings (CatStore o m) where 
    shownice (CatStoreK oms) = (s2t "\nCatStoreK [\n") <> (showAsLines) oms <> "]\n"

unCatStore :: CatStore o m -> [CPoint o m]  
unCatStore (CatStoreK as) = as
wrapCatStore :: ([CPoint o m] -> [CPoint o m]) -> CatStore o m-> CatStore o m
wrapCatStore f = CatStoreK . f . unCatStore  -- not a functor!"\n\t]"


class CatStores o m where
    catStoreEmpty :: CatStore o m
    catStoreInsert :: CPoint o m -> CatStore o m  -> CatStore o m
    catStoreDel :: CPoint o m -> CatStore o m  -> CatStore o m 
--     catStoreFind :: (Maybe o, Maybe m, Maybe o) -> CatStore o m  -> [CPoint o m]
    catStoreBatch :: [Action  (m,(o,o))] -> CatStore o m  -> CatStore o m 
    catStoreBatch [] ts = ts
    catStoreBatch ((Ins t) : as) ts = catStoreInsert t . catStoreBatch as $ ts
    catStoreBatch ((Del t) : as) ts = catStoreDel t . catStoreBatch as $ ts




instance (Eq o, Eq m, Rels (o,o)) => CatStores o m where
    catStoreEmpty =(CatStoreK (emptyRel2)) :: CatStore o m
    catStoreInsert t  = wrapCatStore  (add2rel t)  
    catStoreDel t = wrapCatStore (del2rel t) 
--     catStoreFind t = tsfind t . unCatStore

-- --- monadic versions -----------------------------
-- should be class to work for unwrapped and wrapped 

-- rel2 :: (MonadState (CatStore o m) m1, Eq o, Eq m) => m -> m1 (Rel2 o) 
rel2 :: (MonadState m1, Eq m2, Eq o, StateType m1 ~ CatStore o m2) => m2 -> m1 (Rel2 o)
-- rel2 :: (MonadState m1, Eq m2, Eq o, StateType m1 ~ [(m2, (o, o))]) => m2 -> m1 [(o, o)]
rel2 morph1 = do 
    c <- get 
    return $ getRel morph1 . unCatStore $ c
-- inv2 :: (MonadState m1, Eq m2, Eq b, StateType m1 ~ [(m2, (b, b))]) => m2 -> m1 [(b, b)]
inv2 :: (MonadState m1, Eq m2, Eq b, StateType m1 ~ CatStore b m2) => m2 -> m1 [(b, b)]
inv2 morph1 = do 
    c <- get 
    return . map swap $ getRel morph1 . unCatStore $  c

relPair :: (Eq o) =>  [(o,o)] -> [(o,o)] ->  [(o, (o,o))]
relPair r1 r2 = [ (a,(b,d)) |  (a,b) <- r1, (c,d) <- r2, a==c ]
-- make the obj a pair of the the objects of the two relations
