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

import UniformBase  
    -- ( Generic, fst3, trd3, errorT, putIOwords, showT, Zeros(zero) )
import Uniform.Rels2
-- import Uniform.NaiveTripleStore
-- import UniformBase (NiceStrings)
    -- ( Action(..), TripleStore(tsfind, tsinsert, tsdel) )

 
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
    -- emptyRel2 = []
    -- add2rel a = (a :)
    -- del2rel 
-- instance (Eq m, Eq o) => Rels (m,(o,o)) where


-- emptyRel2 = CatStoreK emptyRel2
-- add2rel a = wrapCatStore (add2rel (a :))


converseRel3 :: (Eq m, Eq o) => m -> [(m, (o, o))] -> [(o, o)]
converseRel3 m = doToRel m converseRel  
-- filterRel3 :: (Eq m, Eq o) => m -> [(m, (o, o))] -> [(o, o)]
filterRel3 :: (Eq m, Eq o) => m -> ((o, o) -> Bool) -> [(m, (o, o))] -> [(o, o)]
filterRel3 m cond = doToRel m (filterRel cond)


    -- compRel r1 r2 = [ (a,d) | (a,b) <- r1, (c,d) <- r2, b==c]
    -- -- relPair r1 r2 = [ (a,(b,d)) |  (a,b) <- r1, (c,d) <- r2, a==c ]
    -- filterRel cond = filter cond 


-- wrapping later

newtype CatStore o m = CatStoreK [(m,(o,o))] 
                     deriving (Show, Read, Eq)

instance (Show o, Show m) =>  NiceStrings (CatStore o m) where 
    shownice (CatStoreK oms) = (s2t "\nCatStoreK [\n") <> (showAsLines) oms <> "]\n"

unCatStore :: CatStore o m -> [CPoint o m]  
unCatStore (CatStoreK as) = as
wrapCatStore :: ([CPoint o m] -> [CPoint o m]) -> CatStore o m-> CatStore o m
wrapCatStore f = CatStoreK . f . unCatStore  -- not a functor!"\n\t]"

-- instance Rels (CatStore o m) where


-- instance Rels (CatStore o m) where 
--     emptyRel2 = CatStoreK emptyRel2 

-- instance (Show a, Show b, Show c) => NiceStrings (a,b,c) where 
--             showNice a = showT a 

-- instance (Show a ) => NiceStrings [a] where 
--             showNice a = intersperse "\n"   map  a 



-- getRel :: (Eq m )=> CatStore o m -> m -> Rel2 o
-- getRel cat m = map out13 . filter ((m ==) . snd3) . unCatStore $ cat

out13 :: (a, b1, b2) -> (a, b2)  -- todo
out13 (a,b,c) = (a,c)


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

