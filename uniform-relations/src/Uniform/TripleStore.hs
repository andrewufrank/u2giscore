-----------------------------------------------------------------------------
--
-- Module      :  Triple Relations
-- base for queries is the compostion of relations

-- the triple store is typed, thus the relations are typed as well
-- (o,p,v) in the triplestore is interpreted as a relation p(o,v)
--
-- uses the naive triplestore
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
import Uniform.TripleRels
import Uniform.NaiveTripleStore
-- import UniformBase (NiceStrings)
    -- ( Action(..), TripleStore(tsfind, tsinsert, tsdel) )

type CPoint o m =  (o,m,o)  -- a relation line (m - is predicate (aka morph), o is an subject/value, here object)

newtype CatStore o m = CatStoreK [CPoint o m] 
                     deriving (Show, Read, Eq)

instance (Show o, Show m) =>  NiceStrings (CatStore o m) where 
    shownice (CatStoreK oms) = (s2t "\nCatStoreK [\n") <> (showlong) oms <> "\n\t]"

-- instance (Show a, Show b, Show c) => NiceStrings (a,b,c) where 
--             showNice a = showT a 

-- instance (Show a ) => NiceStrings [a] where 
--             showNice a = intersperse "\n"   map  a 

unCatStore :: CatStore o m -> [CPoint o m]
unCatStore (CatStoreK as) = as
wrapCatStore :: ([CPoint o m] -> [CPoint o m]) -> CatStore o m-> CatStore o m
wrapCatStore f = CatStoreK . f . unCatStore  -- not a functor!

getRel :: (Eq m )=> CatStore o m -> m -> Rel2 o
getRel cat m = map out13 . filter ((m ==) . snd3) . unCatStore $ cat

out13 :: (a, b1, b2) -> (a, b2)  -- todo
out13 (a,b,c) = (a,c)


class CatStores o m where
    catStoreEmpty :: CatStore o m
    catStoreInsert :: CPoint o m -> CatStore o m  -> CatStore o m
    catStoreDel :: CPoint o m -> CatStore o m  -> CatStore o m 
    catStoreFind :: (Maybe o, Maybe m, Maybe o) -> CatStore o m  -> [CPoint o m]
    catStoreBatch :: [Action (o,m,o)] -> CatStore o m  -> CatStore o m 
    catStoreBatch [] ts = ts
    catStoreBatch ((Ins t) : as) ts = catStoreInsert t . catStoreBatch as $ ts
    catStoreBatch ((Del t) : as) ts = catStoreDel t . catStoreBatch as $ ts




instance (Eq o, Eq m, TripleStore o m o) => CatStores o m where
    catStoreEmpty =(CatStoreK []) :: CatStore o m
    catStoreInsert t  = wrapCatStore  (tsinsert t)  
    catStoreDel t = wrapCatStore (tsdel t) 
    catStoreFind t = tsfind t . unCatStore

