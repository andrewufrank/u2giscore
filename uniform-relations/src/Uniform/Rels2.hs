-----------------------------------------------------------------------------
--
-- Module      :    Binary Relations with two values 

-- base for queries is the compostion of relations

--  
--
-- this is the foundation - data for a single relation
-- 
-- braucht noch family for val v vs element (v,v) vs list [(v,v,)]

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

module Uniform.Rels2
 
    where

import UniformBase 

type Tup2 o = (o,o)
-- type Rel2 o = [Tup2 o]
-- | a binary relation

class Eq o =>  BinRels o where 
-- | relations as list of elements
    data Tup o 
-- this is just list! and all is automatic
    empty2Rel :: [Tup2 o] 
    empty2Rel = []
    add2rel :: Tup2 o -> [Tup2 o] -> [Tup2 o]
    add2rel a = (a :)
    del2rel :: Tup2 o -> [Tup2 o] -> [Tup2 o]
    del2rel a = filter (a /=) 
    -- | deletes all element with same value

-- class (Eq o, Rel2pair o) =>  Rels2 o where 
-- | operations for relations
-- could be a family with element and list  
    converse2Rel :: [Tup2 o] -> [Tup2 o]
    converse2Rel = map swap2rel  

    -- | compose relations r1 and r2 (r1:2, A->B . B->C -> A->C)
    comp2Rel ::   [Tup2 o] -> [Tup2 o] ->  [Tup2 o]
    (.&.) ::    [Tup2 o] -> [Tup2 o] ->  [Tup2 o]
    (.&.) =  comp2Rel  
    filter2Rel :: (Tup2 o -> Bool) -> [Tup2 o] -> [Tup2 o]
    -- filterEq :: o -> [o] -> [o]
    -- filterEq v r = filterRel ((v ==) . fst) r
    -- relPair :: (Eq o) =>  [o] -> [o] ->  [(o, Tup2)]
    -- make the obj a pair of the the objects of the two relations
    rel2pair :: (Eq o) =>  [Tup2 o] -> [Tup2 o] ->  [(o, Tup2 o)]
    -- make the obj a pair of the the objects of the two relations
    swap2rel :: Tup2 o -> Tup2 o 


instance Eq o => BinRels (o) where
    data Tup o = Tup2 o
    -- emptyRel2 = []
    -- add2rel a = (a :)
    -- del2rel a = filter (a /=) 

-- instance Eq o => Rels2 (Tup2 o) where
    comp2Rel r1 r2 = [ (a,d) | (a,b) <- r1, (c,d) <- r2, b==c]
    -- relPair r1 r2 = [ (a,(b,d)) |  (a,b) <- r1, (c,d) <- r2, a==c ]
    filter2Rel cond = filter cond 
    rel2pair r1 r2 = [ (a,(b,d)) |  (a,b) <- r1, (c,d) <- r2, a==c ]
    swap2rel (a,b) = (b,a)



