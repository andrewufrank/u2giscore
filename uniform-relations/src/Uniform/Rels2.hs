-----------------------------------------------------------------------------
--
-- Module      :    Relations with two values 

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

type Rel2 o = [](o,o)
-- | a binary relation

class Rel2pair  o where 
    swaprel :: o -> o 
    -- fstEq :: 
instance Rel2pair (o,o) where
    swaprel (a,b) = (b,a)
    -- fstEqual 


class Eq e =>  Rels e where 
-- | relations as list of elements
-- this is just list! and all is automatic
    emptyRel2 :: [e] 
    add2rel :: e -> [e] -> [e]
    del2rel :: e -> [e] -> [e]
    emptyRel2 = []
    add2rel a = (a :)
    del2rel a = filter (a /=) 

class (Eq e, Rel2pair e) =>  Rels2 e where 
-- | operations for relations
-- could be a family with element and list  
    converseRel :: [e] -> [e]
    converseRel = map swaprel  

    -- | compose relations r1 and r2 (r1:2, A->B . B->C -> A->C)
    compRel ::   [e] -> [e] ->  [e]
    (.&.) ::    [e] -> [e] ->  [e]
    (.&.) =  compRel  
    filterRel :: (e -> Bool) -> [e] -> [e]
    -- filterEq :: e -> [e] -> [e]
    -- filterEq v r = filterRel ((v ==) . fst) r
    -- relPair :: (Eq o) =>  [e] -> [e] ->  [(o, (o,o))]
    -- make the obj a pair of the the objects of the two relations

data Action a = Ins a | Del a
        deriving (Show, Read, Ord, Eq)

instance Eq o => Rels (o,o) where
    -- emptyRel2 = []
    -- add2rel a = (a :)
    -- del2rel a = filter (a /=) 

instance Eq o => Rels2 (o,o) where
    compRel r1 r2 = [ (a,d) | (a,b) <- r1, (c,d) <- r2, b==c]
    -- relPair r1 r2 = [ (a,(b,d)) |  (a,b) <- r1, (c,d) <- r2, a==c ]
    filterRel cond = filter cond 

-- instance Rels2 CatStores 

