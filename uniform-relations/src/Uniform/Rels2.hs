-----------------------------------------------------------------------------
--
-- Module      :    Relations with two values 

-- base for queries is the compostion of relations

--  
--
-- this is the foundation - data for a single relation
-- 
-- 

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

import Control.Monad.State  
import Data.List ( nub ) 
import UniformBase 
-- import Uniform.TripleStore -- ( CatStore, unCatStore )
    -- ( Action(..), TripleStore(tsfind, tsinsert, tsdel) )

type Rel2 o = [](o,o)
-- | a binary relation


-- instance NiceStrings   (Rel2 o) where 
--     showlong r = intersperse 

-- getRel :: (Eq m )=> CatStore o m -> m -> Rel2 o
-- getRel cat m = map out13 . filter ((m ==) . snd3) . unCatStore $ cat
-- class Eq o =>  Rels o where 
--     emptyRel2 :: Rel2 o 
--     add2rel :: (o,o) -> Rel2 o -> Rel2 o
--     converseRel :: Rel2 o -> Rel2 o
-- -- | compose relations r1 and r2 (r1:2, A->B . B->C -> A->C)
--     compRel :: (Eq o) =>  Rel2 o -> Rel2 o ->  Rel2 o
--     (.&.) :: (Eq o) =>  Rel2 o -> Rel2 o ->  Rel2 o
--     (.&.) =  compRel  
--     filterRel :: ((o,o) -> Bool) -> Rel2 o -> Rel2 o
--     filterEq :: o -> Rel2 o -> Rel2 o
--     filterEq v r = filterRel ((v ==) . fst) r
--     relPair :: (Eq o) =>  Rel2 o -> Rel2 o ->  [(o, (o,o))]
--     -- make the obj a pair of the the objects of the two relations

class Eq e =>  Rels e where 
    emptyRel2 :: [e] 
    add2rel :: e -> [e] -> [e]
    converseRel :: [e] -> [e]
-- | compose relations r1 and r2 (r1:2, A->B . B->C -> A->C)
    compRel ::   [e] -> [e] ->  [e]
    (.&.) ::    [e] -> [e] ->  [e]
    (.&.) =  compRel  
    filterRel :: (e -> Bool) -> [e] -> [e]
    -- filterEq :: o -> [e] -> [e]
    -- filterEq v r = filterRel ((v ==) . fst) r
    -- relPair :: (Eq o) =>  [e] -> [e] ->  [(o, (o,o))]
    -- make the obj a pair of the the objects of the two relations


instance Eq o => Rels (o,o) where
    emptyRel2 = []
    add2rel a = (a :)
    converseRel = map swap  
    compRel r1 r2 = [ (a,d) | (a,b) <- r1, (c,d) <- r2, b==c]
    -- relPair r1 r2 = [ (a,(b,d)) |  (a,b) <- r1, (c,d) <- r2, a==c ]
    filterRel cond = filter cond 

-- replOne :: (Eq o) => Rel2 o -> (o,o) -> Rel2 o
-- -- map the value in v to the value rs v
-- replOne rs (a,b) =  zip (repeat a) r
--     where 
--             -- r :: [o]
--             r = (map snd .  filter ((b==).fst) $ rs) 

-- compRel :: (Eq o) =>  Rel2 o -> Rel2 o ->  Rel2 o
-- -- | compose relations r2 . r1 i.e. (B -> C) . (A -> B)
-- -- or: r1;r2 
-- compRel r2 r1 = concat $ map (replOne r2)  r1 


-- compRel r1 r2 = [ (a,d) | (a,b) <- r1, (c,d) <- r2, b==c]
-- compRelZip :: (Eq o) =>  Rel2 o -> Rel2 o ->  [(o, (o,o))]
-- compRelZip r2 r1  = concat $ map (replOneZip r2)

-- replOneZip rs (a,b) = zip (repeat a) r 
--     where 
--         r = zip (repeat b) (map snd . filter ((b==).fst) $ rs)



-- semicolon :: (Eq o) =>  Rel2 o -> Rel2 o ->  Rel2 o
-- | an alternative name for composition of relations, with reverse order (wrt '.')
r1 `semicolon` r2 = compRel r1 r2 

-- --- monadic versions -----------------------------

-- -- rel2 :: (MonadState (CatStore o m) m1, Eq o, Eq m) => m -> m1 (Rel2 o) 
-- rel2 :: (MonadState m1, Eq m2, StateType m1 ~ CatStore o m2) => m2 -> m1 (Rel2 o)
-- rel2 morph1 = do 
--     c <- get 
--     return $ getRel c morph1 
-- inv2 morph1 = do 
--     c <- get 
--     return . map swap $ getRel c morph1 


