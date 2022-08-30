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
    ( module Uniform.TripleStore
    , module Uniform.Rels2)

    where

import Control.Monad.State
import UniformBase  
    -- ( Generic, fst3, trd3, errorT, putIOwords, showT, Zeros(zero) )
import Uniform.Rels2

 
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


-- wrapping 

newtype CatStore o p = CatStoreK [Tup3 o p] 
                     deriving (Show, Read, Eq)

instance (Show o, Show p) =>  NiceStrings (CatStore o p) where 
    shownice (CatStoreK oms) = (s2t "\nCatStoreK [\n") <> (showAsLines) oms <> "]\n"



class CatStores o p where
    catStoreEmpty :: CatStore o p
    catStoreInsert :: Tup3 o p -> CatStore o p  -> CatStore o p
    catStoreDel :: Tup3 o p -> CatStore o p  -> CatStore o p 
--     catStoreFind :: (Maybe o, Maybe p, Maybe o) -> CatStore o p  -> [Tup3 o p]
    catStoreBatch :: [Action  (p,Tup2 o)] -> CatStore o p  -> CatStore o p 
    catStoreBatch [] ts = ts
    catStoreBatch ((Ins t) : as) ts = catStoreInsert t . catStoreBatch as $ ts
    catStoreBatch ((Del t) : as) ts = catStoreDel t . catStoreBatch as $ ts

    unCatStore :: CatStore o p -> [Tup3 o p]  
    -- unCatStore (CatStoreK as) = as
    wrapCatStore :: ([Tup3 o p] -> [Tup3 o p]) -> CatStore o p-> CatStore o p
    -- wrapCatStore f = CatStoreK . f . unCatStore  -- not a functor!"\n\t]"




instance (Eq o, Eq p, Rel2s o) => CatStores o p where
    catStoreEmpty =(CatStoreK []) :: CatStore o p
    catStoreInsert t  = wrapCatStore  ((:) t)  
    -- catStoreDel t = wrapCatStore (del2rel t) 
--     catStoreFind t = tsfind t . unCatStore
    -- unCatStore :: CatStore o p -> [Tup3 o p]  
    unCatStore (CatStoreK as) = as
    -- wrapCatStore :: ([Tup3 o p] -> [Tup3 o p]) -> CatStore o p-> CatStore o p
    wrapCatStore f = CatStoreK . f . unCatStore  -- not a functor!"\n\t]"


-- --- monadic versions -----------------------------
-- should be class to work for unwrapped and wrapped 

class Rels2monadic m p o where 
-- | monadic operatios to get relations and process
    rel2 :: p -> m [Tup2 o]
    -- | get binary relation for a property
    inv2 :: p -> m [Tup2 o]
    -- | get inverse relation for a property

instance (MonadState m, Eq p, Eq o, Rel2s o, StateType m ~ CatStore o p) => Rels2monadic m p o where 
 
    rel2 morph1 = do 
        c <- get 
        return $ get3Rel morph1 . unCatStore $ c
 
    inv2 morph1 = do 
        c <- get 
        return . map swap $ get3Rel morph1 . unCatStore $  c


-- relPair :: (Eq o) =>  [Tup2 o] -> [Tup2 o] ->  [(o, Tup2 o)]
-- relPair r1 r2 = [ (a,(b,d)) |  (a,b) <- r1, (c,d) <- r2, a==c ]
-- make the obj a pair of the the objects of the two relations
