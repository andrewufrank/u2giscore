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
{-# LANGUAGE TemplateHaskell    #-}

module Uniform.TripleStore
    ( module Uniform.TripleStore
    , module Uniform.Rels2)

    where

import Control.Monad.State
import UniformBase  
    -- ( Generic, fst3, trd3, errorT, putIOwords, showT, Zeros(zero) )
import Uniform.Rels2
-- import Control.Lens 
-- import Control.Lens.TH
 
type Tup3 o p = (p,Tup2 o)

-- makeLenses ''Tup3
-- does not give more thand fst, first
 


class (Eq p, Eq o) => Rels3 p o where 
-- | predicate prefixed to binary relation
    get3Rel :: p -> [(p,Tup2 o)] -> [Tup2 o]
    -- | get the binary Rel2
    doTo3Rel :: p -> ([Tup2 o] -> [Tup2 o]) -> [(p,Tup2 o)] -> [Tup2 o]
    -- | get the binary Rel2 and operate on it

    -- converse3Rel :: (Eq p, Eq o, BinRels o) => p -> [(p, (o, o))] -> [(o, o)]
    -- filter3Rel :: (Eq p, Eq o,  BinRels o) => p -> ((o, o) -> Bool) -> [(p, (o, o))] -> [(o, o)]
    
instance (Eq p, Eq o) => Rels3 p o where 
    get3Rel p = map snd . filter ((p==).fst) 
    doTo3Rel p f = f . get3Rel p 

    -- converse3Rel p = doTo3Rel p converse2Rel  
    -- filter3Rel p cond = doTo3Rel p (filter2Rel cond)


-- | the store type for the state monad
newtype Store o p = Store  [Tup3 o p] 
                     deriving (Show, Read, Eq)

-- -- | wrapped
-- newtype CatStore o p = CatStoreK [Tup3 o p] 
--                      deriving (Show, Read, Eq)

instance (Show o, Show p) =>  NiceStrings (Store o p) where 
    shownice (Store oms) = (s2t "\nStore [\n") <> (showAsLines) oms <> "]\n"

data Action a = Ins a | Del a
        deriving (Show, Read, Ord, Eq)
wrapIns :: a -> Action a
wrapIns a =   Ins  a


class Stores  st o p where
-- | a wrapped form of storing triples 
    -- could be family with Tup3 and Maybe3
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


instance (Eq o, Eq p, BinRels o) => Stores  Store o p  where
    storeEmpty =(Store []) 
    storeInsert t  = wrapStore  ((:) t)  
    -- storeDel t = wrapStore (del2rel t) 
--     storeFind t = tsfind t . unStore
    -- unStore :: Store o p -> [Tup3 o p]  
    unStore (Store as) = as
    -- wrapStore :: ([Tup3 o p] -> [Tup3 o p]) -> Store o p-> Store o p
    wrapStore f = Store . f . unStore  


-- instance (Eq o, Eq p, BinRels o) => Stores CatStore o p where
--     storeEmpty =(CatStoreK [])  
--     storeInsert t  = wrapStore  ((:) t)  
--     -- storeDel t = wrapStore (del2rel t) 
-- --     storeFind t = tsfind t . unStore
--     -- unStore :: Store o p -> [Tup3 o p]  
--     unStore (CatStoreK as) = as
--     -- wrapStore :: ([Tup3 o p] -> [Tup3 o p]) -> Store o p-> Store o p
--     wrapStore f = CatStoreK . f . unStore  -- not a functor!"\n\t]"


-- --- monadic versions -----------------------------
-- should be class to work for unwrapped and wrapped 

class BinRels3Monadic m p o where 
-- | monadic operatios to get relations and process
    rel3 :: p -> m [Tup2 o]
    -- | get binary relation for a property
    inv3 :: p -> m [Tup2 o]
    -- | get inverse relation for a property

instance (Stores st o p, MonadState m, Eq p, Eq o, BinRels o, StateType m ~ st o p) => BinRels3Monadic m p o where 
 
    rel3 morph1 = do 
        c <- get 
        return $ get3Rel morph1 . unStore $ c
 
    inv3 morph1 = do 
        c <- get 
        return . map swap $ get3Rel morph1 . unStore $  c


-- relPair :: (Eq o) =>  [Tup2 o] -> [Tup2 o] ->  [(o, Tup2 o)]
-- relPair r1 r2 = [ (a,(b,d)) |  (a,b) <- r1, (c,d) <- r2, a==c ]
-- make the obj a pair of the the objects of the two relations
