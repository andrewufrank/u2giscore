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
--     (
--       CPoint 
--     , CatStore (..), CatStores (..)
--     , Action (..)  -- from NaiveTripleStore
--     , isSingleton, getSingle1, getSingle3
--     , getTarget3, getTarget1
--     , openSingleton
--     , find2fun, find2rel
--     , find2funR, find2relR
--     , MorphSel (..) 
--     , wrapIns
--     ---- for tests
--     -- , pageTriple4cat
    
--     -- , Morph (..), Obj (..), Sobj(..), Tobj(..)
--     -- , v0, v1, v2, v3, a1x, a2x
-- --     Uniforms (..), Uniform (..), UniformState
-- --     -- , ObjID, makeObjID
-- -- --    , module Uniform.NaiveTriplestore
-- -- --    ,  Rel4Test  (..)
-- --     -- , B4val (..), proxyB4val, proxyDouble
-- --     , Val
-- --     , Vals (..)
-- --     -- ,  unVal4, isVal4
-- --     , Row (..)
-- -- --    , newNaiveStore -- for testing
--     )    
    where

-- import Control.Monad.State
-- import Data.List (sort)
-- import GHC.Generics ( Generic )
-- import Control.Monad.State  

import UniformBase  
    -- ( Generic, fst3, trd3, errorT, putIOwords, showT, Zeros(zero) )
import Uniform.NaiveTripleStore
import UniformBase (NiceStrings)
    -- ( Action(..), TripleStore(tsfind, tsinsert, tsdel) )

type CPoint o m =  (o,m,o)  -- a relation line (m - is predicate (aka morph), o is an subject/value, here object)

newtype CatStore o m = CatStoreK [CPoint o m] 
                     deriving (Show, Read, Eq)

instance (Show o, Show m) =>  NiceStrings (CatStore o m) where shownice (CatStoreK oms) = (s2t "\nCatStore\n") <> (showlong) oms

-- instance (Show a, Show b, Show c) => NiceStrings (a,b,c) where 
--             showNice a = showT a 

-- instance (Show a ) => NiceStrings [a] where 
--             showNice a = intersperse "\n"   map  a 

unCatStore :: CatStore o m -> [CPoint o m]
unCatStore (CatStoreK as) = as
wrapCatStore :: ([CPoint o m] -> [CPoint o m]) -> CatStore o m-> CatStore o m
wrapCatStore f = CatStoreK . f . unCatStore  -- not a functor!

-- -- -- type CatStoreState rel = State (CatStore rel)

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


--------------- old

-- ---- helper for queries
-- isSingleton :: Foldable t => t a -> Bool
-- isSingleton a = 1 == length a
-- openSingleton :: Show p => [p] -> p
-- openSingleton [a] = a
-- openSingleton x = errorT ["openSingleton - not", showT x ]

-- getSingle1 :: (Show a, Show b, Show c) => [(a,b,c)] -> a
-- getSingle1 = fst3 . openSingleton
-- -- ^ unwrap the first if singleton 
-- getSingle3 :: (Show a, Show b, Show c) => [(a,b,c)] -> c
-- getSingle3 = trd3 . openSingleton


-- getTarget3 :: [(a, b, c)] -> c
-- -- | get target (pos3) from a singleton result 
-- -- just a helper
-- getTarget3 cps = trd3 . head  $ cps 

-- getTarget1 :: [(a, b, c)] -> a
-- -- | get target (pos 1 ) from a singleton result 
-- -- just a helper
-- getTarget1 cps = fst3 . head  $ cps 

-- -- ^ a monadic wrapper for catStoreFind applied to state
-- -- find :: (MonadState (CatStore o m2) m1, Eq o, Eq m2) =>
-- --         (Maybe o, Maybe m2, Maybe o) -> m1 [CPoint o m2]
-- -- find t = do 
-- --     c <- get
-- --     let res = catStoreFind t c 
-- --     return  res 

-- -- | the different views of a triple - returns for Fun and Inv are different!
-- data MorphSel = Forward | Inv   
--     deriving (Show, Read, Ord, Eq, Generic)
    
-- -- data MorphSel =  Rel | InvRel 
-- --     deriving (Show, Read, Ord, Eq, Generic)
    

-- -- ^ a monadic wrapper for catStoreFind applied to state
-- -- applies an unwrapper
-- -- a relation dom -> codom 
-- find2rel :: (MonadState (CatStore o m) m1, Eq o, Eq m) => 
--     MorphSel ->
--     o -> 
--     m ->
--     (o -> codom)-> m1 [codom]
-- find2rel Forward
--     s p untag = do 
--     c <- get
--     let res = catStoreFind (Just s, Just p, Nothing) c 
--     return . map  (untag . trd3) $ res 
-- find2rel Inv o p untag = do 
--     c <- get
--     let res = catStoreFind (Nothing, Just p, Just o) c 
--     return . map  (untag . fst3) $ res 
-- -- find2rel _ _ _ _= errorT ["find2 can only be used for Rel and InvRel, for functions use find2fun"]

-- -- ^ a monadic wrapper for catStoreFind applied to state
-- -- applies an unwrapper and checks for single result 
-- -- a function dom -> codom 
-- -- find2fun :: (MonadState (CatStore o m2) m1, Eq o, Eq m2, Show codom) => MorphSel ->
-- --         ObjPoint -> 
-- --         MorphPoint ->
-- --          (CPoint o m2 -> codom)-> m1 codom
-- find2fun :: (MonadState (CatStore o m) m1, Show codom, Eq o, Eq m, Eq codom) =>
--     MorphSel
--     -> o
--     -> m
--     -> (o -> codom)
--     -> m1 codom
-- find2fun Forward s p untag =  
--     fmap openSingleton $ find2rel Forward s p untag
-- find2fun Inv s o untag =  fmap openSingleton $ find2rel Inv s o untag
-- find2fun_ _ _ _= errorT ["find2fun can only be used for Fun and InvFun, for relations use find2rel"]

-- -- | find a relation based on a Taged value, return taged values
-- -- morph (predicate) before obj (subject)
-- find2relR :: (MonadState (CatStore o m) m1, Eq o, Eq m, Show o) => 
--     MorphSel -> m ->
--     o -> 
--     m1 [o]
-- find2relR Forward p s   = do 
--     c <- get
--     let res = catStoreFind (Just s, Just p, Nothing) c 
--     return . map trd3 $ res 
-- find2relR Inv p o   = do 
--     c <- get
--     let res = catStoreFind (Nothing, Just p, Just o) c 
--     return . map fst3   $ res 

-- -- -- | find a relation based on a Taged value, return taged values
-- -- -- morph (predicate) before obj (subject)
-- -- find2relM :: (MonadState (CatStore o m) m1, Eq o, Eq m, Show o) => 
-- --     MorphSel -> m ->
-- --     o -> 
-- --     m1 ()
-- -- find2relM Forward p s   =  do
-- --     c <- get 
-- --      (\s -> map trd3 . (catStoreFind (Just s, Just p, Nothing)))   
-- --     -- return . map trd3 $ res 

-- -- find2relM Inv p o   = do 
-- --     c <- get
-- --     let res = catStoreFind (Nothing, Just p, Just o) c 
-- --     return . map fst3   $ res 
-- -- | find a value for a function, based on a result from a query
-- find2funR :: (MonadState (CatStore o m) m1,   Eq o, Eq m, Show o) =>
--     MorphSel
--     -> m
--     -> o
--     -> m1 o
-- find2funR Forward p s  =  
--     fmap openSingleton $ find2relR Forward p s 
-- find2funR Inv p o  =  fmap openSingleton $ find2relR Inv p o  
-- find2funR_ _ _ _= errorT ["find2fun can only be used for Fun and InvFun, for relations use find2rel"]

-- -- -- ^ a monadic wrapper for catStoreFind applied to state
-- -- -- applies an unwrapper
-- -- -- a relation dom -> codom 
-- -- find2relR :: (MonadState (CatStore o m) m1, Eq o, Eq m) => 
-- --     MorphSel -> m ->
-- --     o -> 
-- --     m1 [o]
-- -- find2relR Forward
-- --     p s  = do 
-- --     c <- get
-- --     let res = catStoreFind (Just s, Just p, Nothing) c 
-- --     return . map  (trd3) $ res 
-- -- find2relR Inv p o = do 
-- --     c <- get
-- --     let res = catStoreFind (Nothing, Just p, Just o) c 
-- --     return . map  (fst3) $ res 

-- type CPoint o m =  (o,m,o)  -- a function for a point
-- --             -- deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- -- -- type CPointGraph = CPoint Obj Morph


-- contFindfun  = id
-- -- find with an input from a previous findrel 




