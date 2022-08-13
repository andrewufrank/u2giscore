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

module Uniform.TripleRels
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

-- import Data.List (sort)
-- import GHC.Generics ( Generic )
import Control.Monad.State  
import Data.List ( nub ) 
import UniformBase 
import Uniform.NaiveTripleStore  
import Uniform.TripleStore -- ( CatStore, unCatStore )
    -- ( Action(..), TripleStore(tsfind, tsinsert, tsdel) )

type Rel2 o = [](o,o)
-- | a binary relation


-- instance NiceStrings   (Rel2 o) where 
--     showlong r = intersperse 

getRel :: (Eq m )=> CatStore o m -> m -> Rel2 o
getRel cat m = map out13 . filter ((m ==) . snd3) . unCatStore $ cat

converseRel :: Rel2 o -> Rel2 o
converseRel = map swap  

replOne :: (Eq o) => Rel2 o -> (o,o) -> Rel2 o
-- map the value in v to the value rs v
replOne rs (a,b) =  zip (repeat a) r
    where 
            -- r :: [o]
            r = (map snd .  filter ((b==).fst) $ rs) 

compRel :: (Eq o) =>  Rel2 o -> Rel2 o ->  Rel2 o
-- | compose relations r2 . r1 i.e. (B -> C) . (A -> B)
-- or: r1;r2 
compRel r2 r1 = concat $ map (replOne r2)  r1 

compRelx :: (Eq o) =>  Rel2 o -> Rel2 o ->  Rel2 o
-- | compose relations r1 and r2 (r1:2, A->B . B->C -> A->C)
compRelx r1 r2 = [ (a,d) | (a,b) <- r1, (c,d) <- r2, b==c]
-- compRelZip :: (Eq o) =>  Rel2 o -> Rel2 o ->  [(o, (o,o))]
-- compRelZip r2 r1  = concat $ map (replOneZip r2)

(.&.) :: (Eq o) =>  Rel2 o -> Rel2 o ->  Rel2 o
(.&.) =  compRelx  
-- replOneZip rs (a,b) = zip (repeat a) r 
--     where 
--         r = zip (repeat b) (map snd . filter ((b==).fst) $ rs)
compRelZip :: (Eq o) =>  Rel2 o -> Rel2 o ->  [(o, (o,o))]

compRelZip r2 r1 = [ (a,(b,d)) |  (a,b) <- r1, (c,d) <- r2, a==c ]

semicolon :: (Eq o) =>  Rel2 o -> Rel2 o ->  Rel2 o
-- | an alternative name for composition of relations, with reverse order (wrt '.')
r1 `semicolon` r2 = compRel r2 r1 

--- monadic versions -----------------------------

-- rel2 :: (MonadState (CatStore o m) m1, Eq o, Eq m) => m -> m1 (Rel2 o) 
rel2 :: (MonadState m1, Eq m2, StateType m1 ~ CatStore o m2) => m2 -> m1 (Rel2 o)
rel2 morph1 = do 
    c <- get 
    return $ getRel c morph1 
inv2 morph1 = do 
    c <- get 
    return . map swap $ getRel c morph1 

out13 :: (a, b1, b2) -> (a, b2)  -- todo
out13 (a,b,c) = (a,c)

