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

-- import Control.Monad.State
-- import Data.List (sort)
-- import GHC.Generics ( Generic )
import Control.Monad.State  
import Data.List ( nub ) 
import AOPPrelude (swap)
-- hiding ((.), concat, filter, zip)
import UniformBase ( snd3, swapPair )
    -- ( Generic, fst3, trd3, errorT, putIOwords, showT, Zeros(zero) )
-- import Uniform.NaiveTripleStore ()
import Uniform.TripleStore -- ( CatStore, unCatStore )
import UniformBase (NiceStrings)
    -- ( Action(..), TripleStore(tsfind, tsinsert, tsdel) )

type Rel o = [](o,o)


-- instance NiceStrings   (Rel o) where 
--     showlong r = intersperse 

getRel :: (Eq m )=> CatStore o m -> m -> Rel o
getRel cat m = map out13 . filter ((m ==) . snd3) . unCatStore $ cat

converseRel :: Rel o -> Rel o
converseRel = map swapPair 

replOne :: (Eq o) => Rel o -> (o,o) -> Rel o
-- map the value in v to the value rs v
replOne rs (a,b) =  zip (repeat a) r
    where 
            -- r :: [o]
            r = (map snd .  filter ((b==).fst) $ rs) 

compRel :: (Eq o) =>  Rel o -> Rel o ->  Rel o
-- | compose relations r2 . r1 i.e. (B -> C) . (A -> B)
-- or: r1;r2 
compRel r2 r1 = concat $ map (replOne r2)  r1 

-- compRel :: (Show o, Eq o) =>  Rel o -> Rel o -> (Rel o, Rel o, Rel o) 
-- -- compose bc . ab 
-- compRel bc ab = (ab,bc',ac) 
--     where 
--         ac = []
--         b_ab = map snd ab 
--         bc' = findMany bc b_ab

semicolon :: (Eq o) =>  Rel o -> Rel o ->  Rel o
-- | an alternative name for composition of relations, with reverse order (wrt '.')
r1 `semicolon` r2 = compRel r2 r1 

--- monadic versions 
-- rel3 :: (MonadState (CatStore o m) m1, Eq o, Eq m) => m -> m1 (Rel o) 
rel3 :: (MonadState m1, Eq m2, StateType m1 ~ CatStore o m2) => m2 -> m1 (Rel o)
rel3 morph1 = do 
    c <- get 
    return $ getRel c morph1 
inv3 morph1 = do 
    c <- get 
    return . map swap $ getRel c morph1 
-- comp3 :: (MonadState m, Eq o) => Rel o -> Rel o -> m (Rel o)
-- comp3 rel1 rel2 = do 
--     c <- get 
--     let res = compRel rel2 rel1 
--     return res 


out13 :: (a, b1, b2) -> (a, b2)
out13 (a,b,c) = (a,c)

-- example data 

-- r1 = [(0, 10), (1,11), (0,12)]
-- r2 = [(20,0), (21,2), (20,1), (24,1)]
-- r2sd = map snd r2 

-- t1 = compRel (converseRel r1) (compRel r1 r2) == r2
-- r1r2 = nub (compRel r1 r2)
-- r1_r1r2 = nub $ compRel (converseRel r1) (compRel r1 r2)
-- -- compare with r2 , dropped (21,2)
-- -- not quite, but nearly 



