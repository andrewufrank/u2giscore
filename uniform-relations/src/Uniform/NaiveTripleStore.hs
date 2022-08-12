-----------------------------------------------------------------------------
--
-- Module      :  a naive implementation of  a __typed__ triple store
-- i.e. the minimal and most simple implementation as list
-- the triples are Key - Rel - Val (to reduce confusion later...)
-- corresponding to Subj - Prop - Ojb
-- read and write ok (see TripleFiles.hs)
-- strictly not optimized (search uses filter (O n))
-- 
-- here only data structure and construction (insert,delete)
-- query as relation algebra in NaiveTripleRelations.hs 

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
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Uniform.NaiveTripleStore 
    -- ( TripleStore (..)
    -- , Triple4test
    -- , Action (..)
    -- -- , pageNT
    -- , filterTriple
    -- , toMaybes
    -- , wrapIns
    -- -- for tests
    -- ,ts0,ts1,ts2,t1,t2,m1, k1,r1
    -- )    
    where

import UniformBase
import UniformBase (fst3, snd3, trd3)
-- import Uniform.Error
-- import Data.List.Extra

-- import Storable.Value
-- import Test.Framework.TestManager (testSuiteAsTest)


-- type Val = ValueSum 

-- | a store for typed triples 
class () => TripleStore o p v  where
    tsempty :: [(o,p,v)]
    tsinsert :: (o,p,v) -> [(o,p,v)] -> [(o,p,v)]
    -- tsdel :: (Maybe o, Maybe p, Maybe v) -> [(o,p,v)] -> [(o,p,v)]
    tsdel :: ( o,  p,  v) -> [(o,p,v)] -> [(o,p,v)]
    tsfind :: (Maybe o, Maybe p, Maybe v) -> [(o,p,v)] -> [(o,p,v)]
    tsbatch :: [Action (o,p,v)] -> [(o,p,v)] -> [(o,p,v)]
    tsinsertMany :: [(o,p,v)] -> [(o,p,v)] -> [(o,p,v)]

-- rest for tests

data Action a = Ins a | Del a
        deriving (Show, Read, Ord, Eq)

-- | instance with fixed key (for subject) and predicate (relation)
instance (Eq o,Eq p, Eq v) => TripleStore o p v where 
    tsempty = []
    tsinsert t@(o,p,v) = ( t :)
    tsdel t@(mo, mp, mv) = filter (not . filterTriple (toMaybes t) )
    tsfind t@(mo, mp, mv) =  filter (filterTriple t)
    tsbatch [] ts = ts
    tsbatch ((Ins t) : as) ts = tsinsert t . tsbatch as $ ts
    tsbatch ((Del t) : as) ts = tsdel t . tsbatch as $ ts
    tsinsertMany = tsbatch . map wrapIns
    -- tsinsertMany [] = id
    -- tsinsertMany (t:ts) = tsinsert t . tsinsertMany ts 

wrapIns :: a -> Action a
wrapIns a =   Ins  a



toCond :: Eq v => Maybe v -> (v -> Bool)
toCond (Nothing) = const True
toCond (Just v) = (v==)

toMaybes :: (a1, a2, a3) -> (Maybe a1, Maybe a2, Maybe a3)
toMaybes (s,p,o) = (Just s, Just p, Just o)

filterTriple :: (Eq a, Eq b, Eq v) =>
            (Maybe a, Maybe b, Maybe v) -> (a, b, v) -> Bool
filterTriple (mo, mp, mv) t = (toCond mo . fst3 $ t)
                            && (toCond mp . snd3 $ t)
                            && (toCond mv . trd3 $ t)



