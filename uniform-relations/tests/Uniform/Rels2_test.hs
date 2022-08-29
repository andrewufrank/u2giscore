 -----------------------------------------------------------------------------
--
-- Module      :  Test relations with 2 valued 
--          with a minimal Schema:
-- the tag of the sum type is the constructor for the node id 
--

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards
    , UndecidableInstances     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -w #-}

module Uniform.Rels2_test
    where


import UniformBase
-- import Uniform.NaiveTripleStore
-- import Uniform.Object 
-- import Storable.Value
import Uniform.TripleStore
import Uniform.Rels2

-- import  qualified         Algebra.Laws             as Law
import           Test.Framework
import Data.List ( nub ) 

-- import           Test.Invariant           as Rule  
-- import Test.QuickCheck --  (arbitraryBoundedEnum)

--- example code  -- Minimal Schema

data Morph = F | T  | Null  -- for storing a graph S =s,t> T 
    deriving (Show, Read, Ord, Eq, Generic)
instance Zeros Morph where zero = Null
instance NiceStrings Morph where shownice = showT
data Obj = SS Int | TT Int | ZZ 
    deriving (Show, Read, Ord, Eq, Generic)
instance NiceStrings Obj where shownice = showT 

instance Zeros Obj where zero = ZZ

-- data Sobj a = SK a deriving (Show, Read, Ord, Eq, Generic, Zeros)
-- data Tobj a = TK a deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- instance Show a => NiceStrings a  

-- for test
os1 :: Obj
os1 = SS 0
os2 :: Obj
os2 = SS 1
cp1 :: (Morph, (Obj, Obj))
cp1 = (F, (os1,  os2))
cp2 :: (Morph, (Obj, Obj))
cp2 = (F, (os2,   SS 2))

 
pageTriple4cat :: ErrIO ()
pageTriple4cat = do
    putIOwords ["\n [pageTriple4cat"]
    putIOwords ["cp1", shownice cp1]
--     putIOwords ["ts one", showT x1]


test_time1 = do
        res <- runErr $  pageTriple4cat 
                -- return True
        assertEqual (Right ()) res  -- does not produce output

-- need more data , check that fun and invFun are inverse
-- test_eval_Node_e = do 
--     res <- evalStateT ( sRel (Node 'a')) cat11
--     assertEqual [Edge 1, Edge 5] res 

-- test_eval_edge_1s = do 
--     res <- evalStateT ( sInv (Edge 1)) cat11 -- expect Node a
--     assertEqual ("") (showT res) 

res1 :: Text
res1 = "(SS 0,F,SS 1)"
res2 = "(SS 1,F,SS 2)"
res21 = concat'["[", res2, ",", res1, "]"]

-- test_batch_insert21 :: IO ()
-- test_batch_insert21 = assertEqual (concat'["[", res2, ",", res1, "]"])
--     (showT . tsbatch [Ins t2, Ins t1] $ ts0)

-- test_find = assertEqual (concat'["[", res1, "]"]) (showT (tsfind (Just k1, Nothing, Nothing) ts1))
-- test_find2 = assertEqual (concat'["[", res1, "]"]) (showT (tsfind (Just k1, Just r1, Nothing) ts1))
 

-- test_getRel = assertEqual [(SS 0, SS 1), (SS 1, SS 2)] (getRel a1x F)

-- r1s = getRel v2a F 
r2s = [(SS 0,(SS 19)),((SS 1),(SS 18))]

test_converse =  assertEqual [(SS 19, SS 0), (SS 18, SS 1)](converseRel r2s)

r1, r2, r3 :: Rel2 Int -- [(Int,Int)]
r1 = [(0, 10), (1,11), (0,12)]
r2 = [(20,0), (21,2), (20,1), (24,1)]
r3 = [(20,10), (21,20), (20,10), (24,10)]

test_snd = assertEqual [0, 2, 1, 1] (map snd r2) 

test_compRel = assertEqual [(20, 0), (20, 0), (20, 1), (24, 1)] (compRel  (compRel r2 r1) (converseRel r1))
--  nearly == r2
-- r1r2 = nub (compRel r1 r2)
test_compRel2 = assertEqual [(20, 0), (20, 1), (24, 1)] (nub $ compRel  (compRel r2 r1) (converseRel r1))
-- compare with r2 , dropped (21,2)
-- not quite, but nearly 
-- test_comp_x = assertEqual (compRel r2 r1) (compRelx r1 r2)

-- test_relPair1 = assertEqual ([(20, (0, 10)), (20, (0, 10)), (21, (2, 20)), (20, (1, 10)),
--  (20, (1, 10)), (24, (1, 10))] :: [(Int, (Int,Int))]) $ relPair r2 r3 

-- test_semicolon = assertEqual (compRel r1 r2) (r1 `semicolon` r2)
