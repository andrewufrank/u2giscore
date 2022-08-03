 -----------------------------------------------------------------------------
--
-- Module      :  Test Uniform
-- Copyright   :
--
-- | the test for Uniform to avoid problems there

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

module Uniform.TripleRels_test
    where


import UniformBase
import Uniform.NaiveTripleStore
-- import Uniform.Object 
-- import Storable.Value
import Uniform.TripleStore
import Uniform.TripleRels

-- import  qualified         Algebra.Laws             as Law
import           Test.Framework
-- import           Test.Invariant           as Rule  
-- import Test.QuickCheck --  (arbitraryBoundedEnum)

--- example code 
data Morph = F | T  | Null  -- for storing a graph S =s,t> T 
    deriving (Show, Read, Ord, Eq, Generic)
instance Zeros Morph where zero = Null

data Obj = SS (Sobj Int) | TT (Tobj Int) | ZZ 
    deriving (Show, Read, Ord, Eq, Generic)
instance NiceStrings Obj where shownice = showT 

instance Zeros Obj where zero = ZZ

data Sobj a = SK a deriving (Show, Read, Ord, Eq, Generic, Zeros)
data Tobj a = TK a deriving (Show, Read, Ord, Eq, Generic, Zeros)

-- instance Show a => NiceStrings a where shownice = showT

-- for test
os1 :: Obj
os1 = SS (SK 0)
os2 :: Obj
os2 = SS (SK 1)
cp1 :: (Obj, Morph, Obj)
cp1 = (os1, F, os2)
cp2 :: (Obj, Morph, Obj)
cp2 = (os2, F, SS (SK 2))

-- -------------for test 

v0 :: CatStore Obj Morph  
v0 = catStoreEmpty
v1 :: CatStore   Obj Morph
v1 = catStoreInsert cp1 v0
v2 :: CatStore  Obj Morph
v2 = catStoreInsert cp2 v1
v2a = catStoreInsert (os2, F, SS(SK 0)) v2  -- v2a is rel, sk1 -> sk0 and sk2
v3 :: CatStore  Obj Morph
v3 = catStoreDel cp2 v2

a1 :: [Action (Obj, Morph, Obj)]
a1 = [Ins cp1, Ins cp2]
a1x :: CatStore Obj Morph
a1x = catStoreBatch a1 v0
a2x :: CatStore Obj Morph
a2x = catStoreBatch [Del cp2] a1x



pageTriple4cat :: ErrIO ()
pageTriple4cat = do
    putIOwords ["\n [pageTriple4cat"]
    putIOwords ["cp1", showT cp1]
--     putIOwords ["ts one", showT x1]

    putIOwords ["CatStore empty", showT v0]
    putIOwords ["CatStore v1 with cp1", showT v1]
    putIOwords ["CatStore v2 added cp2, deleted cp1", showT v2]
    putIOwords ["CatStore a1x added batch cp1 cp2", showT a1x]
    putIOwords ["CatStore  a2x", showT a2x]
    putIOwords ["CatStore  v2a", showT v2a]
    -- page2

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

test_empty = assertEqual ("CatStoreK []") (showT (v0))

test_insert1 = assertEqual (concat'["CatStoreK [",  res1, "]"]) (showT v1)
test_insert2 = assertEqual (concat'["CatStoreK [", res2, ",", res1, "]"]) (showT v2)

test_batch_insert = assertEqual (concat'["CatStoreK [", res2, ",", res1, "]"])
    (showT v2)
res1 :: Text
res1 = "(SS (SK 0),F,SS (SK 1))"
res2 = "(SS (SK 1),F,SS (SK 2))"
res21 = concat'["[", res2, ",", res1, "]"]

-- test_batch_insert21 :: IO ()
-- test_batch_insert21 = assertEqual (concat'["[", res2, ",", res1, "]"])
--     (showT . tsbatch [Ins t2, Ins t1] $ ts0)

-- test_find = assertEqual (concat'["[", res1, "]"]) (showT (tsfind (Just k1, Nothing, Nothing) ts1))
-- test_find2 = assertEqual (concat'["[", res1, "]"]) (showT (tsfind (Just k1, Just r1, Nothing) ts1))

test_del0 = assertEqual (concat'["CatStoreK [", res2, ",", res1, "]"])
    (showT  v2)
test_del1 = assertEqual (concat'["CatStoreK [",   res1, "]"])
    (showT v3)

test_Batch = assertEqual (concat'["CatStoreK [", res1, ",", res2, "]"] ) 
    (showT $ a1x)
test_delBatch = assertEqual (concat'["CatStoreK [", res1,  "]"] ) 
    (showT $ a2x)

test_getRel = assertEqual [(SS (SK 0), SS (SK 1)), (SS (SK 1), SS (SK 2))] (getRel a1x F)

r1s = getRel v2a F 
r2s = [(SS (SK 0),(SS (SK 19))),((SS (SK 1)),(SS (SK 18)))]

test_converse =  assertEqual [(SS (SK 1), SS (SK 0)), (SS (SK 2), SS (SK 1))] (converseRel r1s)

-- test_findOneNothing =  assertEqual [] (findOne r1 (SS (SK 2)))
-- test_findOneSK0 =  assertEqual [(SS (SK 1), SS (SK 2))] (findOne r1 (SS (SK 1)))

-- test_findMany = assertEqual [(SS (SK 1), SS (SK 2))] (findMany r1 [(SS (SK 1))])

-- test_findMany2 = assertEqual  [(SS (SK 1), SS (SK 0)), (SS (SK 1), SS (SK 2)),
--  (SS (SK 0), SS (SK 1))] (findMany r2 [(SS(SK 1)), (SS(SK 0))])
--  -- tested that a rel gives in the answer multiple lines

-- (ab, bc',ac) = compRel r2 r1


-- page2 :: ErrIO () 
-- page2  = do 
--     putIOwords ["from comp rel"]
--     putIOwords ["r1\n", showlong r1]
--     putIOwords ["r2\n", showlong r2]
--     putIOwords ["ab\n", showlong ab, "\nbc'\n", showlong bc' ]
