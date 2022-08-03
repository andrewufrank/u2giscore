 -----------------------------------------------------------------------------
--
-- Module      :  Test Uniform.NaiveTripleStore_test
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
    , RecordWildCards     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -w #-}

module Uniform.NaiveTripleStore_test
    where


import UniformBase
import Uniform.NaiveTripleStore
-- import Uniform.Object 
-- import Storable.Value

-- import  qualified         Algebra.Laws             as Law
import           Test.Framework
-- import           Test.Invariant           as Rule  
-- import Test.QuickCheck --  (arbitraryBoundedEnum)

-------------- test data

data TestRel = T1 | T2 deriving (Show, Read, Ord, Eq)
type Triple4test = (Key, TestRel, ValueSum) -- deriving (Show, Read, Ord, Eq)

ts0, ts1 :: [Triple4test]
ts0 = tsempty
ts1 = tsinsert (k1,r1,v1) ts0
ts2 :: [(Key, TestRel, ValueSum)]
ts2 = tsinsert t2 ts1

t1 :: (Key, TestRel, ValueSum)
t1 = (k1, r1, v1)
t2 :: (Key, TestRel, ValueSum)
t2= (mkkey "t2", r1, mktext "label2")
t3 :: (Key, TestRel, ValueSum)
t3= (mkkey "t3", r1, mktext "label3")
m1 :: (Maybe Key, Maybe a1, Maybe a2)
m1 = (Just (mkkey "t1"), Nothing, Nothing)
m2 :: (Maybe Key, Maybe a1, Maybe a2)
m2 = (Just (mkkey "t2"), Nothing, Nothing)
k1 :: Key
k1 = mkkey "t1"
r1 :: TestRel
r1 = T1
v1 :: ValueSum
v1 = mktext "label1"

-- needs construction of tests!
test_empty = assertEqual ("[]") (showT (tsempty :: [Triple4test]))

test_insert = assertEqual (concat'["[", res1, "]"] ) (showT ts1)

test_batch_insert = assertEqual (concat'["[", res2, ",", res1, "]"])
    (showT . tsbatch [Ins t2] $ ts1)
res1 :: Text
res1 = "(Key \"t1\",T1,VT (Value \"label1\"))"
res2 = "(Key \"t2\",T1,VT (Value \"label2\"))"
res21 = concat'["[", res2, ",", res1, "]"]

test_batch_insert21 = assertEqual (concat'["[", res2, ",", res1, "]"])
    (showT . tsbatch [Ins t2, Ins t1] $ ts0)

test_find = assertEqual (concat'["[", res1, "]"]) (showT (tsfind (Just k1, Nothing, Nothing) ts1))
test_find2 = assertEqual (concat'["[", res1, "]"]) (showT (tsfind (Just k1, Just r1, Nothing) ts1))

test_del0 = assertEqual (concat'[ res21 ] ) 
    (showT  ts2)
test_del1 = assertEqual (concat'["[", res2, "]"] ) 
    (showT $ tsdel t1 ts2)

test_delBatch = assertEqual (concat'["[", res1, "]"] ) 
    (showT $ tsbatch [Del t2] ts2)



