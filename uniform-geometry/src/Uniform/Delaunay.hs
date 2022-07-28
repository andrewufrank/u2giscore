-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Delaunay

-- collect the data to store in triple storage
-- the data is not yet typed, in order no to introduce the triple store types here
-- but every relation is a single list of pairs.
-- the ids are types (N for NodeID, )
-----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GeneralisedNewtypeDeriving  #-}


{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.Delaunay 
    ( module Uniform.Delaunay
    , module Uniform.PointData
    , module Uniform.Point
    , module Linear.V2
    , module Control.Lens
        )  where

import UniformBase
import Uniform.Point2d
import Uniform.Point2dData
import qualified Data.Map as Map 

-- import Vector
import Linear.V2
import qualified Linear.Vector as Lin
import Control.Lens 
import GHC.Generics
import Delaunay.Types


-- ----------------------------for nodes 
newtype NodeID = N Integer 
    deriving (Show, Read, Ord, Eq, Generic, Zeros, Enum)
type NodeName = Text 
type Coord = Double

p2_node_name :: NodeID -> P2 -> (NodeID, NodeName)
p2_node_name i p = (i, showT $ p ^. p2id)
type Node_Name = (NodeID, NodeName)

p2s_node_name :: Integer -> [P2] -> [Node_Name]
p2s_node_name off1 p2s = zipWith p2_node_name (map N [off1 ..]) p2s

node_name_five :: [Node_Name]
node_name_five = p2s_node_name 50 fiveP2
node_name_four = p2s_node_name 40 fourP2

p2_node_x :: NodeID -> P2 -> (NodeID, Coord)
p2_node_x i p = (i,    p ^. v2._x)
p2_node_y i p = (i,    p ^. v2._y)
p2s_node_x :: Integer -> [P2] -> [(NodeID, Coord)]
p2s_node_x off1 p2s = zipWith p2_node_x (map N [off1 ..]) p2s
p2s_node_y off1 p2s = zipWith p2_node_y (map N [off1 ..]) p2s
node_x_four :: [(NodeID, Coord)]
node_x_four = p2s_node_x 40 fourP2
node_y_four = p2s_node_y 40 fourP2




