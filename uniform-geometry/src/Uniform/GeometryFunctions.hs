-----------------------------------------------------------------------------
--
-- Module      :  Uniform.GeometryFunctions
-- | Poind2d with ID and V2 for coordinates
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
-- {-# LANGUAGE TypeApplications     #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.GeometryFunctions
    ( module Uniform.GeometryFunctions
    , module Uniform.Point
    , module Linear.V2
    , module Control.Lens
    , P2
        ) 
         where

import UniformBase
import Uniform.Point 
-- import Vector
import Linear.V2
import qualified Linear.Vector as Lin
import Control.Lens 
import GHC.Generics

import qualified Data.Geometry as H
import Data.Ext
import qualified Data.Geometry.Point as HP 

ccw_test a b c = HP.CCW == H.ccw (toHPoint a) (toHPoint b) (toHPoint c)

class ToHPoint2 a where 
    toHPoint :: a -> H.Point 2 Double

instance ToHPoint2 (V2 Double) where 
    toHPoint = v2toHPoint 
