-----------------------------------------------------------------------------
--
-- Module      :   top tests for layout
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

    {-# LANGUAGE
    MultiParamTypeClasses
    , TypeSynonymInstances
--    , FunctionalDependencies
    , FlexibleInstances
    , FlexibleContexts
    , ScopedTypeVariables
--    , UndecidableInstances
    , OverloadedStrings
    , TypeFamilies

    #-}

module Main     where



import Test.Framework
-- import {-@ HTF_TESTS @-} Uniform.Geometry_test
import {-@ HTF_TESTS @-} Uniform.Point2d_test
-- import {-@ HTF_TESTS @-} Uniform.TessHQ_test
-- import {-@ HTF_TESTS @-} Uniform.TessVoronoi_test
import UniformBase

main = do
    putIOwords ["HTF Testing uniform-geometry test"]
    r <- htfMainWithArgs ["--quiet"] htf_importedTests
    putIOwords ["HTF end \n", showT r]
    
    return r




