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
import UniformBase  

import {-@ HTF_TESTS @-} Uniform.Raster_test
import {-@ HTF_TESTS @-} Uniform.Field_test
import {-@ HTF_TESTS @-} Uniform.Fourier_test
import {-@ HTF_TESTS @-} Uniform.FourierComfort_test
import {-@ HTF_TESTS @-} Uniform.FourierTextBook_test
-- import UniformBase

main = do
    putIOwords ["HTF Testing uniform-geometry test"]
    r <- htfMainWithArgs ["--quiet"] htf_importedTests
    putIOwords ["HTF end \n", showT r]
    
    return r




