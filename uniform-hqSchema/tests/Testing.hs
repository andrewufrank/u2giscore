-----------------------------------------------------------------------------
--
-- Module      :   top tests for hqload
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
import {-@ HTF_TESTS @-} HQschema.LoadTesselation_test
import UniformBase

main = do
    putIOwords ["HTF Testing HQload test"]
    r <- htfMainWithArgs ["--quiet"] htf_importedTests
    putIOwords ["HTF end \n", showT r]
    
    return r




