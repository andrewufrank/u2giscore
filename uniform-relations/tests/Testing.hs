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
import {-@ HTF_TESTS @-} Uniform.TripleStore_test
import {-@ HTF_TESTS @-} Uniform.Rels2_test
import UniformBase

main = do
    putIOwords ["HTF Testing uniform-relations test"]
    r <- htfMainWithArgs ["--quiet"] htf_importedTests
    putIOwords ["HTF end \n", showT r]
    
    return r




