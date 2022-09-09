-----------------------------------------------------------------------------
--
-- Module      :   main for building relations and triples 
-----------------------------------------------------------------------------
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



 
import UniformBase
import Uniform.SchemaFoundation
 
import FivePoints
import FourteenPoints

main =
  startProg
    (unwords' ["Uniform.hqSchema Main", "the test for graphics and building geometry from a schema "])
    ( do

        main2
    )

main2 :: ErrIO () 
main2 = do 
 
    mainMake5points
    mainMakeFourteenPoints
    return () 
 