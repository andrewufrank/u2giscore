-----------------------------------------------------------------------------
--
-- Module      :   main for building field
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
-- import Uniform.Point2d
-- import ExampleData.Point2d 
-- import Uniform.FourierComfort
import Uniform.Fourier 
-- import Uniform.FourierRepa
import Uniform.FourierTextBook
import ExampleData.TerrainLike
import Uniform.Field


main =
  startProg
    (unwords' ["Uniform.Field", "the test for field data"])
    ( do
     

        main2
    )

main2 :: ErrIO () 
main2 = do 
    -- pageF1
    -- pageFourier3
    pageField3
    -- pageComfort1
    -- pageFourierTextBool
    -- pageTerrainLike
    
    return ()
