-----------------------------------------------------------------------------
--
-- Module      :   main for building temporal gis (etage 2)
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
-- import Uniform.Fourier 
-- import Uniform.FourierRepa
-- import Uniform.FourierTextBook
-- import ExampleData.TerrainLike
-- import Uniform.Field
import Uniform.TemporalGIS
import ExampleData.TempCountryside

main =
  startProg
    (unwords' ["Uniform.TemporalGIS", "the test for temporal data"])
    ( do
        main2
    )

main2 :: ErrIO () 
main2 = do 
 
    -- pageTemporal2
    pageExample4
    return ()
