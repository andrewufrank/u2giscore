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
import Uniform.TripleStore
import Uniform.Rels2
 
main =
  startProg
    (unwords' ["Uniform.Relations", "the test for relatiosn and triple store"])
    ( do
    

        main2
    )

main2 :: ErrIO () 
main2 = do 

    putIOwords ["nothing in main2"]
    return () 
    
