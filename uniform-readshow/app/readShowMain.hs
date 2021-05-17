    {-# LANGUAGE
    MultiParamTypeClasses
    , OverloadedStrings
    , TypeFamilies

    #-}
-----------------------------------------------------------------
--
-- Module      :   main - prefer lower case file names to avoid the Main
----------------------------------------------------------------------

module Main     where  -- must be Main to output code

import UniformBase



main = startProg
            "example"
            mainErrIO 

mainErrIO :: ErrIO ()
mainErrIO = do 
        putIOwords ["start!"]