{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -w #-}
--------------------------------------------------------------
--
-- Module      :  Uniform.Example
--
-- | a miniaml set of
-------------------------------------------------------------------

 

module Uniform.Example (module Uniform.Example
        )  where

import           UniformBase

example :: ErrIO ()
example = do 
    putIOwords ["example"]


