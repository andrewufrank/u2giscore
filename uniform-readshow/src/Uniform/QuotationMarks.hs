{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
--------------------------------------------------------------
--
-- Module      :  Uniform.QuotationMarks
--
-- | achieving a uniform use of quotation marks
-- respectively avoiding them when the content may
-- quotationMarks
-------------------------------------------------------------------



module Uniform.QuotationMarks (module Uniform.QuotationMarks
        )  where

import           UniformBase
import           Data.Proxy
import           Data.Text   (Text, unpack)

class Print a where
  makeString :: a -> String

data Name = NString | NText | NShow
type family Choose a where
  Choose [Char] = 'NString
  Choose Text = 'NText
  Choose _ = 'NShow

class Print' (n :: Name) a where
  makeString' :: proxy n -> a -> String

instance (Choose a ~ n, Print' n a) => Print a where
  makeString = makeString' (Proxy :: Proxy n)

instance a ~ String => Print' 'NString a where
  makeString' _ = id

instance a ~ Text => Print' 'NText a where
  makeString' _ = unpack

instance Show a => Print' 'NShow a where
  makeString' _ = show


example :: ErrIO ()
example = do
    putIOwords ["QuotationMarks"]
