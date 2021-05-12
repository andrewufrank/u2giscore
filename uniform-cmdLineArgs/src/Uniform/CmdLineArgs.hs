-----------------------------------------------------------------------------
--
-- Module      :  Uniform.CmdLineArgs
--
-----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
-- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}

-- | a miniaml set of
module Uniform.CmdLineArgs (
    module Uniform.CmdLineArgs,
    (<>),
    (<*>),
    Parser (..),
    switch,
    long,
    short,
    help,
    metavar,
    argument,
    str,
    strOption,
    value,
    header,
    helper,
    fullDesc,
    progDesc,
    info,
    execParser,
) where

import Options.Applicative
import Options.Applicative.Builder
import UniformBase
