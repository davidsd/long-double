-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.LongDouble
-- Copyright   :  (C) 2018,2020 Claude Heiland-Allen
-- License     :  BSD3
-- Maintainer  :  Claude Heiland-Allen <claude@mathr.co.uk>
-- Stability   :  experimental
-- Portability :  non-portable 
--
-- This module re-exports the default platform-specfic ABI for C's long double.
module Numeric.LongDouble ( module Numeric.LongDouble.ARM_128 ) where

import Numeric.LongDouble.ARM_128
  ( LongDouble()
  , truncate', round', ceiling', floor'
  , fromDouble, toDouble, fromInt, toInt
  )
