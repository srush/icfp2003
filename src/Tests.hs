{-# LANGUAGE NoMonomorphismRestriction  #-}

module Tests where 

import Test.QuickCheck
import Test.HUnit

import Data.Bits
import Data.Int

value = 5480 - (4 + ((12*5480) `shiftR` 16)+((((5480*5480) `shiftR` 16) * 24) `shiftR` 16)) + 24

