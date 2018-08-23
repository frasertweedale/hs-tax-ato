-- This file is part of hs-tax-ato
-- Copyright (C) 2018  Fraser Tweedale
--
-- hs-tax-ato is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Data.Tax.ATO.FY.FY2018 (tables) where

import Control.Lens (review)
import Data.Tax
import Data.Tax.ATO.Common
import qualified Data.Tax.ATO.FY.FY2017 as FY2017

help, sfss :: (Fractional a, Ord a) => Tax (Money a) (Money a)
help = thresholds'
  [ (55874, 0.04)
  , (62293, 0.005), (68603, 0.005), (72208, 0.005),  (77619, 0.005)
  , (84063, 0.005), (88487, 0.005), (97378, 0.005), (103766, 0.005) ]
sfss = thresholds' [(55874, 0.02), (68603, 0.01), (97378, 0.01)]

-- | Individual tax rates unchanged from 2017.
--
-- The /temporary budget repair levy/ no longer applies.
--
tables :: (Ord a, Fractional a) => TaxTables a
tables = TaxTables
  FY2017.individualIncomeTax
  (medicareLevy (review money 21980))
  medicareLevySurcharge
  help
  sfss
  lowIncomeTaxOffset
