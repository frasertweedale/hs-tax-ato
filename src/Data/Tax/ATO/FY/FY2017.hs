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

-- | Tax tables for 2016–17 financial year.
module Data.Tax.ATO.FY.FY2017 (tables, individualIncomeTax) where

import Control.Lens (review)
import Data.Tax
import Data.Tax.ATO.Common

-- | In FY2017 the 37% threshold was raised from $80,000 to $87,000
--
individualIncomeTax :: (Fractional a, Ord a) => Tax (Money a) (Money a)
individualIncomeTax = marginal'
  [ (18200, 0.19)
  , (37000, 0.325 - 0.19)
  , (87000, 0.37 - 0.325)
  , (180000, 0.45 - 0.37) ]

temporaryBudgetRepairLevy :: (Fractional a, Ord a) => Tax (Money a) (Money a)
temporaryBudgetRepairLevy = above (review money 180000) 0.02

help, sfss :: (Fractional a, Ord a) => Tax (Money a) (Money a)
help = thresholds'
  [ (54869, 0.04)
  , (61120, 0.005), (67369, 0.005), (70910, 0.005), (76223, 0.005)
  , (82551, 0.005), (86895, 0.005), (95627, 0.005), (101900, 0.005) ]
sfss = thresholds' [(54869, 0.02), (67369, 0.01), (95627, 0.01)]

-- | In FY2017 the 37% threshold was raised from $80,000 to $87,000
--
-- The Temporary Budget Repair Levy, 2% of income above $180,000,
-- occurred in 2014-15, 2015-16, 2016-17.  This was the final year.
--
tables :: (Ord a, Fractional a) => TaxTables a
tables = TaxTables
  (individualIncomeTax <> temporaryBudgetRepairLevy)
  (medicareLevy (review money 21656))
  medicareLevySurcharge
  help
  sfss
  lowIncomeTaxOffset
