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

module Data.Tax.ATO.FY.FY2019 (tables) where

import Data.Tax
import Data.Tax.ATO.Common
import qualified Data.Tax.ATO.FY.FY2018 as FY2018

-- | In FY2019 the 37% threshold was increased from $87,000 to $90,000.
individualIncomeTax :: (Fractional a, Ord a) => Tax (Money a) (Money a)
individualIncomeTax = marginal'
  [ (18200, 0.19)
  , (37000, 0.325 - 0.19)
  , (90000, 0.37 - 0.325)
  , (180000, 0.45 - 0.37) ]

help, sfss :: (Fractional a, Ord a) => Tax (Money a) (Money a)
help = thresholds'
  [ (51957, 0.02),  (57730, 0.02)
  , (64307, 0.005), (70882, 0.005),  (74608, 0.005),  (80198, 0.005)
  , (86856, 0.005), (91426, 0.005), (100614, 0.005), (107214, 0.005) ]
sfss = thresholds' [(51957, 0.02), (64307, 0.01), (91426, 0.01)]

-- | The 37% threshold was increased from $87,000 to $90,000.
--
-- The new /low and middle income tax offset (LAMITO)/ was
-- introduced, in addition to LITO.
--
-- NOTE: Medicare levy thresholds have not yet been announced.
-- Re-using thresholds from FY2018.
--
tables :: (Ord a, Fractional a) => TaxTables a
tables = TaxTables
  individualIncomeTax
  (ttMedicareLevy FY2018.tables)
  medicareLevySurcharge
  help
  sfss
  (lowIncomeTaxOffset <> lamito)
