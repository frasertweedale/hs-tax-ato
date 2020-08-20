-- This file is part of hs-tax-ato
-- Copyright (C) 2020  Fraser Tweedale
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

{-# LANGUAGE DataKinds #-}

-- | Tax tables for 2020–21 financial year.
module Data.Tax.ATO.FY.FY2021 (tables) where

import Data.Tax
import Data.Tax.ATO.Common
import Data.Tax.ATO.Days
import qualified Data.Tax.ATO.FY.FY2020 as FY2020

help :: (Fractional a, Ord a) => Tax (Money a) (Money a)
help = thresholds'
  [ (46620, 0.01)
  , (53827, 0.01)
  , (57056, 0.005)
  , (60480, 0.005)
  , (64109, 0.005)
  , (67955, 0.005)
  , (72032, 0.005)
  , (76355, 0.005)
  , (80936, 0.005)
  , (85793, 0.005)
  , (90940, 0.005)
  , (96397, 0.005)
  , (102180, 0.005)
  , (108310, 0.005)
  , (114708, 0.005)
  , (121699, 0.005)
  , (129000, 0.005)
  , (136740, 0.005)
  ]

-- | NOTE: Medicare levy thresholds have not yet been announced.
-- Re-using thresholds from FY2020.
--
tables :: (Ord a, Fractional a) => TaxTables 'CommonYear a
tables = TaxTables
  (ttIndividualIncomeTax FY2020.tables)
  (ttMedicareLevy FY2020.tables)
  medicareLevySurcharge
  help
  help
  (lowIncomeTaxOffset <> lamito)
