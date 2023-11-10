-- This file is part of hs-tax-ato
-- Copyright (C) 2020, 2021  Fraser Tweedale
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
module Data.Tax.ATO.FY.FY2021 (FY, fyProxy, tables, individualIncomeTax) where

import Data.Proxy
import Control.Lens (review)

import Data.Tax
import Data.Tax.ATO.Common
import Data.Tax.ATO.PrivateHealthInsuranceRebate

type FY = 2021
fyProxy :: Proxy FY
fyProxy = Proxy

-- | In 2020–21 the 32.5% threshold was increased from $37,000 to
-- $45,000, and the 37% threshold was increased from $90,000 to
-- $120,000.
individualIncomeTax :: (Fractional a, Ord a) => Tax (Money a) (Money a)
individualIncomeTax = marginal'
  [ (18200, 0.19)
  , (45000, 0.325 - 0.19)
  , (120000, 0.37 - 0.325)
  , (180000, 0.45 - 0.37) ]

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

tables :: (Ord a, Fractional a) => TaxTables FY a
tables = TaxTables
  individualIncomeTax
  (medicareLevy (review money 23226))
  medicareLevySurcharge
  help
  help
  (lowIncomeTaxOffset2021 <> lamito)
  privateHealthInsuranceRebateRates

privateHealthInsuranceRebateRates
  :: (Fractional a) => PrivateHealthInsuranceRebateRates a
privateHealthInsuranceRebateRates =
  [ ( 90000, (0.25059, 0.24608), (0.29236, 0.28710), (0.33413, 0.32812) )
  , (105000, (0.16706, 0.16405), (0.20883, 0.20507), (0.25059, 0.24608) )
  , (140000, (0.08352, 0.08202), (0.12529, 0.12303), (0.16706, 0.16405) )
  ]
