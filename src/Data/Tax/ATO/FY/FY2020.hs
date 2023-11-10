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

-- | Tax tables for 2019–20 financial year.
module Data.Tax.ATO.FY.FY2020 (FY, fyProxy, tables) where

import Data.Proxy
import Control.Lens (review)

import Data.Tax
import Data.Tax.ATO.Common
import Data.Tax.ATO.PrivateHealthInsuranceRebate
import qualified Data.Tax.ATO.FY.FY2019 as FY2019

type FY = 2020
fyProxy :: Proxy FY
fyProxy = Proxy

help :: (Fractional a, Ord a) => Tax (Money a) (Money a)
help = thresholds'
  [ (45881, 0.01)
  , (52974, 0.01)
  , (56152, 0.005)
  , (59522, 0.005)
  , (63093, 0.005)
  , (66878, 0.005)
  , (70891, 0.005)
  , (75145, 0.005)
  , (79653, 0.005)
  , (84433, 0.005)
  , (89499, 0.005)
  , (94869, 0.005)
  , (100561, 0.005)
  , (106594, 0.005)
  , (112990, 0.005)
  , (119770, 0.005)
  , (126956, 0.005)
  , (134573, 0.005)
  ]

-- | From 1 July 2019, all study and training loans are covered by
-- one set of thresholds and rates.  For backwards compatibility,
-- 'ttHelp' and 'ttSfss' now refer to the same value.
--
tables :: (Ord a, Fractional a) => TaxTables FY a
tables = TaxTables
  (ttIndividualIncomeTax FY2019.tables)
  (medicareLevy (review money 22801))
  medicareLevySurcharge
  help
  help
  (lowIncomeTaxOffset <> lamito)
  privateHealthInsuranceRebateRates

privateHealthInsuranceRebateRates
  :: (Fractional a) => PrivateHealthInsuranceRebateRates a
privateHealthInsuranceRebateRates =
  [ ( 90000, (0.25059, 0.25059), (0.29236, 0.29236), (0.33413, 0.33413) )
  , (105000, (0.16706, 0.16706), (0.20883, 0.20883), (0.25059, 0.25059) )
  , (140000, (0.08352, 0.08352), (0.12529, 0.12529), (0.16706, 0.16706) )
  ]
