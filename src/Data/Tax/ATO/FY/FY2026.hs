-- This file is part of hs-tax-ato
-- Copyright (C) 2025  Fraser Tweedale
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

-- | Tax tables for 2025–26 financial year.
--
-- These data are NOT YET FINAL.  We await the federal
-- budget to confirm Medicare Levy thresholds, and the PHI
-- rebate adjustment factor (~1 April 2026), and there could
-- be other changes.
--
module Data.Tax.ATO.FY.FY2026 (FY, fyProxy, tables) where

import Data.Proxy
import Data.Tax
import Data.Tax.ATO.Common
import Data.Tax.ATO.PrivateHealthInsuranceRebate
import qualified Data.Tax.ATO.FY.FY2025 as FY2025

type FY = 2026
fyProxy :: Proxy FY
fyProxy = Proxy

help :: (Fractional a, Ord a) => Tax (Money a) (Money a)
help = thresholds'
  [ (56156, 0.01)
  , (64838, 0.01)
  , (68727, 0.005)
  , (72852, 0.005)
  , (77223, 0.005)
  , (81856, 0.005)
  , (86767, 0.005)
  , (91974, 0.005)
  , (97492, 0.005)
  , (103342, 0.005)
  , (109543, 0.005)
  , (116116, 0.005)
  , (123082, 0.005)
  , (130467, 0.005)
  , (138295, 0.005)
  , (146594, 0.005)
  , (155389, 0.005)
  , (164712, 0.005)
  ]

medicare :: (Fractional a) => MedicareLevyRatesAndThresholds a
medicare = MedicareLevyRatesAndThresholds
  { medicareLevyRate                                  = 0.02
  , medicareLevyThresholdIndividual                   = Money 27222
  , medicareLevyThresholdIndividualSeniorAndPensioner = Money 43020
  , medicareLevyThresholdFamily                       = Money 45907
  , medicareLevyThresholdFamilySeniorAndPensioner     = Money 59886
  , medicareLevyThresholdDependentChildIncrease       = Money  4216
  }

-- | Medicare levy surcharge thresholds for 2025–26 (__NOT FINAL__)
medicareLevySurcharge :: (Fractional a, Ord a) => Tax (Money a) (Money a)
medicareLevySurcharge =
  threshold (Money 101000) 0.01
  <> threshold (Money 118000) 0.0025
  <> threshold (Money 158000) 0.0025

-- Rebate adjustment factor = TBC (currently re-using July–March rate)
-- However, the /thresholds/ did change this year.
privateHealthInsuranceRebateRates
  :: (Fractional a) => PrivateHealthInsuranceRebateRates a
privateHealthInsuranceRebateRates =
  [ (101000, (0.24288, 0.24288), (0.28337, 0.28337), (0.32385, 0.32385) )
  , (118000, (0.16192, 0.16192), (0.20240, 0.20240), (0.24288, 0.24288) )
  , (158000, (0.08095, 0.08095), (0.12143, 0.12143), (0.16192, 0.16192) )
  ]

tables :: (Ord a, Fractional a) => TaxTables FY a
tables = TaxTables
  (ttIndividualIncomeTax FY2025.tables)
  medicare
  medicareLevySurcharge
  help
  help

  lowIncomeTaxOffset2021

  privateHealthInsuranceRebateRates
