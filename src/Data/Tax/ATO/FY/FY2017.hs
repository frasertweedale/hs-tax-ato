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

{-# LANGUAGE DataKinds #-}

-- | Tax tables for 2016–17 financial year.
module Data.Tax.ATO.FY.FY2017 (FY, fyProxy, tables, individualIncomeTax) where

import Data.Proxy

import Data.Tax
import Data.Tax.ATO.Common
import Data.Tax.ATO.PrivateHealthInsuranceRebate

type FY = 2017
fyProxy :: Proxy FY
fyProxy = Proxy

-- | In FY2017 the 37% threshold was raised from $80,000 to $87,000
--
individualIncomeTax :: (Fractional a, Ord a) => Tax (Money a) (Money a)
individualIncomeTax = marginal'
  [ (18200, 0.19)
  , (37000, 0.325 - 0.19)
  , (87000, 0.37 - 0.325)
  , (180000, 0.45 - 0.37) ]

temporaryBudgetRepairLevy :: (Fractional a, Ord a) => Tax (Money a) (Money a)
temporaryBudgetRepairLevy = above (Money 180000) 0.02

help, sfss :: (Fractional a, Ord a) => Tax (Money a) (Money a)
help = thresholds'
  [ (54869, 0.04)
  , (61120, 0.005), (67369, 0.005), (70910, 0.005), (76223, 0.005)
  , (82551, 0.005), (86895, 0.005), (95627, 0.005), (101900, 0.005) ]
sfss = thresholds' [(54869, 0.02), (67369, 0.01), (95627, 0.01)]

medicare :: (Fractional a) => MedicareLevyRatesAndThresholds a
medicare = MedicareLevyRatesAndThresholds
  { medicareLevyRate                                  = 0.02
  , medicareLevyThresholdIndividual                   = Money 21655
  , medicareLevyThresholdIndividualSeniorAndPensioner = Money 34244
  , medicareLevyThresholdFamily                       = Money 36541
  , medicareLevyThresholdFamilySeniorAndPensioner     = Money 47670
  , medicareLevyThresholdDependentChildIncrease       = Money  3356
  }

-- | /Medicare levy surcharge (MLS)/.  Certain exemptions are available.
--
-- __Known issues__: the MLS is levied on taxable income + fringe
-- benefits, but this is not implemented properly yet.  The
-- thresholds are affected by family income and number of
-- dependents; this also is not implemented.
--
medicareLevySurcharge :: (Fractional a, Ord a) => Tax (Money a) (Money a)
medicareLevySurcharge =
  threshold (Money 90000) 0.01
  <> threshold (Money 105000) 0.0025
  <> threshold (Money 140000) 0.0025

-- | In FY2017 the 37% threshold was raised from $80,000 to $87,000
--
-- The Temporary Budget Repair Levy, 2% of income above $180,000,
-- occurred in 2014-15, 2015-16, 2016-17.  This was the final year.
--
tables :: (Ord a, Fractional a) => TaxTables 2017 a
tables = TaxTables
  (individualIncomeTax <> temporaryBudgetRepairLevy)
  medicare
  medicareLevySurcharge
  help
  sfss
  lowIncomeTaxOffset
  privateHealthInsuranceRebateRates

privateHealthInsuranceRebateRates
  :: (Fractional a) => PrivateHealthInsuranceRebateRates a
privateHealthInsuranceRebateRates =
  [ ( 90000, (0.26791, 0.25934), (0.31256, 0.30256), (0.35722, 0.34579) )
  , (105000, (0.17861, 0.17289), (0.22326, 0.21612), (0.26791, 0.25934) )
  , (140000, (0.08930, 0.08644), (0.13395, 0.12966), (0.17861, 0.17289) )
  ]
