-- This file is part of hs-tax-ato
-- Copyright (C) 2024  Fraser Tweedale
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

-- | Tax tables for 2024–25 financial year.
--
-- These data are NOT YET FINAL.  We are waiting on federal budget
-- (25 March 2025) to confirm Medicare Levy thresholds, and the PHI
-- rebate adjustment factor (~1 April 2025).
module Data.Tax.ATO.FY.FY2025 (FY, fyProxy, tables) where

import Data.Proxy
import Data.Tax
import Data.Tax.ATO.Common
import Data.Tax.ATO.PrivateHealthInsuranceRebate

type FY = 2025
fyProxy :: Proxy FY
fyProxy = Proxy

help :: (Fractional a, Ord a) => Tax (Money a) (Money a)
help = thresholds'
  [ (54435, 0.01)
  , (62851, 0.01)
  , (66621, 0.005)
  , (70619, 0.005)
  , (74856, 0.005)
  , (79347, 0.005)
  , (84108, 0.005)
  , (89155, 0.005)
  , (94504, 0.005)
  , (100175, 0.005)
  , (106186, 0.005)
  , (122557, 0.005)
  , (119310, 0.005)
  , (126468, 0.005)
  , (134057, 0.005)
  , (142101, 0.005)
  , (150627, 0.005)
  , (159664, 0.005)
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

-- | Medicare levy surcharge thresholds changed for 2024–25
medicareLevySurcharge :: (Fractional a, Ord a) => Tax (Money a) (Money a)
medicareLevySurcharge =
  threshold (Money 97000) 0.01
  <> threshold (Money 113000) 0.0025
  <> threshold (Money 151000) 0.0025

-- | In 2024–25 the "stage three tax cuts" came into effect.
individualIncomeTax :: (Fractional a, Ord a) => Tax (Money a) (Money a)
individualIncomeTax = marginal'
  [ (18200, 0.16)
  , (45000, 0.30 - 0.16)
  , (135000, 0.37 - 0.30)
  , (190000, 0.45 - 0.37) ]

-- Rebate adjustment factor = TBC (currently re-using FY2024 rates)
-- However, the /thresholds/ did change this year.
privateHealthInsuranceRebateRates
  :: (Fractional a) => PrivateHealthInsuranceRebateRates a
privateHealthInsuranceRebateRates =
  [ ( 97000, (0.24608, 0.24288), (0.28710, 0.28337), (0.32812, 0.32385) )
  , (113000, (0.16405, 0.16192), (0.20507, 0.20240), (0.24608, 0.24288) )
  , (151000, (0.08202, 0.08095), (0.12303, 0.12143), (0.16405, 0.16192) )
  ]

tables :: (Ord a, Fractional a) => TaxTables FY a
tables = TaxTables
  individualIncomeTax
  medicare
  medicareLevySurcharge
  help
  help

  lowIncomeTaxOffset2021

  privateHealthInsuranceRebateRates
