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

-- | Tax tables for 2023–24 financial year.
module Data.Tax.ATO.FY.FY2024 (FY, fyProxy, tables) where

import Data.Proxy
import Data.Tax
import Data.Tax.ATO.Common
import Data.Tax.ATO.PrivateHealthInsuranceRebate
import qualified Data.Tax.ATO.FY.FY2023 as FY2023

type FY = 2024
fyProxy :: Proxy FY
fyProxy = Proxy

help :: (Fractional a, Ord a) => Tax (Money a) (Money a)
help = thresholds'
  [ (51550, 0.01)
  , (59519, 0.01)
  , (63090, 0.005)
  , (66876, 0.005)
  , (70889, 0.005)
  , (75141, 0.005)
  , (79650, 0.005)
  , (84430, 0.005)
  , (89495, 0.005)
  , (94866, 0.005)
  , (100558, 0.005)
  , (106591, 0.005)
  , (112986, 0.005)
  , (119765, 0.005)
  , (126951, 0.005)
  , (134569, 0.005)
  , (142643, 0.005)
  , (151201, 0.005)
  ]

medicare :: (Fractional a) => MedicareLevyRatesAndThresholds a
medicare = MedicareLevyRatesAndThresholds
  { medicareLevyRate                                  = 0.02
  , medicareLevyThresholdIndividual                   = Money 26000
  , medicareLevyThresholdIndividualSeniorAndPensioner = Money 41089
  , medicareLevyThresholdFamily                       = Money 43846
  , medicareLevyThresholdFamilySeniorAndPensioner     = Money 57198
  , medicareLevyThresholdDependentChildIncrease       = Money  4027
  }

-- | Medicare levy surcharge thresholds changed for 2023–24
medicareLevySurcharge :: (Fractional a, Ord a) => Tax (Money a) (Money a)
medicareLevySurcharge =
  threshold (Money 93000) 0.01
  <> threshold (Money 108000) 0.0025
  <> threshold (Money 144000) 0.0025

-- Rebate adjustment factor = 1.000 (no change)
-- https://www.health.gov.au/news/phi-circulars/phi-1724-private-health-insurance-rebate-adjustment-factor-effective-1-april-2024
-- However, the /thresholds/ did change this year.
privateHealthInsuranceRebateRates
  :: (Fractional a) => PrivateHealthInsuranceRebateRates a
privateHealthInsuranceRebateRates =
  [ ( 93000, (0.24608, 0.24608), (0.28710, 0.28710), (0.32812, 0.32812) )
  , (108000, (0.16405, 0.16405), (0.20507, 0.20507), (0.24608, 0.24608) )
  , (144000, (0.08202, 0.08202), (0.12303, 0.12303), (0.16405, 0.16405) )
  ]

tables :: (Ord a, Fractional a) => TaxTables FY a
tables = TaxTables
  (ttIndividualIncomeTax FY2023.tables)
  medicare
  medicareLevySurcharge
  help
  help

  lowIncomeTaxOffset2021

  privateHealthInsuranceRebateRates
