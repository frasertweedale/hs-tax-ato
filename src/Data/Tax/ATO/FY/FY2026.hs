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
-- Apart from the usual Medicare levy and private health insurance
-- rebate adjustments, study loan repayments changed to a marginal
-- tax (previously a threshold tax).
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
help =
  lesserOf
    ( flat 0.10 )
    ( marginal'
        [ ( 67_000, 0.15       )
        , (125_000, 0.17 - 0.15)
        ]
    )

medicare :: (Fractional a) => MedicareLevyRatesAndThresholds a
medicare = MedicareLevyRatesAndThresholds
  { medicareLevyRate                                  = 0.02
  , medicareLevyThresholdIndividual                   = Money 28011
  , medicareLevyThresholdIndividualSeniorAndPensioner = Money 44268
  , medicareLevyThresholdFamily                       = Money 47238
  , medicareLevyThresholdFamilySeniorAndPensioner     = Money 61623
  , medicareLevyThresholdDependentChildIncrease       = Money  4338
  }

-- | Medicare levy surcharge thresholds for 2025–26
medicareLevySurcharge :: (Fractional a, Ord a) => Tax (Money a) (Money a)
medicareLevySurcharge =
  threshold (Money 101000) 0.01
  <> threshold (Money 118000) 0.0025
  <> threshold (Money 158000) 0.0025

-- Rebate adjustment factor = 0.993.
-- https://www.health.gov.au/news/phi-circulars/phi-1226-private-health-insurance-rebate-adjustment-factor-effective-1-april-2026
privateHealthInsuranceRebateRates
  :: (Fractional a) => PrivateHealthInsuranceRebateRates a
privateHealthInsuranceRebateRates =
  [ (101000, (0.24288, 0.24118), (0.28337, 0.28139), (0.32385, 0.32158) )
  , (118000, (0.16192, 0.16079), (0.20240, 0.20098), (0.24288, 0.24118) )
  , (158000, (0.08095, 0.08038), (0.12143, 0.12058), (0.16192, 0.16079) )
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
