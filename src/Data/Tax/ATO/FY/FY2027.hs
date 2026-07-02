-- This file is part of hs-tax-ato
-- Copyright (C) 2026  Fraser Tweedale
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

-- | Tax tables for 2026–27 financial year.
--
-- The 16% personal income tax bracket drops to 15%.
--
-- These data are NOT YET FINAL.  We await the federal
-- budget to confirm Medicare Levy thresholds, and the PHI
-- rebate adjustment factor (~1 April 2027), and there could
-- be other changes.
--
module Data.Tax.ATO.FY.FY2027 (FY, fyProxy, tables) where

import Data.Proxy
import Data.Tax
import Data.Tax.ATO.Common
import Data.Tax.ATO.PrivateHealthInsuranceRebate

type FY = 2027
fyProxy :: Proxy FY
fyProxy = Proxy

-- | In 2026–27 the 16% rate dropped to 15%
individualIncomeTax :: (Fractional a, Ord a) => Tax (Money a) (Money a)
individualIncomeTax = marginal'
  [ (18200, 0.15)
  , (45000, 0.30 - 0.15)
  , (135000, 0.37 - 0.30)
  , (190000, 0.45 - 0.37) ]

help :: (Fractional a, Ord a) => Tax (Money a) (Money a)
help =
  lesserOf
    ( flat 0.10 )
    ( marginal'
        [ ( 69_528, 0.15       )
        , (129_717, 0.17 - 0.15)
        ]
    )

-- TODO update (budget night)
medicare :: (Fractional a) => MedicareLevyRatesAndThresholds a
medicare = MedicareLevyRatesAndThresholds
  { medicareLevyRate                                  = 0.02
  , medicareLevyThresholdIndividual                   = Money 28011
  , medicareLevyThresholdIndividualSeniorAndPensioner = Money 44268
  , medicareLevyThresholdFamily                       = Money 47238
  , medicareLevyThresholdFamilySeniorAndPensioner     = Money 61623
  , medicareLevyThresholdDependentChildIncrease       = Money  4338
  }

-- | Medicare levy surcharge thresholds for 2026–27
medicareLevySurcharge :: (Fractional a, Ord a) => Tax (Money a) (Money a)
medicareLevySurcharge =
  threshold (Money 105000) 0.01
  <> threshold (Money 123000) 0.0025
  <> threshold (Money 164000) 0.0025

-- TODO update (~April 1 PHI circular)
privateHealthInsuranceRebateRates
  :: (Fractional a) => PrivateHealthInsuranceRebateRates a
privateHealthInsuranceRebateRates =
  [ (105000, (0.24118, 0.24118), (0.28139, 0.28139), (0.32158, 0.32158) )
  , (123000, (0.16079, 0.16079), (0.20098, 0.20098), (0.24118, 0.24118) )
  , (164000, (0.08038, 0.08038), (0.12058, 0.12058), (0.16079, 0.16079) )
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
