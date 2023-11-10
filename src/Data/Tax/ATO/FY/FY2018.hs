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

-- | Tax tables for 2017–18 financial year.
module Data.Tax.ATO.FY.FY2018 (FY, fyProxy, tables) where

import Data.Proxy
import Control.Lens (review)
import Data.Tax
import Data.Tax.ATO.Common
import Data.Tax.ATO.PrivateHealthInsuranceRebate
import qualified Data.Tax.ATO.FY.FY2017 as FY2017

type FY = 2018
fyProxy :: Proxy FY
fyProxy = Proxy

help, sfss :: (Fractional a, Ord a) => Tax (Money a) (Money a)
help = thresholds'
  [ (55874, 0.04)
  , (62293, 0.005), (68603, 0.005), (72208, 0.005),  (77619, 0.005)
  , (84063, 0.005), (88487, 0.005), (97378, 0.005), (103766, 0.005) ]
sfss = thresholds' [(55874, 0.02), (68603, 0.01), (97378, 0.01)]

-- | Individual tax rates unchanged from 2017.
--
-- The /temporary budget repair levy/ no longer applies.
--
tables :: (Ord a, Fractional a) => TaxTables FY a
tables = TaxTables
  FY2017.individualIncomeTax
  (medicareLevy (review money 21980))
  medicareLevySurcharge
  help
  sfss
  lowIncomeTaxOffset
  privateHealthInsuranceRebateRates

privateHealthInsuranceRebateRates
  :: (Fractional a) => PrivateHealthInsuranceRebateRates a
privateHealthInsuranceRebateRates =
  [ ( 90000, (0.25934, 0.25415), (0.30256, 0.29651), (0.34579, 0.33887) )
  , (105000, (0.17289, 0.16943), (0.21612, 0.21180), (0.25934, 0.25415) )
  , (140000, (0.08644, 0.08471), (0.12966, 0.12707), (0.17289, 0.16943) )
  ]
