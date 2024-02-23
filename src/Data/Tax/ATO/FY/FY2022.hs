-- This file is part of hs-tax-ato
-- Copyright (C) 2022  Fraser Tweedale
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

-- | Tax tables for 2021–22 financial year.
module Data.Tax.ATO.FY.FY2022 (FY, fyProxy, tables) where

import Data.Proxy
import Control.Lens (review)

import Data.Tax
import Data.Tax.ATO.Common
import Data.Tax.ATO.PrivateHealthInsuranceRebate
import qualified Data.Tax.ATO.FY.FY2021 as FY2021

type FY = 2022
fyProxy :: Proxy FY
fyProxy = Proxy

help :: (Fractional a, Ord a) => Tax (Money a) (Money a)
help = thresholds'
  [ (47014, 0.01)
  , (54283, 0.01)
  , (57539, 0.005)
  , (60992, 0.005)
  , (64652, 0.005)
  , (68530, 0.005)
  , (72642, 0.005)
  , (77002, 0.005)
  , (81621, 0.005)
  , (86519, 0.005)
  , (91710, 0.005)
  , (97213, 0.005)
  , (103046, 0.005)
  , (109228, 0.005)
  , (115782, 0.005)
  , (122729, 0.005)
  , (130093, 0.005)
  , (137898, 0.005)
  ]

tables :: (Ord a, Fractional a) => TaxTables FY a
tables = TaxTables
  FY2021.individualIncomeTax
  (medicareLevy (review money 23365))
  (ttMedicareLevySurcharge FY2021.tables)
  help
  help
  (lowIncomeTaxOffset2021 <> lmito2022)
  privateHealthInsuranceRebateRates

-- https://www.health.gov.au/news/phi-circulars/phi-0422-private-health-insurance-rebate-adjustment-factor-effective-1-april-2022
privateHealthInsuranceRebateRates
  :: (Fractional a) => PrivateHealthInsuranceRebateRates a
privateHealthInsuranceRebateRates =
  [ ( 90000, (0.24608, 0.24608), (0.28710, 0.28710), (0.32812, 0.32812) )
  , (105000, (0.16405, 0.16405), (0.20507, 0.20507), (0.24608, 0.24608) )
  , (140000, (0.08202, 0.08202), (0.12303, 0.12303), (0.16405, 0.16405) )
  ]

-- | LMITO ceiling increased by $420 to $1500 in FY2022
lmito2022 :: (Fractional a, Ord a) => Tax (Money a) (Money a)
lmito2022 = limit mempty $
  greaterOf (lump (review money (-1500)))
    ( lump (review money (-255))
    <> above (review money 37000) (-0.075) )
  <> above (review money 90000) 0.03
