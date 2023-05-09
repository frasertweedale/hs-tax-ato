-- This file is part of hs-tax-ato
-- Copyright (C) 2023  Fraser Tweedale
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

-- | Tax tables for 2022–23 financial year.
module Data.Tax.ATO.FY.FY2023 (tables) where

import Data.Tax
import Data.Tax.ATO.Common
import qualified Data.Tax.ATO.FY.FY2022 as FY2022

help :: (Fractional a, Ord a) => Tax (Money a) (Money a)
help = thresholds'
  [ (48361, 0.01)
  , (55837, 0.01)
  , (59187, 0.005)
  , (62739, 0.005)
  , (66503, 0.005)
  , (70493, 0.005)
  , (74723, 0.005)
  , (79207, 0.005)
  , (83959, 0.005)
  , (88997, 0.005)
  , (94337, 0.005)
  , (99997, 0.005)
  , (105997, 0.005)
  , (112356, 0.005)
  , (119098, 0.005)
  , (126244, 0.005)
  , (133819, 0.005)
  , (141848, 0.005)
  ]

tables :: (Ord a, Fractional a) => TaxTables 2023 a
tables = TaxTables
  (ttIndividualIncomeTax FY2022.tables)

  (medicareLevy (Money 24276))

  medicareLevySurcharge
  help
  help

  -- TODO will LMITO revert to previous definition?
  (ttAdditional FY2022.tables)

  -- https://www.health.gov.au/news/phi-circulars/phi-1023-private-health-insurance-rebate-adjustment-factor-effective-1-april-2023
  (ttPHIRebateRates FY2022.tables)
