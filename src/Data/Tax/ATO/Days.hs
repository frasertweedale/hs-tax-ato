-- This file is part of hs-tax-ato
-- Copyright (C) 2018, 2023  Fraser Tweedale
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

{-|

Types and functions related to financial years.

-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tax.ATO.Days
  ( Days
  , days
  , daysAll
  , daysNone
  , getDays
  , getFraction
  , FinancialYear
  , financialYearRange
  , financialYearRangeFromProxy
  )
  where

import GHC.TypeLits
import Data.Proxy
import Data.Ratio ((%))

import Data.Time.Calendar (Day, Year, fromGregorian, isLeapYear)

type FinancialYear = KnownNat

daysInYear :: KnownNat n => Proxy n -> Integer
daysInYear proxy
  | isLeapYear (natVal proxy) = 366
  | otherwise                 = 365

-- | Some number of days in a year.  Use 'days' to construct.
newtype Days (n :: Nat) = Days
  { getDays :: Integer
  -- ^ Get the number of days, which is between 0 and 365/366 inclusive.
  }
  deriving (Show)

-- | Construct a 'Days' value.  If out of range, the number of days
-- is clamped to 0 or 365/366 (no runtime errors).
days :: forall a. (FinancialYear a) => Integer -> Days a
days = Days . max 0 . min (daysInYear (Proxy :: Proxy a))

-- | Every day of the year
daysAll :: forall a. (FinancialYear a) => Days a
daysAll = Days (daysInYear (Proxy :: Proxy a))

-- | Zero days of the year
daysNone :: Days a
daysNone = Days 0

-- | Get the number of days as a fractional value.
-- The denominator is determined by the year type.
--
getFraction :: forall a frac. (FinancialYear a, Fractional frac) => Days a -> frac
getFraction n = fromRational $ getDays n % daysInYear (Proxy :: Proxy a)

-- | Get the range of days (inclusive) for the financial year (July to June)
-- ending in the given year.
financialYearRange :: Year -> (Day, Day)
financialYearRange y =
  ( fromGregorian (y - 1) 7{-July-} 1
  , fromGregorian y       6{-June-} 30
  )

-- | Get the financial year range (inclusive) for the given type-level
-- financial year.  See also 'financialYearRange'.
financialYearRangeFromProxy :: forall n. (FinancialYear n) => Proxy n -> (Day, Day)
financialYearRangeFromProxy proxy = financialYearRange (natVal proxy)
