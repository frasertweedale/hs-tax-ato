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

{-|

Types for representing a number of days in a year.

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tax.ATO.Days
  ( Days
  , days
  , daysAll
  , daysNone
  , getDays
  , getFraction
  , YearType(..)
  , DaysInYear(..)
  )
  where

import Data.Proxy
import Data.Ratio ((%))

-- | Type of year (common or leap)
data YearType = CommonYear | LeapYear

-- | Get the number of days for the 'YearType'
class DaysInYear (a :: YearType) where
  daysInYear :: Proxy a -> Integer

instance DaysInYear 'CommonYear where
  daysInYear _ = 365

instance DaysInYear 'LeapYear where
  daysInYear _ = 366

-- | Some number of days in a year.  Use 'days' to construct.
newtype Days (n :: YearType) = Days
  { getDays :: Integer
  -- ^ Get the number of days, which is between 0 and 365/366 inclusive.
  }
  deriving (Show)

-- | Construct a 'Days' value.  If out of range, the number of days
-- is clamped to 0 or 365/366 (no runtime errors).
days :: forall a. (DaysInYear a) => Integer -> Days a
days = Days . max 0 . min (daysInYear (Proxy :: Proxy a))

-- | Every day of the year
daysAll :: forall a. (DaysInYear a) => Days a
daysAll = Days (daysInYear (Proxy :: Proxy a))

-- | Zero days of the year
daysNone :: Days a
daysNone = Days 0

-- | Get the number of days as a fractional value.
-- Information about the the year type is discarded.
getFraction :: forall a frac. (DaysInYear a, Fractional frac) => Days a -> frac
getFraction n = fromRational $ getDays n % daysInYear (Proxy :: Proxy a)
