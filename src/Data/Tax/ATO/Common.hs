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

Common taxes and helpers.

-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tax.ATO.Common
  (
  -- * Tax tables
    TaxTables(..)

  -- * Classes
  , HasTaxableIncome(..)
  , HasTaxWithheld(..)

  -- * Medicare levy
  , MedicareLevyRatesAndThresholds(..)
  , medicareLevy'

  -- * Common taxes and helpers
  , medicareLevy
  , lowIncomeTaxOffset
  , lowIncomeTaxOffset2021
  , lamito
  , corporateTax

  -- * Deduction rates that vary by financial year
  , applyCentsPerKilometreMethod
  , HasCentsPerKilometreMethod

  , applyFixedRateMethod
  , HasFixedRateMethod
  , applyFixedRateMethodPre2023
  , HasFixedRateMethodPre2023
  , applyShortcutMethod
  , HasShortcutMethod

  -- * Convenience functions
  , thresholds'
  , marginal'
  ) where

import Control.Lens (Getter, review, to, view)
import Data.Bifunctor (first)
import Data.Proxy
import GHC.TypeLits

import Data.Tax
import Data.Tax.ATO.PrivateHealthInsuranceRebate
import Data.Tax.ATO.FY

-- | A set of tax tables for a particular financial year
data TaxTables y a = TaxTables
  { ttIndividualIncomeTax :: Tax (Money a) (Money a)
  , ttMedicareLevyRatesAndThresholds :: MedicareLevyRatesAndThresholds a
  , ttMedicareLevySurcharge :: Tax (Money a) (Money a)
  , ttHelp :: Tax (Money a) (Money a)
  , ttSfss :: Tax (Money a) (Money a)
  , ttAdditional :: Tax (Money a) (Money a)
  -- ^ Additional taxes and offsets that apply at EOY
  , ttPHIRebateRates :: PrivateHealthInsuranceRebateRates a
  }


data MedicareLevyRatesAndThresholds a = MedicareLevyRatesAndThresholds
  { medicareLevyRate :: a
  , medicareLevyThresholdIndividual :: Money a
  , medicareLevyThresholdIndividualSeniorAndPensioner :: Money a
  , medicareLevyThresholdFamily :: Money a
  , medicareLevyThresholdFamilySeniorAndPensioner :: Money a
  , medicareLevyThresholdDependentChildIncrease :: Money a
  }

medicareLevy'
  :: (Fractional a, Ord a)
  => MedicareLevyRatesAndThresholds a
  -> Money a          -- ^ taxable income
  -> Maybe (Money a)  -- ^ spouse taxable income
  -> Integer          -- ^ number of dependents
  -> Money a
medicareLevy' rates me mYou nDeps = case mYou of
  Nothing | nDeps <= 0 ->
    let
      t = medicareLevyThresholdIndividual rates
    in
      getTax (mk t) me
  _ ->
    let
      t = medicareLevyThresholdFamily rates
          $+$ fromIntegral nDeps *$ medicareLevyThresholdDependentChildIncrease rates
      combinedIncome = maybe me (me $+$) mYou
    in
      (me $/$ combinedIncome) *$ getTax (mk t) combinedIncome
  where
    mk l = lesserOf (above l 0.1) (flat (medicareLevyRate rates))


-- | The Medicare levy, incorporating the Medicare levy reduction.
-- The rate is 10% of the income above the given shade-in threshold
-- or 2% of the total income, whichever is less.
--
medicareLevy :: (Fractional a, Ord a) => Money a -> Tax (Money a) (Money a)
medicareLevy l = lesserOf (above l 0.1) (flat 0.02)

-- | /Low income tax offset (LITO)/.  $445, reduced by 1.5c for
-- every dollar earned over $37,000. The lump amount may change in
-- the future.
lowIncomeTaxOffset :: (Fractional a, Ord a) => Tax (Money a) (Money a)
lowIncomeTaxOffset =
  limit mempty
  (lump (review money (-445)) <> above (review money 37000) 0.015)

-- | /Low income tax offset/, 2020–21 version.
lowIncomeTaxOffset2021 :: (Fractional a, Ord a) => Tax (Money a) (Money a)
lowIncomeTaxOffset2021 =
  limit mempty $
    lump (review money (-700))
    <> above (review money 37500) 0.05
    <> above (review money 45000) (-0.035)

-- | Low and middle income tax offset. FY2019, 2020, 2021.
--
lamito :: (Fractional a, Ord a) => Tax (Money a) (Money a)
lamito = limit mempty $
  greaterOf (lump (review money (-1080)))
    ( lump (review money (-255))
    <> above (review money 37000) (-0.075) )
  <> above (review money 90000) 0.03

-- | The corporate tax rate of 30%.  In the future, different rates may
-- be levied depending on business turnover/income.
corporateTax :: (Fractional a) => Tax (Money a) (Money a)
corporateTax = flat 0.3

thresholds', marginal'
  :: (Fractional a, Ord a) => [(a, a)] -> Tax (Money a) (Money a)
thresholds' = thresholds . fmap (first (review money))
-- ^ Convenience wrapper for 'thresholds'.  Turns the thresholds into 'Money'
marginal' = marginal . fmap (first (review money))
-- ^ Convenience wrapper for 'marginal'.  Turns the margins into 'Money'


-- | Types that may have a taxable income component.
class HasTaxableIncome a b c where
  taxableIncome :: Getter (a b) (Money c)

instance
    (Foldable t, HasTaxableIncome x a a, Num a)
    => HasTaxableIncome t (x a) a
    where
  taxableIncome = to (foldMap (view taxableIncome))

-- | Types that can have an amount of tax withheld.
class HasTaxWithheld a b c where
  taxWithheld :: Getter (a b) (Money c)

instance (Foldable t, HasTaxWithheld x a a, Num a)
            => HasTaxWithheld t (x a) a where
  taxWithheld = to (foldMap (view taxWithheld))


-- | See 'applyCentsPerKilometreMethod'.
class HasCentsPerKilometreMethod y where
  centsPerKilometre :: Num a => a

instance (2016 <= y, y <= 2025, FinancialYear y) => HasCentsPerKilometreMethod y where
  centsPerKilometre = case fromProxy (Proxy @y) of
    y | y == 2025           -> 88
      | y == 2024           -> 85
      | y == 2023           -> 78
      | y > 2020, y <= 2022 -> 72
      | y > 2018, y <= 2020 -> 68
      | y > 2015, y <= 2018 -> 66
      | otherwise           ->  0  -- can't happen

-- | Apply the single-rate cents per kilometre method (in use since
-- the 2015–16 FY).  Clamped (by law) at 5000 kms.  Use
-- @TypeApplications@ to specify the financial year, e.g.
--
-- @
-- import Data.Tax.ATO.FY.2025 (FY)
--
-- y = applyCentsPerKilometreMethod \@2025 1111  -- using type literal
-- x = applyCentsPerKilometreMethod \@FY   1111  -- same thing, using type synonym
-- @
--
applyCentsPerKilometreMethod
  :: forall y a. (HasCentsPerKilometreMethod y, Fractional a, Ord a)
  => a -> Money a
applyCentsPerKilometreMethod kms = Money $ centsPerKilometre @y * min kms 5000 / 100


-- | See 'applyFixedRateMethod'
class HasFixedRateMethod y where
  fixedRateMethodCentsPerHour :: Num a => a

instance HasFixedRateMethod 2023 where
  fixedRateMethodCentsPerHour = 67
instance HasFixedRateMethod 2024 where
  fixedRateMethodCentsPerHour = 67
instance HasFixedRateMethod 2025 where
  fixedRateMethodCentsPerHour = 70

-- | The updated fixed rate method available from the 2022–23 income
-- year.  It covers:
--
-- * home and mobile internet or data expenses
-- * mobile and home phone usage expenses
-- * electricity and gas (energy expenses) for heating, cooling and lighting
-- * stationery and computer consumables, such as printer ink and paper.
--
-- You can separately claim deductions for work-related use of
-- technology and office furniture, and (in limited circumstances)
-- occupancy expenses and cleaning expenses.
--
-- Use @TypeApplications@ to specify the financial year, e.g.
--
-- @
-- import Data.Tax.ATO.FY.2025 (FY)
--
-- y = applyFixedRateMethod \@2025 1111  -- using type literal
-- x = applyFixedRateMethod \@FY   1111  -- same thing, using type synonym
-- @
--
-- For earlier income years use 'applyFixedRateMethodPre2023'.
--
applyFixedRateMethod
  :: forall y a. (HasFixedRateMethod y, Fractional a)
  => a -> Money a
applyFixedRateMethod hours = Money $ fixedRateMethodCentsPerHour @y * hours / 100


-- | See 'applyShortcutMethod'
class HasShortcutMethod y where
  shortcutMethodCentsPerHour :: Num a => a

instance (2020 <= y, y <= 2022) => HasShortcutMethod y where
  shortcutMethodCentsPerHour = 80

-- | The shortcut method for working from home deductions was
-- available from 1 March 2020 to 30 June 2020 in the 2019–20 income
-- year, and for the 2020–21 and 2021–22 income years.  It covers:
--
-- * phone and data expenses
-- * internet expenses
-- * the decline in value of equipment and furniture
-- * electricity and gas (energy expenses) for heating, cooling and lighting.
--
-- Use @TypeApplications@ to specify the financial year, e.g.
--
-- @
-- import Data.Tax.ATO.FY.2025 (FY)
--
-- y = applyShortcutMethod \@2025 1111  -- using type literal
-- x = applyShortcutMethod \@FY   1111  -- same thing, using type synonym
-- @
--
applyShortcutMethod
  :: forall y a. (HasShortcutMethod y, Fractional a)
  => a -> Money a
applyShortcutMethod hours = Money $ shortcutMethodCentsPerHour @y * hours / 100


-- | See 'applyFixedRateMethodPre2023'
class HasFixedRateMethodPre2023 y where
  fixedRateMethodPre2023CentsPerHour :: Num a => a

instance (2002 <= y, y <= 2022, FinancialYear y) => HasFixedRateMethodPre2023 y where
  fixedRateMethodPre2023CentsPerHour = case fromProxy (Proxy @y) of
    y | y > 2018, y <= 2022 -> 52
      | y > 2014, y <= 2018 -> 45
      | y > 2010, y <= 2014 -> 34
      | y > 2004, y <= 2010 -> 26
      | y > 2001, y <= 2004 -> 20
      | otherwise           ->  0  -- can't happen

-- | The fixed rate method available up to the 2021–22 income year.
-- It differs from the current fixed rate method (see
-- 'HasFixedRateMethod') in which categories of expenses are
-- included.  This method covers:
--
-- * the decline in value of home office furniture and
--   furnishings – for example, a desk
-- * electricity and gas (energy expenses) for heating,
--   cooling and lighting
-- * cleaning your dedicated home office.
--
-- You should separately claim for:
--
-- * home phone and internet expenses, including the decline in
--   value of the handset
-- * stationery and computer consumables, such as printer ink
--   and paper
-- * decline in value of depreciating assets other than
--   home office furniture and furnishings used for work
--   purposes – for example, computers and laptops.
--
-- Use @TypeApplications@ to specify the financial year, e.g.
--
-- @
-- import Data.Tax.ATO.FY.2025 (FY)
--
-- y = applyFixedRateMethodPre2023 \@2025 1111  -- using type literal
-- x = applyFixedRateMethodPre2023 \@FY   1111  -- same thing, using type synonym
-- @
--
-- For later income years, use 'applyFixedRateMethod'.
--
applyFixedRateMethodPre2023
  :: forall y a. (HasFixedRateMethodPre2023 y, Fractional a)
  => a -> Money a
applyFixedRateMethodPre2023 hours = Money $ fixedRateMethodPre2023CentsPerHour @y * hours / 100
