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
