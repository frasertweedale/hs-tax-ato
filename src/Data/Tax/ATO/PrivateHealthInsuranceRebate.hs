-- This file is part of hs-tax-ato
-- Copyright (C) 2021  Fraser Tweedale
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

{-# LANGUAGE RankNTypes #-}

{- |

Types and functions for the Private Health Insurance Rebate.

-}
module Data.Tax.ATO.PrivateHealthInsuranceRebate
  (
    PrivateHealthInsuranceRebateRates
  , PrivateHealthInsurancePolicyDetail(..)
  , BenefitCode(..)
  , assessExcessPrivateHealthRebate
  ) where

import Data.List (find)

import Control.Lens
import Data.Tax
import Data.Tax.ATO.Rounding

type HealthInsurerID = String
type MembershipNumber = String

data BenefitCode
  = BenefitCode30 -- ^ Under 65, 1 July to 31 March
  | BenefitCode31 -- ^ Over 65, 1 April to 30 June
  | BenefitCode35 -- ^ 65 to 69, 1 July to 31 March
  | BenefitCode36 -- ^ 65 to 69, 1 April to 30 June
  | BenefitCode40 -- ^ 70 or over, 1 July to 31 March
  | BenefitCode41 -- ^ 70 or over, 1 April to 30 June

data PrivateHealthInsurancePolicyDetail a =
  PrivateHealthInsurancePolicyDetail
    HealthInsurerID
    MembershipNumber
    (Money a) -- ^ premiums eligible for rebate
    (Money a) -- ^ rebate received
    BenefitCode

-- | A line of rebate rates.
--
-- The first field is the upper income threshold for single persons
-- (inclusive) for the given rate.  Thresholds must be given in
-- increasing order.
--
-- The second, third and fourth fields are the rebate rates for when
-- the oldest person on the policy is aged, respectively: under 65;
-- 65 to 69; 70 or older.  Each of these values is a pair.
--
-- The first subfield is the rebate for 1 July to 31 March.
--
-- The second subfield is the rebate for 1 April to 30 June.
--
-- An income that exceeds the highest threshold implicitly gets a
-- rebate of 0%.
--
type PrivateHealthInsuranceRebateRatesLine a
  = (a, (a, a), (a, a), (a, a))
type PrivateHealthInsuranceRebateRates a
  = [PrivateHealthInsuranceRebateRatesLine a]

byBenefitCode
  :: BenefitCode
  -> Lens' (PrivateHealthInsuranceRebateRatesLine a) a
byBenefitCode code = case code of
  BenefitCode30 -> _2 . _1
  BenefitCode31 -> _2 . _2
  BenefitCode35 -> _3 . _1
  BenefitCode36 -> _3 . _2
  BenefitCode40 -> _4 . _1
  BenefitCode41 -> _4 . _2

getRebateRate
  :: (Ord a, Num a)
  => Money a
  -> BenefitCode
  -> PrivateHealthInsuranceRebateRates a
  -> Tax (Money a) (Money a)
getRebateRate income code rates =
  case find (\line -> income <= Money (view _1 line)) rates of
    Nothing -> mempty
    Just rec -> flat $ view (byBenefitCode code) rec

-- | Compute rebates received minus rebate entitlements.
-- Therefore a positive result is tax DUE, and a
-- negative result is a tax CREDIT.
--
assessExcessPrivateHealthRebate
  :: (RealFrac a)
  => Money a          -- ^ income for MLS purposes
  -> Maybe (Money a)  -- ^ spouse income for MLS purposes
  -> Integer          -- ^ number of dependents
  -> PrivateHealthInsuranceRebateRates a
  -> [PrivateHealthInsurancePolicyDetail a]
  -> Money a
assessExcessPrivateHealthRebate income spouseIncome dependents rates =
  foldMap f
  where

  -- The family income threshold is double the single threshold,
  -- increased by $1,500 for each Medicare levy surcharge dependent
  -- child after the first child.
  factor = maybe (if dependents > 0 then 2 else 1) (const 2) spouseIncome
  increase = 1500 * max 0 (fromIntegral dependents - 1)
  preppedRates = over (traverse . _1) ((+ increase) . (* factor)) rates

  preppedIncome = maybe income (<> income) spouseIncome

  f (PrivateHealthInsurancePolicyDetail _ _ eligible received code) =
    let rate = getRebateRate preppedIncome code preppedRates
    in received $-$ getTax (fmap roundCents rate) eligible
