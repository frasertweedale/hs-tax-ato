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

{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE PatternSynonyms #-}

{-|

Types and optics for payment summaries.

-}
module Data.Tax.ATO.PaymentSummary
  (
  -- ** Individual non-business
    PaymentSummaryIndividualNonBusiness
  , newPaymentSummaryIndividualNonBusiness
  , paymentSummaryIndividualNonBusinessGrossPayments
  , paymentSummaryIndividualNonBusinessTotalTaxWithheld

  -- *** Fringe benefits
  , HasReportableFringeBenefits(..)
  , fringeBenefitsEmployerNotExempt
  , fringeBenefitsEmployerExempt
  , ReportableFringeBenefits(..)
  , FBTEmployerExemption(..)

  -- ** Classes and helpers
  , HasReportableEmployerSuperannuationContributions(..)
  , pattern PaymentSummary
  )
  where

import Control.Lens

import Data.Tax
import Data.Tax.ATO.ABN (ABN)
import Data.Tax.ATO.Common


-- | Deprecated pattern provided for compatibility.  Will be removed
-- in a future release.  Use 'PaymentSummaryIndividualNonBusiness'
-- constructor and accessors instead.
--
pattern PaymentSummary
  :: (Num a) => ABN -> Money a -> Money a -> Money a -> PaymentSummaryIndividualNonBusiness a
pattern PaymentSummary abn gross tax resc
    <- PaymentSummaryIndividualNonBusiness abn gross tax resc _fb
    where
  PaymentSummary abn gross tax resc = newPaymentSummaryIndividualNonBusiness abn
    & set paymentSummaryIndividualNonBusinessGrossPayments gross
    & set paymentSummaryIndividualNonBusinessTotalTaxWithheld tax
    & set reportableEmployerSuperannuationContributions resc
{-# DEPRECATED PaymentSummary "see instead PaymentSummaryIndividualNonBusiness" #-}


-- | PAYG payment summary - individual non-business.
--
-- Use 'newPaymentSummaryIndividualNonBusiness' to construct this data
-- type (all amounts initially zero).
--
-- To @set@ and @view@ the various fields, these lenses are available:
--
-- +------------------------------------------------------+------------------------------------+
-- | 'paymentSummaryIndividualNonBusinessTotalTaxWithheld'| TOTAL TAX WITHHELD                 |
-- +------------------------------------------------------+------------------------------------+
-- | 'paymentSummaryIndividualNonBusinessGrossPayments'   | Gross payments                     |
-- +------------------------------------------------------+------------------------------------+
-- | 'reportableEmployerSuperannuationContributions'      | Reportable employer superannuation |
-- |                                                      | contributions (do not include      |
-- |                                                      | compulsory super guarantee amounts)|
-- +------------------------------------------------------+------------------------------------+
-- | 'reportableFringeBenefits'                           | Fringe benefits amount for FBT year|
-- |                                                      | 1 April to 31 March, and employer  |
-- |                                                      | FBT exemption status.  See also    |
-- |                                                      | 'ReportableFringeBenefits'.        |
-- +------------------------------------------------------+------------------------------------+
--
data PaymentSummaryIndividualNonBusiness a = PaymentSummaryIndividualNonBusiness
  { _inbABN :: ABN
  , _inbGross :: Money a
  , _inbWithholding :: Money a
  , _inbRESC :: Money a
  , _inbFringeBenefits :: Maybe (ReportableFringeBenefits a)
  }

instance HasTaxableIncome PaymentSummaryIndividualNonBusiness a a where
  taxableIncome = paymentSummaryIndividualNonBusinessGrossPayments

instance HasTaxWithheld PaymentSummaryIndividualNonBusiness a a where
  taxWithheld = paymentSummaryIndividualNonBusinessTotalTaxWithheld

-- | Construct a new payment summary.  All amounts are initially zero.
newPaymentSummaryIndividualNonBusiness
  :: (Num a) => ABN -> PaymentSummaryIndividualNonBusiness a
newPaymentSummaryIndividualNonBusiness abn =
  PaymentSummaryIndividualNonBusiness abn mempty mempty mempty Nothing

paymentSummaryIndividualNonBusinessGrossPayments
  :: Lens' (PaymentSummaryIndividualNonBusiness a) (Money a)
paymentSummaryIndividualNonBusinessGrossPayments =
  lens _inbGross (\s b -> s { _inbGross = b })

paymentSummaryIndividualNonBusinessTotalTaxWithheld
  :: Lens' (PaymentSummaryIndividualNonBusiness a) (Money a)
paymentSummaryIndividualNonBusinessTotalTaxWithheld =
  lens _inbWithholding (\s b -> s { _inbWithholding = b })

instance HasReportableEmployerSuperannuationContributions PaymentSummaryIndividualNonBusiness where
  reportableEmployerSuperannuationContributions =
    lens _inbRESC (\s b -> s { _inbRESC = b })

-- | Objects which may have reportable fringe benefit amounts
class HasReportableFringeBenefits s where
  reportableFringeBenefits :: Lens' (s a) (Maybe (ReportableFringeBenefits a))

instance HasReportableFringeBenefits PaymentSummaryIndividualNonBusiness where
  reportableFringeBenefits =
    lens _inbFringeBenefits (\s b -> s { _inbFringeBenefits = b })


-- | Helper constructor for fringe benefits amount (employer not exempt from FBT).
-- See also 'fringeBenefitsEmployerExempt'.
fringeBenefitsEmployerNotExempt :: Money a -> Maybe (ReportableFringeBenefits a)
fringeBenefitsEmployerNotExempt x = Just $ ReportableFringeBenefits x EmployerNotFBTExempt

-- | Helper constructor for fringe benefits amount (employer exempt from FBT)
-- See also 'fringeBenefitsEmployerNotExempt'.
fringeBenefitsEmployerExempt :: Money a -> Maybe (ReportableFringeBenefits a)
fringeBenefitsEmployerExempt x = Just $ ReportableFringeBenefits x EmployerFBTExempt


-- | Objects which may have Reportable employer superannuation contributions
class HasReportableEmployerSuperannuationContributions s where
  reportableEmployerSuperannuationContributions :: Lens' (s a) (Money a)


data FBTEmployerExemption = EmployerNotFBTExempt | EmployerFBTExempt
  deriving (Eq, Ord)

-- | Reportable fringe benefits amount, with the employer exemption status.
--
-- For setting the fringe benefits amount in a
-- 'PaymentSummaryIndividualNonBusiness', we provide more ergonomic helper
-- constructors: 'fringeBenefitsEmployerNotExempt' and
-- 'fringeBenefitsEmployerExempt'.
--
data ReportableFringeBenefits a =
  ReportableFringeBenefits (Money a) FBTEmployerExemption
  deriving (Eq, Ord)
