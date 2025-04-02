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
    PaymentSummaryIndividualNonBusiness
  , newPaymentSummaryIndividualNonBusiness
  , paymentSummaryIndividualNonBusinessGrossPayments
  , paymentSummaryIndividualNonBusinessTotalTaxWithheld

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
pattern PaymentSummary abn gross tax resc <- PaymentSummaryIndividualNonBusiness abn gross tax resc where
  PaymentSummary abn gross tax resc = newPaymentSummaryIndividualNonBusiness abn
    & set paymentSummaryIndividualNonBusinessGrossPayments gross
    & set paymentSummaryIndividualNonBusinessTotalTaxWithheld tax
    & set reportableEmployerSuperannuationContributions resc
{-# DEPRECATED PaymentSummary "see instead PaymentSummaryIndividualNonBusiness" #-}


-- | PAYG payment summary - individual non-business
data PaymentSummaryIndividualNonBusiness a = PaymentSummaryIndividualNonBusiness
  { _inbABN :: ABN
  , _inbGross :: Money a
  , _inbWithholding :: Money a
  , _inbRESC :: Money a
  }

instance HasTaxableIncome PaymentSummaryIndividualNonBusiness a a where
  taxableIncome = paymentSummaryIndividualNonBusinessGrossPayments

instance HasTaxWithheld PaymentSummaryIndividualNonBusiness a a where
  taxWithheld = paymentSummaryIndividualNonBusinessTotalTaxWithheld

newPaymentSummaryIndividualNonBusiness
  :: (Num a) => ABN -> PaymentSummaryIndividualNonBusiness a
newPaymentSummaryIndividualNonBusiness abn =
  PaymentSummaryIndividualNonBusiness abn mempty mempty mempty

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


-- | Objects which may have Reportable employer superannuation contributions
class HasReportableEmployerSuperannuationContributions s where
  reportableEmployerSuperannuationContributions :: Lens' (s a) (Money a)
