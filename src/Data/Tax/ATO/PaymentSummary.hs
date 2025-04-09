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
  , GrossPaymentsTypeIndividualNonBusiness(..)
  , grossPaymentsTypeP
  , grossPaymentsTypeH

  -- *** Allowances
  , allowances
  , Allowance
  , allowance
  , allowanceDetail
  , allowanceAmount

  -- *** Fringe benefits
  , HasReportableFringeBenefits(..)
  , fringeBenefitsEmployerNotExempt
  , fringeBenefitsEmployerExempt
  , ReportableFringeBenefits(..)
  , FBTEmployerExemption(..)

  -- *** Lump sum A
  , HasLumpSumA(..)
  , lumpSumATypeR
  , lumpSumATypeT
  , LumpSumA
  , lumpSumAType
  , lumpSumAAmount
  , LumpSumAType(..)

  -- *** Lump sum B
  , lumpSumB

  -- *** Lump sum D
  , HasLumpSumD(..)

  -- ** Classes and helpers
  , HasGrossPayments(..)
  , HasGrossPaymentsType(..)
  , HasTotalTaxWithheld(..)
  , HasReportableEmployerSuperannuationContributions(..)
  , pattern PaymentSummary
  )
  where

import Control.Lens

import Data.Tax
import Data.Tax.ATO.ABN (ABN)
import Data.Tax.ATO.Common
import Data.Tax.ATO.Rounding


-- | Deprecated pattern provided for compatibility.  Will be removed
-- in a future release.  Use 'PaymentSummaryIndividualNonBusiness'
-- constructor and accessors instead.
--
pattern PaymentSummary
  :: (Num a) => ABN -> Money a -> Money a -> Money a -> PaymentSummaryIndividualNonBusiness a
pattern PaymentSummary abn gross tax resc <-
  PaymentSummaryIndividualNonBusiness abn tax gross _type resc _fb
    _allow _lumpA _lumpB _lumpD
  where
  PaymentSummary abn gross tax resc = newPaymentSummaryIndividualNonBusiness abn
    & set grossPayments gross
    & set totalTaxWithheld tax
    & set reportableEmployerSuperannuationContributions resc
{-# DEPRECATED PaymentSummary "see instead PaymentSummaryIndividualNonBusiness" #-}


-- | PAYG payment summary - individual non-business.
--
-- Use 'newPaymentSummaryIndividualNonBusiness' to construct this data
-- type (all amounts initially zero).
--
-- To @set@ and @view@ the various fields, these lenses are available:
--
-- +-------------------------------------------------+-----------------------------------------+
-- | 'totalTaxWithheld'                              | TOTAL TAX WITHHELD                      |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'grossPayments'                                 | GROSS PAYMENTS.  Do not include amounts |
-- |                                                 | shown under 'allowances'.               |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'grossPaymentsType'                             | Use 'grossPaymentsTypeP' for non super  |
-- |                                                 | pensions and annuities, or              |
-- |                                                 | 'grossPaymentsTypeH' for working        |
-- |                                                 | holiday makers. Otherwise, leave unset. |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'reportableEmployerSuperannuationContributions' | Reportable employer superannuation      |
-- |                                                 | contributions (do not include           |
-- |                                                 | compulsory super guarantee amounts)     |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'reportableFringeBenefits'                      | Fringe benefits amount for FBT year     |
-- |                                                 | 1 April to 31 March, and employer       |
-- |                                                 | FBT exemption status.  See also         |
-- |                                                 | 'ReportableFringeBenefits'.             |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'allowances'                                    | List of 'allowance'.                    |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'lumpSumA'                                      | Lump sum A. Use 'lumpSumATypeR' or      |
-- |                                                 | 'lumpSumATypeT' to construct.           |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'lumpSumB'                                      | Lump sum B.                             |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'lumpSumD'                                      | Lump sum D.                             |
-- +-------------------------------------------------+-----------------------------------------+
--
data PaymentSummaryIndividualNonBusiness a = PaymentSummaryIndividualNonBusiness
  { _inbABN :: ABN
  , _inbWithholding :: Money a
  , _inbGross :: Money a
  , _inbGrossPaymentsType :: Maybe GrossPaymentsTypeIndividualNonBusiness
  , _inbRESC :: Money a
  , _inbFringeBenefits :: Maybe (ReportableFringeBenefits a)
  , _inbAllowances :: [Allowance a]
  , _inbLumpSumA :: Maybe (LumpSumA a)
  , _inbLumpSumB :: Money a
  , _inbLumpSumD :: Money a
  }

-- | Construct a new payment summary.  All amounts are initially zero.
newPaymentSummaryIndividualNonBusiness
  :: (Num a) => ABN -> PaymentSummaryIndividualNonBusiness a
newPaymentSummaryIndividualNonBusiness abn =
  PaymentSummaryIndividualNonBusiness abn
    mempty    -- total tax withheld
    mempty    -- gross payments
    Nothing   -- gross payments type
    mempty    -- reportable super contributions
    Nothing   -- reportable fringe benefits
    []        -- allowances
    Nothing   -- lump sum a
    mempty    -- lump sum b
    mempty    -- lump sum d

instance HasLumpSumD PaymentSummaryIndividualNonBusiness where
  lumpSumD = lens _inbLumpSumD (\s b -> s { _inbLumpSumD = b })

data GrossPaymentsTypeIndividualNonBusiness
  = GrossPaymentsTypeP
  -- ^ non super pensions or annuity.  See also 'grossPaymentsTypeP'.
  | GrossPaymentsTypeH
  -- ^ working holiday makers.  See also 'grossPaymentsTypeH'.
  deriving (Eq, Ord)

-- | Helper constructor - __P - non super pensions or annuity__
grossPaymentsTypeP :: Maybe GrossPaymentsTypeIndividualNonBusiness
grossPaymentsTypeP = Just GrossPaymentsTypeP

-- | Helper constructor - __H - working holiday makers__
grossPaymentsTypeH :: Maybe GrossPaymentsTypeIndividualNonBusiness
grossPaymentsTypeH = Just GrossPaymentsTypeH

instance (RealFrac a) => HasTaxableIncome PaymentSummaryIndividualNonBusiness a a where
  taxableIncome = to $ \s ->
    view grossPayments s
    <> foldOf (allowances . traverse . allowanceAmount) s
    <> foldOf (lumpSumA . traverse . lumpSumAAmount) s
    <> view (lumpSumB . to (wholeDollars . ($/ 20))) s
    -- TODO lump sums E

instance HasTaxWithheld PaymentSummaryIndividualNonBusiness a a where
  taxWithheld = totalTaxWithheld

instance HasTotalTaxWithheld PaymentSummaryIndividualNonBusiness where
  totalTaxWithheld = lens _inbWithholding (\s b -> s { _inbWithholding = b })

instance HasGrossPayments PaymentSummaryIndividualNonBusiness where
  grossPayments = lens _inbGross (\s b -> s { _inbGross = b })

instance HasGrossPaymentsType PaymentSummaryIndividualNonBusiness
    (Maybe GrossPaymentsTypeIndividualNonBusiness) where
  grossPaymentsType = lens _inbGrossPaymentsType (\s b -> s { _inbGrossPaymentsType = b })

instance HasReportableEmployerSuperannuationContributions PaymentSummaryIndividualNonBusiness where
  reportableEmployerSuperannuationContributions =
    lens _inbRESC (\s b -> s { _inbRESC = b })

allowances :: Lens' (PaymentSummaryIndividualNonBusiness a) [Allowance a]
allowances = lens _inbAllowances (\s b -> s { _inbAllowances = b })

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


-- | Objects which have a /GROSS PAYMENTS/ field.
class HasGrossPayments s where
  grossPayments :: Lens' (s a) (Money a)

-- | Objects which have a /gross payments type/ field.
-- The value type may vary.
--
class HasGrossPaymentsType s a where
  grossPaymentsType :: Lens' (s any) a

-- | Objects which have a /GROSS PAYMENTS/ field.
class HasTotalTaxWithheld s where
  totalTaxWithheld :: Lens' (s a) (Money a)

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


-- | Use 'allowance' to construct, and lenses 'allowanceDetail' and
-- 'allowanceAmount' for field access.
--
data Allowance a = Allowance
  { _allowanceDetail :: String
  , _allowanceAmount :: (Money a)
  }
  deriving (Eq, Ord)

allowance
  :: (RealFrac a)
  => String   -- ^ Allowance details
  -> Money a  -- ^ Amount (discards cents)
  -> Allowance a
allowance s = Allowance s . wholeDollars

allowanceDetail :: Lens' (Allowance a) String
allowanceDetail = lens _allowanceDetail (\s b -> s { _allowanceDetail = b })

allowanceAmount :: Lens (Allowance a) (Allowance b) (Money a) (Money b)
allowanceAmount = lens _allowanceAmount (\s b -> s { _allowanceAmount = b })


-- | Use 'lumpSumATypeR' or 'lumpSumATypeT' to construct, and lenses
-- 'lumpSumAType' and 'lumpSumAAmount' for field access.
data LumpSumA a = LumpSumA
  { _lumpSumAType   :: LumpSumAType
  , _lumpSumAAmount :: Money a
  }
  deriving (Eq, Ord)

data LumpSumAType
  = LumpSumATypeR
  -- ^ if the payment was made for a genuine redundancy, invalidity
  -- or under an early retirement scheme
  | LumpSumATypeT
  -- ^ if the payment was made for any other reason
  deriving (Eq, Ord)

-- | A /lump sum A/ payment made for genuine redundancy, invalidity
-- or under an early retirement scheme.  See also 'lumpSumATypeT'.
lumpSumATypeR :: (RealFrac a) => Money a -> LumpSumA a
lumpSumATypeR = LumpSumA LumpSumATypeR . wholeDollars

-- | A /lump sum A/ payment made for any other reason.
-- See also 'lumpSumATypeR'.
lumpSumATypeT :: (RealFrac a) => Money a -> LumpSumA a
lumpSumATypeT = LumpSumA LumpSumATypeT . wholeDollars

lumpSumAType :: Lens' (LumpSumA a) LumpSumAType
lumpSumAType = lens _lumpSumAType (\s b -> s { _lumpSumAType = b })

lumpSumAAmount :: Lens (LumpSumA a) (LumpSumA b) (Money a) (Money b)
lumpSumAAmount = lens _lumpSumAAmount (\s b -> s { _lumpSumAAmount = b })

class HasLumpSumA s where
  lumpSumA :: Lens' (s a) (Maybe (LumpSumA a))

instance HasLumpSumA PaymentSummaryIndividualNonBusiness where
  lumpSumA = lens _inbLumpSumA (\s b -> s { _inbLumpSumA = b })


lumpSumB :: Lens' (PaymentSummaryIndividualNonBusiness a) (Money a)
lumpSumB = lens _inbLumpSumB (\s b -> s { _inbLumpSumB = b })


class HasLumpSumD s where
  lumpSumD :: Lens' (s a) (Money a)
