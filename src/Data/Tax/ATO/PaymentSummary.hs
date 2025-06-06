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

  -- *** Lump sum E
  , HasLumpSumE(..)

  -- *** Exempt foreign employment income
  , exemptForeignEmploymentIncome

  -- ** Foreign employment
  , PaymentSummaryForeignEmployment
  , newPaymentSummaryForeignEmployment
  , GrossPaymentsTypeForeignEmployment(..)
  , foreignTaxPaid

  -- ** Business and personal services income
  , PaymentSummaryBusinessAndPersonalServicesIncome
  , newPaymentSummaryBusinessAndPersonalServicesIncome

  -- ** Withholding where ABN not quoted
  , PaymentSummaryWithholdingWhereABNNotQuoted
  , newPaymentSummaryWithholdingWhereABNNotQuoted

  -- ** Payer details
  , PayerDetails
  , newPayerDetails
  , payerABN
  , payerBranchNumber
  , payerName
  , HasPayerDetails(..)

  -- ** Classes and helpers
  , HasGrossPayments(..)
  , HasGrossPaymentsType(..)
  , HasTotalTaxWithheld(..)
  , HasReportableEmployerSuperannuationContributions(..)
  , pattern PaymentSummary
  )
  where

import Data.String (IsString(..))

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
  :: (Num a) => PayerDetails -> Money a -> Money a -> Money a -> PaymentSummaryIndividualNonBusiness a
pattern PaymentSummary payer gross tax resc <-
  PaymentSummaryIndividualNonBusiness payer tax gross _type resc _fb
    _allow _lumpA _lumpB _lumpD _lumpE _foreign
  where
  PaymentSummary payer gross tax resc = newPaymentSummaryIndividualNonBusiness payer
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
-- | 'payerDetails'                                  | Use 'newPayerDetails' to construct.     |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'totalTaxWithheld'                              | TOTAL TAX WITHHELD                      |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'grossPayments'                                 | GROSS PAYMENTS.  Do not include amounts |
-- |                                                 | shown at 'allowances', 'lumpSumA',      |
-- |                                                 | 'lumpSumB', 'lumpSumD', 'lumpSumE',     |
-- |                                                 | or 'exemptForeignEmploymentIncome'.     |
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
-- | 'exemptForeignEmploymentIncome'                 |                                         |
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
-- | 'lumpSumE'                                      | Lump sum E - payments in arrears.  This |
-- |                                                 | amount is included in taxable income.   |
-- |                                                 | The tax offset calculations are not yet |
-- |                                                 | implemented.                            |
-- +-------------------------------------------------+-----------------------------------------+
--
data PaymentSummaryIndividualNonBusiness a = PaymentSummaryIndividualNonBusiness
  { _inbPayer :: PayerDetails
  , _inbWithholding :: Money a
  , _inbGross :: Money a
  , _inbGrossPaymentsType :: Maybe GrossPaymentsTypeIndividualNonBusiness
  , _inbRESC :: Money a
  , _inbFringeBenefits :: Maybe (ReportableFringeBenefits a)
  , _inbAllowances :: [Allowance a]
  , _inbLumpSumA :: Maybe (LumpSumA a)
  , _inbLumpSumB :: Money a
  , _inbLumpSumD :: Money a
  , _inbLumpSumE :: Money a
  , _inbForeign :: Money a
  }

-- | Construct a new payment summary.  All amounts are initially zero.
newPaymentSummaryIndividualNonBusiness
  :: (Num a) => PayerDetails -> PaymentSummaryIndividualNonBusiness a
newPaymentSummaryIndividualNonBusiness payer =
  PaymentSummaryIndividualNonBusiness payer
    mempty    -- total tax withheld
    mempty    -- gross payments
    Nothing   -- gross payments type
    mempty    -- reportable super contributions
    Nothing   -- reportable fringe benefits
    []        -- allowances
    Nothing   -- lump sum a
    mempty    -- lump sum b
    mempty    -- lump sum d
    mempty    -- lump sum e
    mempty    -- exempt foreign employment income

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
    <> view lumpSumE s

instance HasTaxWithheld PaymentSummaryIndividualNonBusiness a a where
  taxWithheld = totalTaxWithheld

instance HasPayerDetails PaymentSummaryIndividualNonBusiness where
  payerDetails = lens _inbPayer (\s b -> s { _inbPayer = b })

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

instance HasReportableFringeBenefits PaymentSummaryIndividualNonBusiness where
  reportableFringeBenefits =
    lens _inbFringeBenefits (\s b -> s { _inbFringeBenefits = b })

instance HasLumpSumA PaymentSummaryIndividualNonBusiness where
  lumpSumA = lens _inbLumpSumA (\s b -> s { _inbLumpSumA = b })

lumpSumB :: Lens' (PaymentSummaryIndividualNonBusiness a) (Money a)
lumpSumB = lens _inbLumpSumB (\s b -> s { _inbLumpSumB = b })

instance HasLumpSumD PaymentSummaryIndividualNonBusiness where
  lumpSumD = lens _inbLumpSumD (\s b -> s { _inbLumpSumD = b })

instance HasLumpSumE PaymentSummaryIndividualNonBusiness where
  lumpSumE = lens _inbLumpSumE (\s b -> s { _inbLumpSumE = b })

exemptForeignEmploymentIncome :: Lens' (PaymentSummaryIndividualNonBusiness a) (Money a)
exemptForeignEmploymentIncome = lens _inbForeign (\s b -> s { _inbForeign = b })

allowances :: Lens' (PaymentSummaryIndividualNonBusiness a) [Allowance a]
allowances = lens _inbAllowances (\s b -> s { _inbAllowances = b })


-- | PAYG payment summary - foreign employment
--
-- Use 'newPaymentSummaryForeignEmployment' to construct this data
-- type (all amounts initially zero).
--
-- To @set@ and @view@ the various fields, these lenses are available:
--
-- +-------------------------------------------------+-----------------------------------------+
-- | 'payerDetails'                                  | Use 'newPayerDetails' to construct.     |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'totalTaxWithheld'                              | TOTAL TAX WITHHELD                      |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'grossPayments'                                 | GROSS PAYMENTS.  You __must__ include   |
-- |                                                 | allowances.  Do not include 'lumpSumA'  |
-- |                                                 | 'lumpSumD' or 'lumpSumE' amounts.       |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'grossPaymentsType'                             | Initial value 'GrossPaymentsTypeF'.     |
-- |                                                 | @set@ to 'GrossPaymentsTypeJ' for income|
-- |                                                 | earned from work conducted in the Joint |
-- |                                                 | Petroleum Development Area (JPDA).      |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'foreignTaxPaid'                                |                                         |
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
-- | 'lumpSumA'                                      | Lump sum A. Use 'lumpSumATypeR' or      |
-- |                                                 | 'lumpSumATypeT' to construct.           |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'lumpSumD'                                      | Lump sum D.                             |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'lumpSumE'                                      | Lump sum E - payments in arrears.  This |
-- |                                                 | amount is included in taxable income.   |
-- |                                                 | The tax offset calculations are not yet |
-- |                                                 | implemented.                            |
-- +-------------------------------------------------+-----------------------------------------+
--
data PaymentSummaryForeignEmployment a = PaymentSummaryForeignEmployment
  { _foreignPayer :: PayerDetails
  , _foreignWithholding :: Money a
  , _foreignGross :: Money a
  , _foreignGrossPaymentsType :: GrossPaymentsTypeForeignEmployment
  , _foreignForeignTaxPaid :: Money a
  , _foreignRESC :: Money a
  , _foreignFringeBenefits :: Maybe (ReportableFringeBenefits a)
  , _foreignLumpSumA :: Maybe (LumpSumA a)
  , _foreignLumpSumD :: Money a
  , _foreignLumpSumE :: Money a
  }

-- | Construct a new payment summary.  All amounts are initially zero.
newPaymentSummaryForeignEmployment
  :: (Num a) => PayerDetails -> PaymentSummaryForeignEmployment a
newPaymentSummaryForeignEmployment payer =
  PaymentSummaryForeignEmployment payer
    mempty    -- total tax withheld
    mempty    -- gross payments
    GrossPaymentsTypeF   -- gross payments type
    mempty    -- foreign tax paid
    mempty    -- reportable super contributions
    Nothing   -- reportable fringe benefits
    Nothing   -- lump sum a
    mempty    -- lump sum d
    mempty    -- lump sum e

data GrossPaymentsTypeForeignEmployment
  = GrossPaymentsTypeF
  -- ^ foreign employment income
  | GrossPaymentsTypeJ
  -- ^ income earned from work conducted in the Joint Petroleum
  -- Development Area (JPDA).
  deriving (Eq, Ord)

instance (RealFrac a) => HasTaxableIncome PaymentSummaryForeignEmployment a a where
  taxableIncome = to $ \s ->
    view grossPayments s
    <> foldOf (lumpSumA . traverse . lumpSumAAmount) s
    <> view lumpSumE s

instance HasTaxWithheld PaymentSummaryForeignEmployment a a where
  taxWithheld = totalTaxWithheld

instance HasPayerDetails PaymentSummaryForeignEmployment where
  payerDetails = lens _foreignPayer (\s b -> s { _foreignPayer = b })

instance HasTotalTaxWithheld PaymentSummaryForeignEmployment where
  totalTaxWithheld = lens _foreignWithholding (\s b -> s { _foreignWithholding = b })

instance HasGrossPayments PaymentSummaryForeignEmployment where
  grossPayments = lens _foreignGross (\s b -> s { _foreignGross = b })

instance HasGrossPaymentsType PaymentSummaryForeignEmployment
    GrossPaymentsTypeForeignEmployment where
  grossPaymentsType = lens _foreignGrossPaymentsType (\s b -> s { _foreignGrossPaymentsType = b })

foreignTaxPaid :: Lens' (PaymentSummaryForeignEmployment a) (Money a)
foreignTaxPaid =
  lens _foreignForeignTaxPaid (\s b -> s { _foreignForeignTaxPaid = b })

instance HasReportableEmployerSuperannuationContributions PaymentSummaryForeignEmployment where
  reportableEmployerSuperannuationContributions =
    lens _foreignRESC (\s b -> s { _foreignRESC = b })

instance HasReportableFringeBenefits PaymentSummaryForeignEmployment where
  reportableFringeBenefits =
    lens _foreignFringeBenefits (\s b -> s { _foreignFringeBenefits = b })

instance HasLumpSumA PaymentSummaryForeignEmployment where
  lumpSumA = lens _foreignLumpSumA (\s b -> s { _foreignLumpSumA = b })

instance HasLumpSumD PaymentSummaryForeignEmployment where
  lumpSumD = lens _foreignLumpSumD (\s b -> s { _foreignLumpSumD = b })

instance HasLumpSumE PaymentSummaryForeignEmployment where
  lumpSumE = lens _foreignLumpSumE (\s b -> s { _foreignLumpSumE = b })


-- | PAYG payment summary - business and personal services income
--
-- Use 'newPaymentSummaryBusinessAndPersonalServicesIncome' to construct this data
-- type (all amounts initially zero).
--
-- To @set@ and @view@ the various fields, these lenses are available:
--
-- +-------------------------------------------------+-----------------------------------------+
-- | 'payerDetails'                                  | Use 'newPayerDetails' to construct.     |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'totalTaxWithheld'                              | TOTAL TAX WITHHELD                      |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'grossPayments'                                 | Gross payments or gross attributed      |
-- |                                                 | income.                                 |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'reportableEmployerSuperannuationContributions' | Reportable employer superannuation      |
-- |                                                 | contributions (do not include           |
-- |                                                 | compulsory super guarantee amounts)     |
-- +-------------------------------------------------+-----------------------------------------+
--
data PaymentSummaryBusinessAndPersonalServicesIncome a = PaymentSummaryBusinessAndPersonalServicesIncome
  { _bpsiPayer :: PayerDetails
  , _bpsiWithholding :: Money a
  , _bpsiGross :: Money a
  , _bpsiRESC :: Money a
  }

-- | Construct a new payment summary.  All amounts are initially zero.
newPaymentSummaryBusinessAndPersonalServicesIncome
  :: (Num a) => PayerDetails -> PaymentSummaryBusinessAndPersonalServicesIncome a
newPaymentSummaryBusinessAndPersonalServicesIncome payer =
  PaymentSummaryBusinessAndPersonalServicesIncome payer
    mempty    -- total tax withheld
    mempty    -- gross payments
    mempty    -- reportable super contributions

instance HasTaxableIncome PaymentSummaryBusinessAndPersonalServicesIncome a a where
  taxableIncome = grossPayments

instance HasTaxWithheld PaymentSummaryBusinessAndPersonalServicesIncome a a where
  taxWithheld = totalTaxWithheld

instance HasPayerDetails PaymentSummaryBusinessAndPersonalServicesIncome where
  payerDetails = lens _bpsiPayer (\s b -> s { _bpsiPayer = b })

instance HasTotalTaxWithheld PaymentSummaryBusinessAndPersonalServicesIncome where
  totalTaxWithheld = lens _bpsiWithholding (\s b -> s { _bpsiWithholding = b })

instance HasGrossPayments PaymentSummaryBusinessAndPersonalServicesIncome where
  grossPayments = lens _bpsiGross (\s b -> s { _bpsiGross = b })

instance HasReportableEmployerSuperannuationContributions PaymentSummaryBusinessAndPersonalServicesIncome where
  reportableEmployerSuperannuationContributions =
    lens _bpsiRESC (\s b -> s { _bpsiRESC = b })


-- | PAYG payment summary - withholding where ABN not quoted (other
-- than those covered by annual investment income reporting).
--
-- Use 'newPaymentSummaryWithholdingWhereABNNotQuoted' to construct this data
-- type (all amounts initially zero).
--
-- To @set@ and @view@ the various fields, these lenses are available:
--
-- +-------------------------------------------------+-----------------------------------------+
-- | 'payerDetails'                                  | Use 'newPayerDetails' to construct.     |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'totalTaxWithheld'                              | TOTAL AMOUNT WITHHELD                   |
-- +-------------------------------------------------+-----------------------------------------+
-- | 'grossPayments'                                 | Gross payment (including the market     |
-- |                                                 | value of non-cash benefits).            |
-- +-------------------------------------------------+-----------------------------------------+
--
data PaymentSummaryWithholdingWhereABNNotQuoted a = PaymentSummaryWithholdingWhereABNNotQuoted
  { _noabnPayer :: PayerDetails
  , _noabnWithholding :: Money a
  , _noabnGross :: Money a
  }

-- | Construct a new payment summary.  All amounts are initially zero.
newPaymentSummaryWithholdingWhereABNNotQuoted
  :: (Num a) => PayerDetails -> PaymentSummaryWithholdingWhereABNNotQuoted a
newPaymentSummaryWithholdingWhereABNNotQuoted payer =
  PaymentSummaryWithholdingWhereABNNotQuoted payer
    mempty    -- total tax withheld
    mempty    -- gross payments

instance HasTaxableIncome PaymentSummaryWithholdingWhereABNNotQuoted a a where
  taxableIncome = grossPayments

instance HasTaxWithheld PaymentSummaryWithholdingWhereABNNotQuoted a a where
  taxWithheld = totalTaxWithheld

instance HasPayerDetails PaymentSummaryWithholdingWhereABNNotQuoted where
  payerDetails = lens _noabnPayer (\s b -> s { _noabnPayer = b })

instance HasTotalTaxWithheld PaymentSummaryWithholdingWhereABNNotQuoted where
  totalTaxWithheld = lens _noabnWithholding (\s b -> s { _noabnWithholding = b })

instance HasGrossPayments PaymentSummaryWithholdingWhereABNNotQuoted where
  grossPayments = lens _noabnGross (\s b -> s { _noabnGross = b })


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


-- | Objects which may have reportable fringe benefit amounts
class HasReportableFringeBenefits s where
  reportableFringeBenefits :: Lens' (s a) (Maybe (ReportableFringeBenefits a))

-- | Helper constructor for fringe benefits amount (employer not exempt from FBT).
-- See also 'fringeBenefitsEmployerExempt'.
fringeBenefitsEmployerNotExempt :: Money a -> Maybe (ReportableFringeBenefits a)
fringeBenefitsEmployerNotExempt x = Just $ ReportableFringeBenefits x EmployerNotFBTExempt

-- | Helper constructor for fringe benefits amount (employer exempt from FBT)
-- See also 'fringeBenefitsEmployerNotExempt'.
fringeBenefitsEmployerExempt :: Money a -> Maybe (ReportableFringeBenefits a)
fringeBenefitsEmployerExempt x = Just $ ReportableFringeBenefits x EmployerFBTExempt

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


class HasLumpSumD s where
  lumpSumD :: Lens' (s a) (Money a)


class HasLumpSumE s where
  lumpSumE :: Lens' (s a) (Money a)


-- | Payer details.  Use 'newPayerDetails' to construct.
-- Use 'payerABN', 'payerBranchNumber' and 'payerName' lenses to view or
-- set the fields.
--
-- Use the 'payerDetails' classy optic to access the payer details field
-- in payment summary data types.
--
data PayerDetails = PayerDetails
  { _payerABN :: ABN
  , _payerBranch :: Maybe Int
  , _payerName :: String
  }
  deriving (Eq, Ord)

-- | __Deprecated__ instance that interprets string as ABN and
-- sets payer name to the empty string.
--
instance IsString PayerDetails where
  fromString s = PayerDetails (fromString s) Nothing ""

newPayerDetails :: ABN -> Maybe Int -> String -> PayerDetails
newPayerDetails = PayerDetails

payerABN :: Lens' PayerDetails ABN
payerABN = lens _payerABN (\s b -> s { _payerABN = b })

payerBranchNumber :: Lens' PayerDetails (Maybe Int)
payerBranchNumber = lens _payerBranch (\s b -> s { _payerBranch = b })

payerName :: Lens' PayerDetails String
payerName = lens _payerName (\s b -> s { _payerName = b })

class HasPayerDetails s where
  payerDetails :: Lens' (s a) PayerDetails
