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

Types and computations for taxes in Australia.

No guarantee that computations are correct, complete or current.

Lots of things are not implemented, including (but not limited to):
__ETPs__, income from __partnerships and trusts__,
__superannuation__ income streams and lump payments, tax losses from
previous years, __Medicare levy reduction/exemption__, adjustments,
and variations based on family income and dependents.

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Tax.ATO
  (
  -- * Tax returns
    assessTax
  , TaxReturnInfo
  , newTaxReturnInfo
  , newTaxReturnInfoForTables
  , HasCapitalLossCarryForward(..)
  , paymentSummaries
  , income
  , interest
  , dividends
  , ess
  , foreignIncome
  , cgtEvents
  , deductions
  , offsets
  , mlsExemption
  , helpBalance
  , sfssBalance

  -- ** PAYG Payment Summaries
  , PaymentSummary(..)
  , ABN

  -- ** Employee share schemes
  , ESSStatement
  , newESSStatement
  , essTaxedUpfrontReduction
  , essTaxedUpfrontNoReduction
  , essDeferral
  , essPre2009
  , essTFNAmounts
  , essForeignSourceDiscounts

  -- ** Dividends and franking credits
  , Dividend(..)
  , dividendFrankingCredit

  -- ** Tax offsets
  , Offsets
  , spouseContributionOffset
  , foreignTaxOffset

  -- ** Tax assessment
  , TaxAssessment
  , taxDue
  , taxCreditsAndOffsets
  , taxBalance
  , taxCGTAssessment

  -- * Tax computations
  , individualTax
  , corporateTax

  -- * Miscellaneous
  , GrossAndWithheld(..)
  , HasTaxWithheld(..)
  , Proportion
  , getProportion
  , proportion
  , module Data.Tax
  ) where

import Control.Lens (Getter, Lens', lens, to, view)
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))

import Data.Tax
import Data.Tax.ATO.CGT
import Data.Tax.ATO.Common
import Data.Tax.ATO.Days

-- | Data that can have an amount of tax withheld
class HasTaxWithheld a b c where
  taxWithheld :: Getter (a b) (Money c)

instance (Foldable t, HasTaxWithheld x a a, Num a)
            => HasTaxWithheld t (x a) a where
  taxWithheld = to (foldMap (view taxWithheld))

-- | Individual tax return information.
--
-- Use 'newTaxReturnInfo' to construct.  Alternatively,
-- 'newTaxReturnInfoForTables' can be used to coerce the type
-- parameters to be the same as some 'TaxTables'.
--
-- The following lenses are available:
--
-- +--------------------+----------------------------------+
-- | 'mlsExemption'     | Medicare levy exemption          |
-- +--------------------+----------------------------------+
-- | 'helpBalance'      | HELP account balance             |
-- +--------------------+----------------------------------+
-- | 'sfssBalance'      | SFSS account balance             |
-- +--------------------+----------------------------------+
-- | 'paymentSummaries' | PAYG payment summaries           |
-- +--------------------+----------------------------------+
-- | 'interest'         | Interest data                    |
-- +--------------------+----------------------------------+
-- | 'dividends'        | Dividend data                    |
-- +--------------------+----------------------------------+
-- | 'ess'              | Employee Share Scheme statement  |
-- +--------------------+----------------------------------+
-- | 'foreignIncome'    | Foreign income                   |
-- +--------------------+----------------------------------+
-- | 'cgtEvents'        | Capital gains and losses         |
-- +--------------------+----------------------------------+
-- | 'deductions'       | Deductions                       |
-- +--------------------+----------------------------------+
-- | 'offsets'          | Tax offsets                      |
-- +--------------------+----------------------------------+
--
data TaxReturnInfo y a = TaxReturnInfo
  { _mlsExemption :: Days y
  , _helpBalance :: Money a
  , _sfssBalance :: Money a
  , _paymentSummaries :: [PaymentSummary a]
  , _interest :: GrossAndWithheld a
  , _dividends :: [Dividend a]
  , _ess :: ESSStatement a
  , _foreignIncome :: Money a
  , _cgtEvents :: [CGTEvent a]
  , _deductions :: Money a
  , _offsets :: Offsets a
  , _triCapitalLossCarryForward :: Money a
  }

-- | Construct a new 'TaxReturnInfo'.
--
-- All monetary fields and lists are initially empty.
--
-- The /Medicare levy surcharge exemption/ field is initially
-- set to the number of days in the year (i.e. the taxpayer is
-- fully exempt).
--
newTaxReturnInfo
  :: (DaysInYear y, Num a)
  => TaxReturnInfo y a
newTaxReturnInfo = TaxReturnInfo
  daysAll  -- MLS exemption
  mempty -- HELP
  mempty -- SFSS
  mempty -- payment summaries
  mempty -- interest
  mempty -- dividends
  mempty -- ESS
  mempty -- foreign income
  mempty -- CGT events
  mempty -- deductions
  mempty -- offsets
  mempty -- cap loss carry forward

-- | Construct a 'TaxReturnInfo' per 'newTaxReturnInfo',
-- coercing the type parameters to match the 'TaxTables'
-- argument (which is ignored).
--
newTaxReturnInfoForTables
  :: (DaysInYear y, Num a)
  => TaxTables y a -> TaxReturnInfo y a
newTaxReturnInfoForTables _ = newTaxReturnInfo

instance HasCapitalLossCarryForward (TaxReturnInfo y) a where
  capitalLossCarryForward = lens _triCapitalLossCarryForward
      (\s b -> s { _triCapitalLossCarryForward = b })

helpBalance :: Lens' (TaxReturnInfo y a) (Money a)
helpBalance = lens _helpBalance (\s b -> s { _helpBalance = b })

sfssBalance :: Lens' (TaxReturnInfo y a) (Money a)
sfssBalance = lens _sfssBalance (\s b -> s { _sfssBalance = b })

mlsExemption :: Lens' (TaxReturnInfo y a) (Days y)
mlsExemption = lens _mlsExemption (\s b -> s { _mlsExemption = b })

paymentSummaries :: Lens' (TaxReturnInfo y a) [PaymentSummary a]
paymentSummaries = lens _paymentSummaries (\s b -> s { _paymentSummaries = b })

interest :: Lens' (TaxReturnInfo y a) (GrossAndWithheld a)
interest = lens _interest (\s b -> s { _interest = b })

dividends :: Lens' (TaxReturnInfo y a) [Dividend a]
dividends = lens _dividends (\s b -> s { _dividends = b })

ess :: Lens' (TaxReturnInfo y a) (ESSStatement a)
ess = lens _ess (\s b -> s { _ess = b })

foreignIncome :: Lens' (TaxReturnInfo y a) (Money a)
foreignIncome = lens _foreignIncome (\s b -> s { _foreignIncome = b })

cgtEvents :: Lens' (TaxReturnInfo y a) [CGTEvent a]
cgtEvents = lens _cgtEvents (\s b -> s { _cgtEvents = b })

deductions :: Lens' (TaxReturnInfo y a) (Money a)
deductions = lens _deductions (\s b -> s { _deductions = b })

offsets :: Lens' (TaxReturnInfo y a) (Offsets a)
offsets = lens _offsets (\s b -> s { _offsets = b })


-- | A tax assessment.  Use 'assessTax' to compute a
-- @TaxAssessment@.
data TaxAssessment a = TaxAssessment
  { _taxableIncome :: Money a
  , _taxDue :: Money a
  , _taxWithheld :: Money a
  , _taxCreditsAndOffsets :: Money a
  , _taCGTAssessment :: CGTAssessment a
  }

-- | Taxable income
instance HasIncome TaxAssessment a a where
  income = to _taxableIncome

instance HasTaxWithheld TaxAssessment a a where
  taxWithheld = to _taxWithheld

taxDue :: Getter (TaxAssessment a) (Money a)
taxDue = to _taxDue

taxCreditsAndOffsets :: Getter (TaxAssessment a) (Money a)
taxCreditsAndOffsets = to _taxCreditsAndOffsets

taxCGTAssessment :: Lens' (TaxAssessment a) (CGTAssessment a)
taxCGTAssessment = lens _taCGTAssessment (\s b -> s { _taCGTAssessment = b })

-- | What is the balance of the assessment?  Positive means a
-- refund (tax withheld exceeds obligation), negative means a bill.
taxBalance :: Num a => Getter (TaxAssessment a) (Money a)
taxBalance = to $ \a ->
  view taxWithheld a
  $-$ view taxDue a
  $+$ view taxCreditsAndOffsets a

instance (Num a, Eq a) => HasCapitalLossCarryForward TaxAssessment a where
  capitalLossCarryForward = taxCGTAssessment . capitalLossCarryForward


-- | Consolidated individual tax rate incorporating
-- Medicare levy and surcharge, HELP and SFSS repayments
-- (if applicable) and automatic offsets (e.g. LITO).
individualTax
  :: (DaysInYear y, Fractional a, Ord a)
  => TaxTables y a
  -> TaxReturnInfo y a
  -> Tax (Money a) (Money a)    -- grand unified individual income tax
individualTax (TaxTables tax ml mls help sfss more) info =
  let
    mlsFrac = 1 - getFraction (view mlsExemption info)
  in
    greaterOf mempty $
      tax
      <> ml  -- TODO medicare levy exemption
      <> fmap ($* mlsFrac) mls -- FIXME income for MLS purposes
                               -- includes fringe benefits;
                               -- family thresholds apply
      <> limit (view helpBalance info) help
      <> limit (view sfssBalance info) sfss
      <> more

-- | Taxable income
instance (Fractional a, Ord a) => HasIncome (TaxReturnInfo y) a a where
  income = to $ \info ->
    let
      cf = view capitalLossCarryForward info
      gross =
        view (paymentSummaries . income) info
        <> view (interest . income) info
        <> view (dividends . income) info
        <> view (ess . income) info
        <> view (cgtEvents . to (assessCGTEvents cf) . cgtNetGain) info
        <> view foreignIncome info
    in
      gross $-$ view deductions info

instance (Num a) => HasTaxWithheld (TaxReturnInfo y) a a where
  taxWithheld = to $ \info ->
    view (paymentSummaries . taxWithheld) info
    <> view (interest . taxWithheld) info

-- | Assess a tax return, given tax tables and tax return info.
assessTax
  :: (DaysInYear y, Fractional a, Ord a)
  => TaxTables y a -> TaxReturnInfo y a -> TaxAssessment a
assessTax tables info =
  let
    cg = assessCGTEvents
          (view capitalLossCarryForward info) (view cgtEvents info)
    taxable = view income info
    due = getTax (individualTax tables info) taxable

    frankingCredit = foldMap dividendFrankingCredit (view dividends info)
    off =
      view (offsets . spouseContributionOffset) info
      <> view (offsets . foreignTaxOffset) info
  in
    TaxAssessment
      taxable
      due
      (view taxWithheld info)
      (frankingCredit <> off)
      cg

-- | Australian Business Number
type ABN = String

-- | PAYG payment summary
data PaymentSummary a = PaymentSummary
  { summaryABN :: ABN
  , summaryGross :: Money a
  , summaryWithheld :: Money a
  , reportableEmployerSuperannuationContributions :: Money a
  }

-- | Gross income
instance HasIncome PaymentSummary a a where
  income = to summaryGross

instance HasTaxWithheld PaymentSummary a a where
  taxWithheld = to summaryWithheld

-- | A proportion is a non-negative number in interval @[0,1]@.
-- Use 'proportion' to construct.
newtype Proportion a = Proportion
  { getProportion :: a -- ^ Return underlying figure, which is in interval @[0,1]@
  }
  deriving (Show, Eq, Ord)

-- | Construct a proportion.  Out of range numbers are clamped
-- to @0@ or @1@ (no runtime errors).
proportion :: (Ord a, Num a) => a -> Proportion a
proportion = Proportion . max 0 . min 1

-- | Dividend payment.  Records net income, franked portion
-- and amount of tax withheld.
data Dividend a = Dividend
  { dividendSource :: String
  , dividendDate :: String  -- FUTURE better type
  , dividendNetPayment :: Money a
  , dividendFrankedPortion :: Proportion a  -- ^ Franked ratio (@1@ = 100%)
  , dividendTaxWithheld :: Money a
  }
  deriving (Show)

instance HasTaxWithheld Dividend a a where
  taxWithheld = to dividendTaxWithheld

-- | Calculate the franking credit for a dividend
--
dividendFrankingCredit :: (Fractional a) => Dividend a -> Money a
dividendFrankingCredit d =
  (getProportion . dividendFrankedPortion) d
  *$ getTax corporateTax (dividendNetPayment d $* (1 / 0.7))

-- | Attributable income
instance (Fractional a) => HasIncome Dividend a a where
  income = to $ \d ->
    dividendNetPayment d
    <> dividendFrankingCredit d
    <> view taxWithheld d

-- | Tax offsets that individuals can claim
data Offsets a = Offsets
  { _spouseOffset :: Money a
  , _foreignTaxOffset :: Money a
  }

instance Num a => Semigroup (Offsets a) where
  Offsets a b <> Offsets a' b' = Offsets (a <> a') (b <> b')

instance Num a => Monoid (Offsets a) where
  mempty = Offsets mempty mempty
  Offsets a b `mappend` Offsets a' b' = Offsets (a `mappend` a') (b `mappend` b')

-- | Spouse contribution offset.  Maximum of /$540/ (not enforced).
spouseContributionOffset :: Lens' (Offsets a) (Money a)
spouseContributionOffset = lens _spouseOffset (\s b -> s { _spouseOffset = b })

-- | Offset for tax paid on foreign income.
foreignTaxOffset :: Lens' (Offsets a) (Money a)
foreignTaxOffset = lens _foreignTaxOffset (\s b -> s { _foreignTaxOffset = b })

-- | A gross income (first argument) and amount of tax withheld (second argument)
data GrossAndWithheld a = GrossAndWithheld (Money a) (Money a)

instance (Num a) => Semigroup (GrossAndWithheld a) where
  GrossAndWithheld a b <> GrossAndWithheld a' b' =
    GrossAndWithheld (a <> a') (b <> b')

instance (Num a) => Monoid (GrossAndWithheld a) where
  mempty = GrossAndWithheld mempty mempty
  GrossAndWithheld a b `mappend` GrossAndWithheld a' b' =
    GrossAndWithheld (a `mappend` a') (b `mappend` b')

instance HasIncome GrossAndWithheld a a where
  income = to $ \(GrossAndWithheld a _) -> a

instance HasTaxWithheld GrossAndWithheld a a where
  taxWithheld = to $ \(GrossAndWithheld _ a) -> a


-- | Employee share scheme statement.  Use 'newESSStatement' to construct.
data ESSStatement a = ESSStatement
  { _taxedUpfrontReduction :: Money a
  , _taxedUpfrontNoReduction :: Money a
  , _deferral :: Money a
  , _pre2009 :: Money a
  , _tfnAmounts :: Money a
  , _foreignSourceDiscounts :: Money a
  }

-- | Construct an 'ESSStatement' with all amounts at /zero/.
newESSStatement :: Num a => ESSStatement a
newESSStatement = ESSStatement mempty mempty mempty mempty mempty mempty

-- | Discount from taxed up front schemes—eligible for reduction.
-- Item __D__ in /Employee share schemes/ section.
essTaxedUpfrontReduction :: Lens' (ESSStatement a) (Money a)
essTaxedUpfrontReduction =
  lens _taxedUpfrontReduction (\s b -> s { _taxedUpfrontReduction = b })

-- | Discount from taxed up front schemes—not eligible for reduction
-- Item __E__ in /Employee share schemes/ section.
essTaxedUpfrontNoReduction :: Lens' (ESSStatement a) (Money a)
essTaxedUpfrontNoReduction =
  lens _taxedUpfrontNoReduction (\s b -> s { _taxedUpfrontNoReduction = b })

-- | Discount from taxed deferral schemes.
-- Item __F__ in /Employee share schemes/ section.
essDeferral :: Lens' (ESSStatement a) (Money a)
essDeferral = lens _deferral (\s b -> s { _deferral = b })

-- | discounts on ESS interests acquired pre 1 July 2009 and
-- "cessation time" occurred during the finanical year.
-- Item __G__ in /Employee share schemes/ section.
essPre2009 :: Lens' (ESSStatement a) (Money a)
essPre2009 = lens _pre2009 (\s b -> s { _pre2009 = b })

-- | TFN amounts withheld from discounts.
-- Item __C__ in /Employee share schemes/ section.
essTFNAmounts :: Lens' (ESSStatement a) (Money a)
essTFNAmounts = lens _tfnAmounts (\s b -> s { _tfnAmounts = b })

-- | ESS foreign source discounts
-- Item __A__ in /Employee share schemes/ section.
essForeignSourceDiscounts :: Lens' (ESSStatement a) (Money a)
essForeignSourceDiscounts =
  lens _foreignSourceDiscounts (\s b -> s { _foreignSourceDiscounts = b })

-- | __Note:__ does not implement the reduction of taxed up front
-- amounts eligible for reduction.
instance (Num a) => HasIncome ESSStatement a a where
  income = to $ \s ->
    view essTaxedUpfrontReduction s
    <> view essTaxedUpfrontNoReduction s
    <> view essDeferral s
    <> view essPre2009 s

instance (Num a) => Semigroup (ESSStatement a) where
  ESSStatement a b c d e f <> ESSStatement a' b' c' d' e' f' =
    ESSStatement (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f')

instance (Num a) => Monoid (ESSStatement a) where
  mempty = newESSStatement
  mappend = (<>)
