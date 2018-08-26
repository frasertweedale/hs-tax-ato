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

Types and taxes in Australia.

No guarantee that computations are correct, complete or current.

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Tax.ATO
  (
  -- * Tax returns
    assessTax
  , TaxReturnInfo
  , newTaxReturnInfo
  , HasHelpBalance(..)
  , HasSfssBalance(..)
  , HasMLSExemption(..)
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

  -- ** PAYG Payment Summaries
  , PaymentSummary(..)
  , ABN

  -- ** Dividends and franking credits
  , Dividend(..)
  , dividendFrankingCredit
  , dividendAttributableIncome

  -- ** Tax offsets
  , Offsets
  , spouseContributionOffset
  , foreignTaxOffset

  -- ** Tax assessment
  , TaxAssessment
  , HasTaxWithheld(..)
  , taxDue
  , taxCreditsAndOffsets
  , taxBalance
  , taxCGTAssessment

  -- * Tax computations
  , individualTax
  , corporateTax

  -- * Miscellaneous
  , module Data.Tax
  ) where

import Control.Lens (Getter, Lens', lens, to, view)
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))

import Data.Tax
import Data.Tax.ATO.CGT
import Data.Tax.ATO.Common

-- | Data that can have an amount of tax withheld
class HasTaxWithheld a b c where
  taxWithheld :: Getter (a b) (Money c)

instance (Foldable t, HasTaxWithheld x a a, Num a)
            => HasTaxWithheld t (x a) a where
  taxWithheld = to (foldMap (view taxWithheld))

class HasHelpBalance a b where
  helpBalance :: Lens' (a b) (Money b)

class HasSfssBalance a b where
  sfssBalance :: Lens' (a b) (Money b)

class HasMLSExemption a where
  mlsExemption :: Lens' (a b) Bool

-- | Individual tax return information.
-- Use 'newTaxReturnInfo' to construct.  The following lenses are
-- available:
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
-- | 'ess'              | Employee Share Scheme (ESS) data |
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
data TaxReturnInfo a = TaxReturnInfo
  { _mlsExemption :: Bool
  , _helpBalance :: Money a
  , _sfssBalance :: Money a
  , _paymentSummaries :: [PaymentSummary a]
  , _interest :: Money a        -- TODO dedicated type for income + withholding
  , _dividends :: [Dividend a]  -- TODO dedicated sum type
  , _ess :: Money a             -- TODO dedicated type
  , _foreignIncome :: Money a
  , _cgtEvents :: [CGTEvent a]
  , _deductions :: Money a
  , _offsets :: Offsets a
  , _triCapitalLossCarryForward :: Money a
  }

-- | Construct a new 'TaxReturnInfo'.
--
-- All monetary fields and lists are initially empty.
-- The /Medicare levy exemption/ field is __@False@__.
--
newTaxReturnInfo :: Num a => TaxReturnInfo a
newTaxReturnInfo = TaxReturnInfo
  False  -- MLS exemption
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

instance HasHelpBalance TaxReturnInfo b where
  helpBalance = lens _helpBalance (\s b -> s { _helpBalance = b })

instance HasSfssBalance TaxReturnInfo b where
  sfssBalance = lens _sfssBalance (\s b -> s { _sfssBalance = b })

instance HasMLSExemption TaxReturnInfo where
  mlsExemption = lens _mlsExemption (\s b -> s { _mlsExemption = b })

instance HasCapitalLossCarryForward TaxReturnInfo a where
  capitalLossCarryForward = lens _triCapitalLossCarryForward
      (\s b -> s { _triCapitalLossCarryForward = b })

paymentSummaries :: Lens' (TaxReturnInfo a) [PaymentSummary a]
paymentSummaries = lens _paymentSummaries (\s b -> s { _paymentSummaries = b })

interest :: Lens' (TaxReturnInfo a) (Money a)
interest = lens _interest (\s b -> s { _interest = b })

dividends :: Lens' (TaxReturnInfo a) [Dividend a]
dividends = lens _dividends (\s b -> s { _dividends = b })

ess :: Lens' (TaxReturnInfo a) (Money a)
ess = lens _ess (\s b -> s { _ess = b })

foreignIncome :: Lens' (TaxReturnInfo a) (Money a)
foreignIncome = lens _foreignIncome (\s b -> s { _foreignIncome = b })

cgtEvents :: Lens' (TaxReturnInfo a) [CGTEvent a]
cgtEvents = lens _cgtEvents (\s b -> s { _cgtEvents = b })

deductions :: Lens' (TaxReturnInfo a) (Money a)
deductions = lens _deductions (\s b -> s { _deductions = b })

offsets :: Lens' (TaxReturnInfo a) (Offsets a)
offsets = lens _offsets (\s b -> s { _offsets = b })


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

taxBalance :: Num a => Getter (TaxAssessment a) (Money a)
taxBalance = to $ \a ->
  view taxWithheld a
  $-$ view taxDue a
  $+$ view taxCreditsAndOffsets a

instance (Num a, Eq a) => HasCapitalLossCarryForward TaxAssessment a where
  capitalLossCarryForward = taxCGTAssessment . capitalLossCarryForward


individualTax
  ::  ( Fractional a, Ord a
      , HasMLSExemption info
      , HasHelpBalance info a
      , HasSfssBalance info a
      )
  => TaxTables a
  -> info a
  -> Tax (Money a) (Money a)    -- grand unified individual income tax
individualTax (TaxTables tax ml mls help sfss more) info =
    greaterOf mempty $
      tax
      <> ml  -- TODO medicare levy exemption
      <> (if view mlsExemption info then mempty else mls)
      <> limit (view helpBalance info) help
      <> limit (view sfssBalance info) sfss
      <> more

-- | Taxable income
instance (Fractional a, Ord a) => HasIncome TaxReturnInfo a a where
  income = to $ \info ->
    let
      cf = view capitalLossCarryForward info
      gross =
        view (paymentSummaries . income) info
        <> view interest info
        <> foldMap dividendAttributableIncome (view dividends info)
        -- <> managedFundIncome
        <> view (cgtEvents . to (assessCGTEvents cf) . cgtNetGain) info
        <> view foreignIncome info
        <> view ess info
    in
      gross $-$ view deductions info

instance (Num a) => HasTaxWithheld TaxReturnInfo a a where
  taxWithheld = to $ \info ->
    view (paymentSummaries . taxWithheld) info
    -- <> interestWithheld TODO

assessTax
  :: (Fractional a, Ord a)
  => TaxTables a -> TaxReturnInfo a -> TaxAssessment a
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

type ABN = String
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


data Dividend a = Dividend
  { dividendSource :: String
  , dividendDate :: String  -- FUTURE better type
  , dividendNetPayment :: Money a
  , dividendFrankedPortion :: a    -- ^ Franked ratio (@1@ = 100%)
  , dividendTaxWithheld :: Money a
  }
  deriving (Show)

instance HasTaxWithheld Dividend a a where
  taxWithheld = to dividendTaxWithheld

-- | Calculate the franking credit for a dividend
--
dividendFrankingCredit :: (Fractional a) => Dividend a -> Money a
dividendFrankingCredit d =
  dividendFrankedPortion d
  *$ getTax corporateTax (dividendNetPayment d $* (1 / 0.7))

dividendAttributableIncome :: (Fractional a) => Dividend a -> Money a
dividendAttributableIncome d =
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
