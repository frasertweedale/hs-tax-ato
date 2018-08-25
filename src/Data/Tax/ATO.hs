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
  , newOffsets
  , spouseContributionOffset
  , foreignTaxOffset

  -- ** Tax assessment
  , TaxAssessment
  , taxWithheld
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

import Data.Tax
import Data.Tax.ATO.CGT
import Data.Tax.ATO.Common

class HasHelpBalance a b where
  helpBalance :: Lens' (a b) (Money b)

class HasSfssBalance a b where
  sfssBalance :: Lens' (a b) (Money b)

class HasMLSExemption a where
  mlsExemption :: Lens' (a b) Bool

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
  newOffsets
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

taxDue :: Getter (TaxAssessment a) (Money a)
taxDue = to _taxDue

taxWithheld :: Getter (TaxAssessment a) (Money a)
taxWithheld = to _taxWithheld

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

assessTax
  :: (Fractional a, Ord a)
  => TaxTables a -> TaxReturnInfo a -> TaxAssessment a
assessTax tables info =
  let
    cg = assessCGTEvents
          (view capitalLossCarryForward info) (view cgtEvents info)
    taxable = view income info
    due = getTax (individualTax tables info) taxable
    wagesWithheld = foldMap summaryWithheld (view paymentSummaries info)
    withheld = wagesWithheld -- <> interestWithheld TODO

    frankingCredit = foldMap dividendFrankingCredit (view dividends info)
    off =
      view (offsets . spouseContributionOffset) info
      <> view (offsets . foreignTaxOffset) info
  in
    TaxAssessment
      taxable
      due
      withheld
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


data Dividend a = Dividend
  { dividendSource :: String
  , dividendDate :: String  -- FUTURE better type
  , dividendNetPayment :: Money a
  , dividendFrankedPortion :: a    -- ^ Franked ratio (@1@ = 100%)
  , dividendTaxWithheld :: Money a
  }
  deriving (Show)

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
  <> dividendTaxWithheld d

data Offsets a = Offsets
  { _spouseOffset :: Money a
  , _foreignTaxOffset :: Money a
  }

newOffsets :: Num a => Offsets a
newOffsets = Offsets mempty mempty

spouseContributionOffset :: Lens' (Offsets a) (Money a)
spouseContributionOffset = lens _spouseOffset (\s b -> s { _spouseOffset = b })

foreignTaxOffset :: Lens' (Offsets a) (Money a)
foreignTaxOffset = lens _foreignTaxOffset (\s b -> s { _foreignTaxOffset = b })
