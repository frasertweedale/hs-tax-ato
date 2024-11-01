{-# LANGUAGE PolyKinds #-}

-- This file is part of hs-tax-ato
-- Copyright (C) 2018-2021  Fraser Tweedale
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
  -- * Synopsis
  -- $synopsis

  -- * Individual tax returns
    TaxReturnInfo
  , newTaxReturnInfo
  , newTaxReturnInfoForTables
  , taxableIncome

  -- ** Income

  -- *** PAYG Payment Summaries
  , PaymentSummary(..)
  , paymentSummaries
  , ABN

  -- *** Interest
  , interest

  -- *** Dividends
  , Dividend(..)
  , dividends
  , dividendFromGross
  , dividendFromNet
  , dividendFromNetFranked
  , dividendFromNetFranked30

  -- *** Capital gains tax (CGT)
  , HasCapitalLossCarryForward(..)
  , cgtEvents

  -- *** Employee share schemes
  , ESSStatement
  , newESSStatement
  , ess
  , essTaxedUpfrontReduction
  , essTaxedUpfrontNoReduction
  , essDeferral
  , essPre2009
  , essTFNAmounts
  , essForeignSourceDiscounts

  -- *** Foreign income
  , foreignIncome

  -- ** Medicare Levy Surcharge and Private Health Insurance
  , mlsExemption
  , privateHealthInsurancePolicyDetails

  -- ** Student loan balances
  , helpBalance
  , sfssBalance

  -- ** Spouse details
  , SpouseDetails
  , spouseDetails
  , newSpouseDetails
  , spouseTaxableIncome

  -- ** Income Tests
  , IncomeTests
  , incomeTests
  , newIncomeTests
  , taxFreeGovernmentPensionsOrBenefits
  , targetForeignIncome
  , childSupportPaid
  , dependentChildren

  -- ** Deductions
  , Deductions
  , deductions
  , totalDeductions
  , workRelatedCarExpenses
  , workRelatedTravelExpenses
  , workRelatedClothingLaundryAndDryCleaningExpenses
  , workRelatedSelfEducationExpenses
  , otherWorkRelatedExpenses
  , lowValuePoolDeduction
  , interestDeductions
  , dividendDeductions
  , giftsOrDonations
  , costOfManagingTaxAffairs
  , deductibleAmountOfUndeductedPurchasePriceOfAForeignPensionOrAnnuity
  , personalSuperannuationContributions
  , deductionForProjectPool
  , forestryManagedInvestmentSchemeDeduction
  , otherDeductions
  , foreignIncomeDeductions

  -- ** Tax offsets
  , Offsets
  , offsets
  , spouseContributionOffset
  , foreignTaxOffset
  , paygInstalments

  -- ** Assessing tax
  , TaxAssessment
  , assessTax
  , taxBalance
  , taxDue
  , medicareLevyDue
  , studyAndTrainingLoanRepayment
  , taxCreditsAndOffsets
  , paygInstalmentsCredit
  , taxCGTAssessment
  , privateHealthInsuranceRebateAdjustment

  -- * Corporate tax
  , corporateTax

  -- * Miscellaneous
  , GrossAndWithheld(..)
  , HasTaxWithheld(..)
  , Proportion
  , getProportion
  , proportion
  , module Data.Tax
  , module Data.Tax.ATO.Depreciation
  , module Data.Tax.ATO.PrivateHealthInsuranceRebate
  , module Data.Tax.ATO.Rounding
  ) where

import Control.Lens (Getter, Lens', (&), foldOf, lens, set, to, view, views)
import Data.Time (Day)

import Data.Tax
import Data.Tax.ATO.CGT
import Data.Tax.ATO.Common
import Data.Tax.ATO.Depreciation
import Data.Tax.ATO.FY
import Data.Tax.ATO.PrivateHealthInsuranceRebate
import Data.Tax.ATO.Rounding

{- $synopsis

@
{-# LANGUAGE DataKinds #-}

import Control.Lens
import Data.Time
import Text.PrettyPrint (render)

import Data.Tax.ATO
import "Data.Tax.ATO.CGT"
import "Data.Tax.ATO.Pretty"

-- Import the tables for the financial year ending 30 June 2024
import qualified "Data.Tax.ATO.FY.FY2024" as FY

-- Convenience function for parsing a Data.Time.Day
day :: String -> Day
day = parseTimeOrError False defaultTimeLocale "%Y-%m-%d"

main :: IO ()
main = do
  let assessment = 'assessTax' FY.tables taxReturn
  putStrLn . render $ 'Data.Tax.ATO.Pretty.summariseTaxReturnInfo' taxReturn
  putStrLn ""
  putStrLn . render $ 'Data.Tax.ATO.Pretty.summariseAssessment' assessment

taxReturn :: 'TaxReturnInfo' FY.FY Rational
taxReturn = 'newTaxReturnInfo'
  & set 'paymentSummaries'
      [ 'PaymentSummary'
          "1234567890"      -- ABN
          (Money $ 180000)  -- Gross payments
          (Money 50000)     -- Tax withheld
          mempty            -- Reportable super contributions
      ]

  & set 'cgtEvents'
      [ 'CGTEvent'
          \"CODE\"             -- asset identifier (arbitrary string)
          10                 -- number of units (may be fractional)
          (day "2014-01-01") -- acquisition date
          (Money 30)         -- acquisition price
          (Money 20)         -- acquisition cost (brokerage)
          (day "2024-03-01") -- disposal date
          (Money 300)        -- disposal price
          (Money 20)         -- disposal cost (brokerage)
          mempty             -- capital costs
          mempty             -- costs of ownership
      ]

  & set 'dividends'
      [ 'dividendFromNetFranked30'
          \"CODE\"
          (day "2023-09-01")  -- payment date
          (Money 70)          -- net payment
          ('proportion' 1)      -- franking proportion (1 = 100%)
      ]

  & set 'helpBalance' (Money 10000)

  & set 'privateHealthInsurancePolicyDetails'
      [ 'PrivateHealthInsurancePolicyDetail'
          \"MBF\"
          "98765432"  -- policy number
          (Money 750) -- premiums eligible for rebate
          (Money 180) -- rebate received
          'BenefitCode30'
      , 'PrivateHealthInsurancePolicyDetail'
          "MBF" "98765432" (Money 250) (Money  60) 'BenefitCode31'
      ]

  & set ('deductions' . 'workRelatedTravelExpenses') (Money 1000)
  & set ('deductions' . 'personalSuperannuationContributions') (Money 5000)
@

-}

-- | Data that can have an amount of tax withheld
class HasTaxWithheld a b c where
  taxWithheld :: Getter (a b) (Money c)

instance (Foldable t, HasTaxWithheld x a a, Num a)
            => HasTaxWithheld t (x a) a where
  taxWithheld = to (foldMap (view taxWithheld))

-- TODO part year spouse
newtype SpouseDetails a = SpouseDetails
  { _spouseTaxableIncome :: Money a
  -- TODO other fields
  }

newSpouseDetails :: (Num a) => SpouseDetails a
newSpouseDetails = SpouseDetails mempty

spouseTaxableIncome :: Lens' (SpouseDetails a) (Money a)
spouseTaxableIncome =
  lens _spouseTaxableIncome (\s b -> s { _spouseTaxableIncome = b })

data IncomeTests a = IncomeTests
  { _govBenefit :: Money a
  , _targetForeignIncome :: Money a
  , _childSupportPaid :: Money a
  , _dependents :: Integer
  }

newIncomeTests :: (Num a) => IncomeTests a
newIncomeTests = IncomeTests mempty mempty mempty 0

taxFreeGovernmentPensionsOrBenefits :: Lens' (IncomeTests a) (Money a)
taxFreeGovernmentPensionsOrBenefits =
  lens _govBenefit (\s b -> s { _govBenefit = b })

targetForeignIncome :: Lens' (IncomeTests a) (Money a)
targetForeignIncome =
  lens _targetForeignIncome (\s b -> s { _targetForeignIncome = b })

childSupportPaid :: Lens' (IncomeTests a) (Money a)
childSupportPaid =
  lens _childSupportPaid (\s b -> s { _childSupportPaid = b })

dependentChildren :: Lens' (IncomeTests a) Integer
dependentChildren =
  lens _dependents (\s b -> s { _dependents = b })


-- | Individual tax return information.
--
-- Use 'newTaxReturnInfo' to construct.  Alternatively,
-- 'newTaxReturnInfoForTables' can be used to coerce the type
-- parameters to be the same as some 'TaxTables'.
--
-- The following lenses are available:
--
-- +---------------------------------------+----------------------------------+
-- | 'mlsExemption'                        | Medicare levy exemption          |
-- +---------------------------------------+----------------------------------+
-- | 'helpBalance'                         | HELP, VSL, SSL, ABSTUDY SSL,     |
-- |                                       | and TSL account balance          |
-- +---------------------------------------+----------------------------------+
-- | 'sfssBalance'                         | SFSS account balance             |
-- +---------------------------------------+----------------------------------+
-- | 'paymentSummaries'                    | PAYG payment summaries           |
-- +---------------------------------------+----------------------------------+
-- | 'interest'                            | Interest income and tax withheld |
-- +---------------------------------------+----------------------------------+
-- | 'dividends'                           | Dividend data                    |
-- +---------------------------------------+----------------------------------+
-- | 'ess'                                 | Employee Share Scheme statement  |
-- +---------------------------------------+----------------------------------+
-- | 'foreignIncome'                       | Foreign income                   |
-- +---------------------------------------+----------------------------------+
-- | 'cgtEvents'                           | Capital gains and losses         |
-- +---------------------------------------+----------------------------------+
-- | 'deductions'                          | Deductions                       |
-- +---------------------------------------+----------------------------------+
-- | 'offsets'                             | Tax offsets                      |
-- +---------------------------------------+----------------------------------+
-- | 'privateHealthInsurancePolicyDetails' | Private health insurance         |
-- |                                       | policy details                   |
-- +---------------------------------------+----------------------------------+
-- | 'spouseDetails'                       | Spouse Details (or @Nothing@)    |
-- +---------------------------------------+----------------------------------+
-- | 'incomeTests'                         | Income Tests                     |
-- +---------------------------------------+----------------------------------+
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
  , _deductions :: Deductions a
  , _offsets :: Offsets a
  , _triCapitalLossCarryForward :: Money a
  , _phi :: [PrivateHealthInsurancePolicyDetail a]
  , _spouseDetails :: Maybe (SpouseDetails a)
  , _incomeTests :: IncomeTests a
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
  :: (FinancialYear y, Num a)
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
  mempty -- private health insurance policy details
  Nothing -- spouse details
  newIncomeTests

-- | Construct a 'TaxReturnInfo' per 'newTaxReturnInfo',
-- coercing the type parameters to match the 'TaxTables'
-- argument (which is ignored).
--
newTaxReturnInfoForTables
  :: (FinancialYear y, Num a)
  => TaxTables y a -> TaxReturnInfo y a
newTaxReturnInfoForTables _ = newTaxReturnInfo

instance HasCapitalLossCarryForward (TaxReturnInfo y) a where
  capitalLossCarryForward = lens _triCapitalLossCarryForward
      (\s b -> s { _triCapitalLossCarryForward = b })

-- | HELP, VSL, SSL, ABSTUDY SSL, and TSL account balance.
helpBalance :: Lens' (TaxReturnInfo y a) (Money a)
helpBalance = lens _helpBalance (\s b -> s { _helpBalance = b })

-- | SFSS account balance.  From 1 July 2019, all study and training
-- loans are covered by one set of threshold and rates.  Since then,
-- you can specify your entire study and training loan balance via
-- 'helpBalance'.  But it will still calculate correctly if you specify
-- your SFSS balance separately.
--
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

deductions :: Lens' (TaxReturnInfo y a) (Deductions a)
deductions = lens _deductions (\s b -> s { _deductions = b })

offsets :: Lens' (TaxReturnInfo y a) (Offsets a)
offsets = lens _offsets (\s b -> s { _offsets = b })

privateHealthInsurancePolicyDetails
  :: Lens' (TaxReturnInfo y a) [PrivateHealthInsurancePolicyDetail a]
privateHealthInsurancePolicyDetails = lens _phi (\s b -> s { _phi = b })

spouseDetails :: Lens' (TaxReturnInfo y a) (Maybe (SpouseDetails a))
spouseDetails = lens _spouseDetails (\s b -> s { _spouseDetails = b })

incomeTests :: Lens' (TaxReturnInfo y a) (IncomeTests a)
incomeTests = lens _incomeTests (\s b -> s { _incomeTests = b })


-- | A tax assessment.  Use 'assessTax' to compute a
-- @TaxAssessment@.
data TaxAssessment a = TaxAssessment
  { _taxableIncome :: Money a
  , _taxDue :: Money a
  , _medicareLevyDue :: Money a
  , _taxWithheld :: Money a
  , _taxCreditsAndOffsets :: Money a
  , _taCGTAssessment :: CGTAssessment a
  , _phiAdj :: Money a
  , _studyAndTrainingLoanRepayment :: Money a
  , _paygInstalmentsCredit :: Money a
  }

instance HasTaxableIncome TaxAssessment a a where
  taxableIncome = to _taxableIncome

instance HasTaxWithheld TaxAssessment a a where
  taxWithheld = to _taxWithheld

taxDue :: Getter (TaxAssessment a) (Money a)
taxDue = to _taxDue

medicareLevyDue :: Getter (TaxAssessment a) (Money a)
medicareLevyDue = to _medicareLevyDue

taxCreditsAndOffsets :: Getter (TaxAssessment a) (Money a)
taxCreditsAndOffsets = to _taxCreditsAndOffsets

taxCGTAssessment :: Lens' (TaxAssessment a) (CGTAssessment a)
taxCGTAssessment = lens _taCGTAssessment (\s b -> s { _taCGTAssessment = b })

studyAndTrainingLoanRepayment :: Lens' (TaxAssessment a) (Money a)
studyAndTrainingLoanRepayment =
  lens _studyAndTrainingLoanRepayment (\s b -> s { _studyAndTrainingLoanRepayment = b })

privateHealthInsuranceRebateAdjustment :: Lens' (TaxAssessment a) (Money a)
privateHealthInsuranceRebateAdjustment = lens _phiAdj (\s b -> s { _phiAdj = b })

paygInstalmentsCredit :: Lens' (TaxAssessment a) (Money a)
paygInstalmentsCredit =
  lens _paygInstalmentsCredit (\s b -> s { _paygInstalmentsCredit = b })

-- | What is the balance of the assessment?  Positive means a
-- refund (tax withheld exceeds obligation), negative means a bill.
taxBalance :: Num a => Getter (TaxAssessment a) (Money a)
taxBalance = to $ \a ->
  view taxWithheld a
  $-$ view taxDue a
  $-$ view medicareLevyDue a
  $-$ view studyAndTrainingLoanRepayment a
  $-$ view privateHealthInsuranceRebateAdjustment a
  $+$ view taxCreditsAndOffsets a
  $+$ view paygInstalmentsCredit a

instance (Num a, Eq a) => HasCapitalLossCarryForward TaxAssessment a where
  capitalLossCarryForward = taxCGTAssessment . capitalLossCarryForward


-- | Consolidated individual tax rate incorporating
-- HELP and SFSS repayments
-- (if applicable) and automatic offsets (e.g. LITO, LMITO).
individualTax
  :: (Fractional a, Ord a)
  => TaxTables y a
  -> Tax (Money a) (Money a)
individualTax table =
  greaterOf mempty (ttIndividualIncomeTax table <> ttAdditional table)

-- | Tax to calculate compulsory study and training loan repayments
-- (e.g. HELP, SFSS)
studyAndTrainingLoanRepaymentTax
  :: (Fractional a, Ord a)
  => TaxTables y a
  -> TaxReturnInfo y a
  -> Tax (Money a) (Money a)
studyAndTrainingLoanRepaymentTax table info =
  limit (view helpBalance info) (ttHelp table)
  <> limit (view sfssBalance info) (ttSfss table)

-- | Medicare levy + surcharge
medicareLevyTax
  :: (FinancialYear y, Fractional a)
  => TaxTables y a
  -> TaxReturnInfo y a
  -> Tax (Money a) (Money a)    -- grand unified individual income tax
medicareLevyTax table info =
  let
    ml = ttMedicareLevy table
    mls = ttMedicareLevySurcharge table
    mlsFrac = 1 - getFraction (view mlsExemption info)
  in
    -- TODO medicare levy exemption
    ml
    -- FIXME income for MLS purposes includes
    -- fringe benefits; family thresholds apply
    <> fmap ($* mlsFrac) mls

-- | Taxable income
instance (RealFrac a) => HasTaxableIncome (TaxReturnInfo y) a a where
  taxableIncome = to $ \info ->
    let
      cf = view capitalLossCarryForward info
      gross = foldMap wholeDollars
        [ view (paymentSummaries . taxableIncome) info
        , view (interest . taxableIncome) info
        , view (dividends . taxableIncome) info
        , view (ess . taxableIncome) info
        , view (cgtEvents . to (assessCGTEvents cf) . cgtNetGain) info
        , view foreignIncome info
        ]
    in
      wholeDollars (gross $-$ views deductions totalDeductions info)

-- | Includes PAYG withholding by employer or bank.
-- Does not include franking credits.
instance (Num a) => HasTaxWithheld (TaxReturnInfo y) a a where
  taxWithheld = to $ \info ->
    view (paymentSummaries . taxWithheld) info
    <> view (interest . taxWithheld) info

-- | Assess a tax return, given tax tables and tax return info.
assessTax
  :: (FinancialYear y, RealFrac a)
  => TaxTables y a -> TaxReturnInfo y a -> TaxAssessment a
assessTax tables info =
  let
    cg = assessCGTEvents
          (view capitalLossCarryForward info) (view cgtEvents info)
    taxable = view taxableIncome info
    due = getTax (individualTax tables) taxable
    studyRepayment = getTax (studyAndTrainingLoanRepaymentTax tables info) taxable
    mlAndMLS = getTax (medicareLevyTax tables info) taxable

    incomeForSurchargePurposes =
      taxable
      -- TODO reportable fringe benefits
      -- TODO net investment losses
      <> foldOf (paymentSummaries . traverse . to reportableEmployerSuperannuationContributions) info

    spouseIncomeForSurchargePurposes =
      fmap (view spouseTaxableIncome) (view spouseDetails info)

    phiAdj = assessExcessPrivateHealthRebate
      incomeForSurchargePurposes
      spouseIncomeForSurchargePurposes
      (view (incomeTests . dependentChildren) info)
      (ttPHIRebateRates tables)
      (view privateHealthInsurancePolicyDetails info)

    foreignIncomeTaxOffsetLimit =
      let
        step1 = due <> mlAndMLS
        step2 =
          let
            info' = info & set foreignIncome mempty
            taxable' =
              view taxableIncome info'
              <> view (deductions . foreignIncomeDeductions) info'
            due' = getTax (individualTax tables) taxable'
            mlAndMLS' = getTax (medicareLevyTax tables info') taxable'
          in
            due' <> mlAndMLS'
        step3 = step1 $-$ step2
      in
        max (Money 1000) step3

    frankingCredit = wholeDollars $ view (dividends . taxWithheld) info
    off =
      view (offsets . spouseContributionOffset) info
      <> min (view (offsets . foreignTaxOffset) info) foreignIncomeTaxOffsetLimit

  in
    TaxAssessment
      taxable
      due
      mlAndMLS
      (view taxWithheld info)
      (frankingCredit <> off)
      cg
      phiAdj
      studyRepayment
      (view (offsets . paygInstalments) info)

-- | Australian Business Number
type ABN = String

-- | PAYG payment summary - individual non-business
data PaymentSummary a = PaymentSummary
  { summaryABN :: ABN
  , summaryGross :: Money a
  , summaryWithheld :: Money a
  , reportableEmployerSuperannuationContributions :: Money a
  }

-- | Gross income
instance HasTaxableIncome PaymentSummary a a where
  taxableIncome = to summaryGross

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
  , dividendDate :: Day
  , dividendGrossAndWithheld :: GrossAndWithheld a
  }

-- | Rounds to whole cents
instance (RealFrac a) => HasTaxWithheld Dividend a a where
  taxWithheld = to dividendGrossAndWithheld . taxWithheld . to roundCents

-- | Rounds to whole cents
instance (RealFrac a) => HasTaxableIncome Dividend a a where
  taxableIncome = to dividendGrossAndWithheld . taxableIncome . to roundCents

-- | Construct a dividend from a net payment, with a proportion
-- of the dividend franked at the given corporate tax rate (must
-- be a flat rate).
--
-- Does not perform rounding.
--
-- For franking at the standard corporate tax rate of 30%, you can
-- use the convenience function 'dividendFromNetFranked30'.
--
dividendFromNetFranked
  :: (Fractional a)
  => String         -- ^ Source name (e.g. ticker)
  -> Day            -- ^ Dividend date
  -> Money a        -- ^ Net payment
  -> Proportion a   -- ^ Franked proportion
  -> Tax (Money a) (Money a)  -- ^ Corporate tax rate (must be a flat rate)
  -> Dividend a
dividendFromNetFranked src date net franked rate =
  dividendFromGross src date gross withheld
  where
    Money r = getTax rate (Money 1)  -- extract flat tax rate
    withheld = net $* ( getProportion franked * r / (1 - r) )
    gross = net <> withheld

-- | Construct a dividend from a net payment, with a proportion
-- of the dividend franked at the 30% corporate tax rate.
--
-- Does not perform rounding.
--
-- For franking at a tax rate other than 30%, use 'dividendFromNetFranked'.
--
dividendFromNetFranked30
  :: (Fractional a)
  => String         -- ^ Source name (e.g. ticker)
  -> Day            -- ^ Dividend date
  -> Money a        -- ^ Net payment
  -> Proportion a   -- ^ Franked proportion
  -> Dividend a
dividendFromNetFranked30 src date net franked =
  dividendFromNetFranked src date net franked corporateTax

-- | Construct a dividend from a net payment, with explicit
-- declaration of tax withheld.
--
-- Does not perform rounding.
--
dividendFromNet
  :: (Num a)
  => String         -- ^ Source name (e.g. ticker)
  -> Day            -- ^ Dividend date
  -> Money a        -- ^ Net payment
  -> Money a        -- ^ Tax withheld
  -> Dividend a
dividendFromNet src date net withheld =
  dividendFromGross src date (net <> withheld) withheld

-- | Construct a dividend from a gross payment, with explicit
-- declaration of tax withheld.
--
-- Does not perform rounding.
--
dividendFromGross
  :: String         -- ^ Source name (e.g. ticker)
  -> Day            -- ^ Dividend date
  -> Money a        -- ^ Gross payment
  -> Money a        -- ^ Tax withheld
  -> Dividend a
dividendFromGross src date gross withheld =
  Dividend src date (GrossAndWithheld gross withheld)

-- | Tax offsets that individuals can claim
--
-- The following lenses are available:
--
-- +---------------------------------------+----------------------------------+
-- | 'spouseContributionOffset'            | Spouse super contribution        |
-- +---------------------------------------+----------------------------------+
-- | 'foreignTaxOffset'                    | Foreign income tax offset        |
-- +---------------------------------------+----------------------------------+
-- | 'paygInstalments'                     | PAYG Instalments                 |
-- +---------------------------------------+----------------------------------+
--
data Offsets a = Offsets
  { _spouseOffset :: Money a
  , _foreignTaxOffset :: Money a
  , _paygInstalments :: Money a
  }

instance Num a => Semigroup (Offsets a) where
  Offsets a b c <> Offsets a' b' c' = Offsets (a <> a') (b <> b') (c <> c')

instance Num a => Monoid (Offsets a) where
  mempty = Offsets mempty mempty mempty
  mappend = (<>)

-- | Spouse contribution offset.  Maximum of /$540/ (not enforced).
spouseContributionOffset :: Lens' (Offsets a) (Money a)
spouseContributionOffset = lens _spouseOffset (\s b -> s { _spouseOffset = b })

-- | Offset for tax paid on foreign income.
foreignTaxOffset :: Lens' (Offsets a) (Money a)
foreignTaxOffset = lens _foreignTaxOffset (\s b -> s { _foreignTaxOffset = b })

paygInstalments :: Lens' (Offsets a) (Money a)
paygInstalments = lens _paygInstalments (\s b -> s { _paygInstalments = b })


-- | Deductions that individuals can claim.
--
-- +------------------------------------------------------------------------+----------------------------------------------------------------------------------------+
-- | 'workRelatedCarExpenses'                                               | __D1__ Work-related car expenses                                                       |
-- +------------------------------------------------------------------------+----------------------------------------------------------------------------------------+
-- | 'workRelatedTravelExpenses'                                            | __D2__ Work-related travel expenses                                                    |
-- +------------------------------------------------------------------------+----------------------------------------------------------------------------------------+
-- | 'workRelatedClothingLaundryAndDryCleaningExpenses'                     | __D3__ Work-related clothing, laundry and dry-cleaning expenses                        |
-- +------------------------------------------------------------------------+----------------------------------------------------------------------------------------+
-- | 'workRelatedSelfEducationExpenses'                                     | __D4__ Work-related self-education expenses                                            |
-- +------------------------------------------------------------------------+----------------------------------------------------------------------------------------+
-- | 'otherWorkRelatedExpenses'                                             | __D5__ Other work-related expenses                                                     |
-- +------------------------------------------------------------------------+----------------------------------------------------------------------------------------+
-- | 'lowValuePoolDeduction'                                                | __D6__ Low-value pool deduction                                                        |
-- +------------------------------------------------------------------------+----------------------------------------------------------------------------------------+
-- | 'interestDeductions'                                                   | __D7__ Interest deductions                                                             |
-- +------------------------------------------------------------------------+----------------------------------------------------------------------------------------+
-- | 'dividendDeductions'                                                   | __D8__ Dividend deductions                                                             |
-- +------------------------------------------------------------------------+----------------------------------------------------------------------------------------+
-- | 'giftsOrDonations'                                                     | __D9__ Gifts or donations                                                              |
-- +------------------------------------------------------------------------+----------------------------------------------------------------------------------------+
-- | 'costOfManagingTaxAffairs'                                             | __D10__ Cost of managing tax affairs                                                   |
-- +------------------------------------------------------------------------+----------------------------------------------------------------------------------------+
-- | 'deductibleAmountOfUndeductedPurchasePriceOfAForeignPensionOrAnnuity'  | __D11__ Deductible amount of undeducted purchase price of a foreign pension or annuity |
-- +------------------------------------------------------------------------+----------------------------------------------------------------------------------------+
-- | 'personalSuperannuationContributions'                                  | __D12__ Personal superannuation contributions                                          |
-- +------------------------------------------------------------------------+----------------------------------------------------------------------------------------+
-- | 'deductionForProjectPool'                                              | __D13__ Deduction for project pool                                                     |
-- +------------------------------------------------------------------------+----------------------------------------------------------------------------------------+
-- | 'forestryManagedInvestmentSchemeDeduction'                             | __D14__ Forestry managed investment scheme deduction                                   |
-- +------------------------------------------------------------------------+----------------------------------------------------------------------------------------+
-- | 'otherDeductions'                                                      | __D15__ Other deductions — not claimable at __D1__ to __D14__ or elsewhere in your tax |
-- |                                                                        | return                                                                                 |
-- +------------------------------------------------------------------------+----------------------------------------------------------------------------------------+
-- | 'foreignIncomeDeductions'                                              | Aggregate of deductions related to foreign income.  The components making up this      |
-- |                                                                        | amount __must be included in the other fields__.  This field is only used in           |
-- |                                                                        | calculating the Foreign Income Tax Offset Limit.                                       |
-- +------------------------------------------------------------------------+----------------------------------------------------------------------------------------+
--
data Deductions a = Deductions
  { _workRelatedCarExpenses :: Money a
  , _workRelatedTravelExpenses :: Money a
  , _workRelatedClothingLaundryAndDryCleaningExpenses :: Money a
  , _workRelatedSelfEducationExpenses :: Money a
  , _otherWorkRelatedExpenses :: Money a
  , _lowValuePoolDeduction :: Money a
  , _interestDeductions :: Money a
  , _dividendDeductions :: Money a
  , _giftsOrDonations :: Money a
  , _costOfManagingTaxAffairs :: Money a
  , _deductibleAmountOfUndeductedPurchasePriceOfAForeignPensionOrAnnuity :: Money a
  , _personalSuperannuationContributions :: Money a
  , _deductionForProjectPool :: Money a
  , _forestryManagedInvestmentSchemeDeduction :: Money a
  , _otherDeductions :: Money a
  , _foreignIncomeDeductions :: Money a
  }

instance Num a => Semigroup (Deductions a) where
  Deductions a b c d e f g h i j k l m n o p
    <> Deductions a' b' c' d' e' f' g' h' i' j' k' l' m' n' o' p'
      = Deductions (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g') (h <> h')
                   (i <> i') (j <> j') (k <> k') (l <> l') (m <> m') (n <> n') (o <> o') (p <> p')

instance Num a => Monoid (Deductions a) where
  mempty = Deductions mempty mempty mempty mempty mempty mempty mempty mempty
                      mempty mempty mempty mempty mempty mempty mempty mempty

-- | Sum the deductions.  Negative components are ignored.
totalDeductions :: (Num a, Ord a) => Deductions a -> Money a
totalDeductions a =
  foldMap (max mempty)
    [ view workRelatedCarExpenses a
    , view workRelatedTravelExpenses a
    , view workRelatedClothingLaundryAndDryCleaningExpenses a
    , view workRelatedSelfEducationExpenses a
    , view otherWorkRelatedExpenses a
    , view lowValuePoolDeduction a
    , view interestDeductions a
    , view dividendDeductions a
    , view giftsOrDonations a
    , view costOfManagingTaxAffairs a
    , view deductibleAmountOfUndeductedPurchasePriceOfAForeignPensionOrAnnuity a
    , view personalSuperannuationContributions a
    , view deductionForProjectPool a
    , view forestryManagedInvestmentSchemeDeduction a
    , view otherDeductions a
    ]

-- | __D1__ Work-related car expenses
workRelatedCarExpenses :: Lens' (Deductions a) (Money a)
workRelatedCarExpenses =
  lens _workRelatedCarExpenses (\s b -> s { _workRelatedCarExpenses = b })

-- | __D2__ Work-related travel expenses
workRelatedTravelExpenses :: Lens' (Deductions a) (Money a)
workRelatedTravelExpenses =
  lens _workRelatedTravelExpenses (\s b -> s { _workRelatedTravelExpenses = b })

-- | __D3__ Work-related clothing, laundry and dry-cleaning expenses
workRelatedClothingLaundryAndDryCleaningExpenses :: Lens' (Deductions a) (Money a)
workRelatedClothingLaundryAndDryCleaningExpenses =
  lens
    _workRelatedClothingLaundryAndDryCleaningExpenses
    (\s b -> s { _workRelatedClothingLaundryAndDryCleaningExpenses = b })

-- | __D4__ Work-related self-education expenses
workRelatedSelfEducationExpenses :: Lens' (Deductions a) (Money a)
workRelatedSelfEducationExpenses =
  lens _workRelatedSelfEducationExpenses (\s b -> s { _workRelatedSelfEducationExpenses = b })

-- | __D5__ Other work-related expenses
otherWorkRelatedExpenses :: Lens' (Deductions a) (Money a)
otherWorkRelatedExpenses =
  lens _otherWorkRelatedExpenses (\s b -> s { _otherWorkRelatedExpenses = b })

-- | __D6__ Low-value pool deduction
lowValuePoolDeduction :: Lens' (Deductions a) (Money a)
lowValuePoolDeduction =
  lens _lowValuePoolDeduction (\s b -> s { _lowValuePoolDeduction = b })

-- | __D7__ Interest deductions
interestDeductions :: Lens' (Deductions a) (Money a)
interestDeductions =
  lens _interestDeductions (\s b -> s { _interestDeductions = b })

-- | __D8__ Dividend deductions
dividendDeductions :: Lens' (Deductions a) (Money a)
dividendDeductions =
  lens _dividendDeductions (\s b -> s { _dividendDeductions = b })

-- | __D9__ Gifts or donations
giftsOrDonations :: Lens' (Deductions a) (Money a)
giftsOrDonations =
  lens _giftsOrDonations (\s b -> s { _giftsOrDonations = b })

-- | __D10__ Cost of managing tax affairs
costOfManagingTaxAffairs :: Lens' (Deductions a) (Money a)
costOfManagingTaxAffairs =
  lens _costOfManagingTaxAffairs (\s b -> s { _costOfManagingTaxAffairs = b })

-- | __D11__ Deductible amount of undeducted purchase price of a foreign pension or annuity
deductibleAmountOfUndeductedPurchasePriceOfAForeignPensionOrAnnuity :: Lens' (Deductions a) (Money a)
deductibleAmountOfUndeductedPurchasePriceOfAForeignPensionOrAnnuity =
  lens
    _deductibleAmountOfUndeductedPurchasePriceOfAForeignPensionOrAnnuity 
    (\s b -> s { _deductibleAmountOfUndeductedPurchasePriceOfAForeignPensionOrAnnuity = b })

-- | __D12__ Personal superannuation contributions
personalSuperannuationContributions :: Lens' (Deductions a) (Money a)
personalSuperannuationContributions =
  lens _personalSuperannuationContributions (\s b -> s { _personalSuperannuationContributions = b })

-- | __D13__ Deduction for project pool
deductionForProjectPool :: Lens' (Deductions a) (Money a)
deductionForProjectPool =
  lens _deductionForProjectPool (\s b -> s { _deductionForProjectPool = b })

-- | __D14__ Forestry managed investment scheme deduction
forestryManagedInvestmentSchemeDeduction :: Lens' (Deductions a) (Money a)
forestryManagedInvestmentSchemeDeduction =
  lens
    _forestryManagedInvestmentSchemeDeduction
    (\s b -> s { _forestryManagedInvestmentSchemeDeduction = b })

-- | __D15__ Other deductions — not claimable at __D1__ to __D14__ or elsewhere
-- in your tax return
otherDeductions :: Lens' (Deductions a) (Money a)
otherDeductions =
  lens _otherDeductions (\s b -> s { _otherDeductions = b })

-- | Aggregate of deductions related to foreign income, including:
--
-- * Deductions that are reasonably related to amounts on which foreign
--   income tax has been paid
-- * Debt deductions attributable to your overseas permanent establishment
-- * Amount of the foreign loss component of one or more tax losses deducted
--   in the income year.
--
-- The components making up this amount __must be included in other fields__.
-- This field is only used in calculating the Foreign Income Tax Offset Limit.
--
foreignIncomeDeductions :: Lens' (Deductions a) (Money a)
foreignIncomeDeductions =
  lens _foreignIncomeDeductions (\s b -> s { _foreignIncomeDeductions = b })


-- | A gross income (first argument) and amount of tax withheld (second argument).
-- The whole gross amount is considered taxable income.
--
data GrossAndWithheld a = GrossAndWithheld (Money a) (Money a)

instance (Num a) => Semigroup (GrossAndWithheld a) where
  GrossAndWithheld a b <> GrossAndWithheld a' b' =
    GrossAndWithheld (a <> a') (b <> b')

instance (Num a) => Monoid (GrossAndWithheld a) where
  mempty = GrossAndWithheld mempty mempty
  mappend = (<>)

instance HasTaxableIncome GrossAndWithheld a a where
  taxableIncome = to $ \(GrossAndWithheld a _) -> a

instance HasTaxWithheld GrossAndWithheld a a where
  taxWithheld = to $ \(GrossAndWithheld _ a) -> a


-- | Employee share scheme statement.  Use 'newESSStatement' to construct.
-- The following lenses are available:
--
-- +------------------------------+--------------------------------------------+
-- | 'essTaxedUpfrontReduction'   | __D__ Discount from taxed up front         |
-- |                              | schemes—eligible for reduction             |
-- +------------------------------+--------------------------------------------+
-- | 'essTaxedUpfrontNoReduction' | __E__ Discount from taxed up front         |
-- |                              | schemes—not eligible for reduction         |
-- +------------------------------+--------------------------------------------+
-- | 'essDeferral'                | __F__ Discount from taxed deferral schemes |
-- +------------------------------+--------------------------------------------+
-- | 'essPre2009'                 | __G__ Discounts on ESS interests acquired  |
-- |                              | pre 1 July 2009 and "cessation time"       |
-- |                              | occurred during the financial year.        |
-- +------------------------------+--------------------------------------------+
-- | 'essTFNAmounts'              | __C__ TFN amounts withheld from discounts  |
-- +------------------------------+--------------------------------------------+
-- | 'essForeignSourceDiscounts'  | __A__ ESS foreign source discounts         |
-- +------------------------------+--------------------------------------------+
--
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
instance (Num a) => HasTaxableIncome ESSStatement a a where
  taxableIncome = to $ \s ->
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
