-- This file is part of hs-tax-ato
-- Copyright (C) 2024  Fraser Tweedale
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

Pretty-print tax data.

Monetary values are rounded to the nearest whole cent (half-up).

-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Tax.ATO.Pretty
  ( summariseTaxReturnInfo
  , summariseAssessment
  ) where

import Data.Function (on)
import Data.List (groupBy, sortOn)

import Control.Lens (ALens', cloneLens, view, views)
import qualified Text.PrettyPrint as P

import Data.Tax.ATO
import Data.Tax.ATO.CGT


colWidthMoney, colWidthLabel :: Int
colWidthMoney = 16
colWidthLabel = 80 - colWidthMoney

-- | Format money for display.  Rounds to the nearest whole cent (half-up).
-- Does not prepend '$'.
formatMoney :: Money Rational -> P.Doc
formatMoney (Money x) =
  P.text (replicate (colWidthMoney - length amount) ' ' <> amount)
  where
    (iPart, fPart) = properFraction (x * 100) :: (Integer, Rational)
    iPart' = if fPart >= 0.5 then iPart + 1 else iPart
    digits = reverse (show iPart')
    cents = case reverse (take 2 digits) of [] -> "00" ; [c] -> c:"0" ; s -> s
    dollars = case drop 2 digits of "" -> "0" ; s -> reverse (putCommas s)
    amount = dollars <> "." <> cents
    putCommas (a:b:c:d:rest) | d /= '-' = a:b:c:',':putCommas (d:rest)
    putCommas rest                      = rest

twoCol :: (P.Doc, Money Rational) -> P.Doc
twoCol (label, value) = label P.$$ P.nest (80 - colWidthMoney) (formatMoney value)

threeCol :: (P.Doc, Money Rational, Money Rational) -> P.Doc
threeCol (label, v1, v2) =
  label
  P.$$ P.nest (80 - 2 * colWidthMoney) (formatMoney v1)
  P.<> formatMoney v2

-- | 3-column layout with rightmost column blank
threeColLeft :: (P.Doc, Money Rational) -> P.Doc
threeColLeft (label, v1) =
  label
  P.$$ P.nest (80 - 2 * colWidthMoney) (formatMoney v1)

vcatWith :: (a -> P.Doc) -> [a] -> P.Doc
vcatWith f = P.vcat . fmap f

summariseTaxReturnInfo :: TaxReturnInfo y Rational -> P.Doc
summariseTaxReturnInfo info =
  "Income"
  P.$+$ vcatWith threeCol
    [ ("  1   Salary or wages"  , view (paymentSummaries . taxWithheld) info, view (paymentSummaries . taxableIncome) info)
    , ("  10  Interest"         , view (interest . taxWithheld) info, view (interest . taxableIncome) info)
    ]
  P.$+$ "  11  Dividends"
  P.$+$ views dividends summariseDividends info
  P.$+$ views ess summariseESS info
  P.$+$ summariseCGT info
  P.$+$ vcatWith twoCol
    [ ("  20M Other net foreign source income" , view foreignIncome info)
    ]
  P.$+$ "Deductions"
  P.$+$ P.vcat (uncurry (summariseDeduction (view deductions info)) <$> deductionsTable)
  P.$+$ "Tax offsets"
  P.$+$ vcatWith threeColLeft
    [ ("  20O Foreign income tax offset"  , view (offsets . foreignTaxOffset) info)
    ]

summariseDividends :: [Dividend Rational] -> P.Doc
summariseDividends =
  vcatWith (threeCol . prep)
  . groupBy ((==) `on` dividendSource)
  . sortOn dividendSource
  where
    prep :: [Dividend Rational] -> (P.Doc, Money Rational, Money Rational)
    prep l =
      ( P.text ("        " <> dividendSource (head l))
      , view taxWithheld l
      , view taxableIncome l
      )

summariseESS :: ESSStatement Rational -> P.Doc
summariseESS s
  | s == newESSStatement = P.empty
  | otherwise =
      "  12  Employee share schemes"
      P.$+$ P.vcat
        [ twoCol ("    D Discount from upfront schemes - eligible for reduction", view essTaxedUpfrontReduction s)
        , twoCol ("    E Discount from upfront schemes - ineligible for reduction", view essTaxedUpfrontNoReduction s)
        , twoCol ("    F Discount from deferral schemes", view essDeferral s)
        , threeColLeft ("    C TFN amounts withheld from discounts", view essTFNAmounts s)
        , twoCol ("    A Foreign source discounts", view essForeignSourceDiscounts s)
        ]

summariseCGT :: TaxReturnInfo y Rational -> P.Doc
summariseCGT info
  | o == nullCGTAssessment = P.empty
  | otherwise =
      "  18  Capital gains"
      P.$+$ P.vcat
        [ twoCol        ("    A Net capital gain",                   view cgtNetGain o)
        , threeColLeft  ("    H Total current year capital gains",   view cgtTotalCurrentYearGains o)
        , threeColLeft  ("    V Net capital losses carried forward", view cgtNetLossesCarriedForward o)
        ]
  where
    o = assessCGTEvents (view capitalLossCarryForward info) (view cgtEvents info)

deductionsTable :: [(ALens' (Deductions Rational) (Money Rational), String)]
deductionsTable =
  [ (workRelatedCarExpenses, "D1  Work-related car expenses")
  , (workRelatedTravelExpenses, "D2  Work-related travel expenses")
  , (workRelatedClothingLaundryAndDryCleaningExpenses, "D3  Work-related clothing, laundry and dry cleaning expenses")
  , (workRelatedSelfEducationExpenses, "D4  Work-related self-education expenses")
  , (otherWorkRelatedExpenses, "D5  Other work-related expenses")
  , (lowValuePoolDeduction, "D6  Low value pool deduction")
  , (interestDeductions, "D7  Interest deductions")
  , (dividendDeductions, "D8  Dividend deductions")
  , (giftsOrDonations, "D9  Gifts or donations")
  , (costOfManagingTaxAffairs, "D10 Cost of managing tax affairs")
  , (deductibleAmountOfUndeductedPurchasePriceOfAForeignPensionOrAnnuity, "D11 Deductible amount of undeducted purchase price of a foreign pension or annuity")
  , (personalSuperannuationContributions, "D12 Personal superannuation contributions")
  , (deductionForProjectPool, "D13 Deduction for project pool")
  , (forestryManagedInvestmentSchemeDeduction, "D14 Forestry managed investment scheme deduction")
  , (otherDeductions, "D15 Other deductions")
  ]

summariseDeduction
  :: Deductions Rational
  -> ALens' (Deductions Rational) (Money Rational)
  -> String
  -> P.Doc
summariseDeduction a l desc
  | amt > mempty
  = P.nest 2 (P.text desc) P.$$ P.nest colWidthLabel (formatMoney amt)
  | otherwise
  = P.empty
  where
    amt = view (cloneLens l) a


summariseAssessment :: TaxAssessment Rational -> P.Doc
summariseAssessment assessment =
  "Your taxable income is $" P.<> formatMoney (view taxableIncome assessment)
  P.$+$ P.text (replicate 80 '-')
  P.$+$ vcatWith twoCol
    (
      [ ("Tax on your taxable income"                 , view taxDue assessment)
      , ("Less credits and offsets"                   , views taxCreditsAndOffsets (fmap negate) assessment)
      , ("Medicare levy (and surcharge, if any)"      , view medicareLevyDue assessment)
      ]
    <> filter ((/= mempty) . snd)
      [ ("Study and training loan repayment"          , view studyAndTrainingLoanRepayment assessment)
      , ("Excess private health reduction or refund"  , view privateHealthInsuranceRebateAdjustment assessment)
      ]
    <>
      [ ("Less PAYG withholding"                      , views taxWithheld (fmap negate) assessment)
      , ("Less PAYG instalments"                      , views paygInstalmentsCredit (fmap negate) assessment)
      ]
    )
  P.$+$ P.text (replicate 80 '-')
  P.$+$ "Result of this notice" P.$$ P.nest colWidthLabel (views taxBalance formatMoney assessment)
  P.$+$ "Net capital loss to carry forward" P.$$ P.nest colWidthLabel (views (taxCGTAssessment . capitalLossCarryForward) formatMoney assessment)
