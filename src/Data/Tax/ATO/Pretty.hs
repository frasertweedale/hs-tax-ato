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
  , summariseCGTSchedule
  , summariseAssessment
  , formatMoney
  ) where

import Data.List.NonEmpty as NE (NonEmpty, groupAllWith, head)

import Control.Lens (ALens', _2, at, cloneLens, foldOf, view, views)
import qualified Text.PrettyPrint as P

import Data.Tax.ATO
import Data.Tax.ATO.Common (EntityType(..))
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

omitIfZero :: ((a, Money Rational) -> P.Doc) -> (a, Money Rational) -> P.Doc
omitIfZero f rec@(_,x)
  | x == mempty = P.empty
  | otherwise   = f rec

data Formatter a = Formatter Int (a -> P.Doc)

prepend :: String -> Formatter a -> Formatter a
prepend s (Formatter w f) = Formatter (w + length s) ((P.text s <>) . f)

append :: String -> Formatter a -> Formatter a
append s (Formatter w f) = Formatter (w + length s) ((<> P.text s) . f)

moneyFormatter :: Formatter (Money Rational)
moneyFormatter = Formatter colWidthMoney formatMoney

-- | Make a formatter that has a same width as the given formatter,
-- but outputs blank space.
blank :: Formatter a -> Formatter a
blank (Formatter w _) = Formatter w (\_ -> P.text $ replicate w ' ')


twoCol :: (P.Doc, Money Rational) -> P.Doc
twoCol = twoCol' id moneyFormatter

twoCol' :: (a -> P.Doc) -> Formatter b -> (a, b) -> P.Doc
twoCol' fa (Formatter wb fb) (a, b) =
  fa a
  P.$$ P.nest (80 - wb) (fb b)

threeCol :: (P.Doc, Money Rational, Money Rational) -> P.Doc
threeCol = threeCol' id moneyFormatter moneyFormatter

threeCol' :: (a -> P.Doc) -> Formatter b -> Formatter c -> (a, b, c) -> P.Doc
threeCol' fa (Formatter wb fb) (Formatter wc fc) (a, b, c) =
  fa a
  P.$$ P.nest (80 - wb - wc) (fb b)
  P.<> fc c

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
    [ ( "  1   Salary or wages"
      , view (paymentSummariesIndividualNonBusiness . taxWithheld) info
      , view (paymentSummariesIndividualNonBusiness . taxableIncome) info
      )
    , ("  10  Interest"         , view (interest . taxWithheld) info, view (interest . taxableIncome) info)
    ]
  P.$+$ "  11  Dividends"
  P.$+$ views dividends summariseDividends info
  P.$+$ views ess summariseESS info
  P.$+$ summariseCGT info
  P.$+$ vcatWith twoCol
    [ ("  20M Other net foreign source income" , view foreignIncome info)
    ]
  P.$+$ views otherIncome summariseOtherIncome info
  P.$+$ views businessAndProfessionalItems summariseBPI info
  P.$+$ "Deductions"
  P.$+$ P.vcat (uncurry (summariseDeduction (view deductions info)) <$> deductionsTable)
  P.$+$ "Tax offsets"
  P.$+$ vcatWith threeColLeft
    [ ("  20O Foreign income tax offset"  , view (offsets . foreignTaxOffset) info)
    ]

summariseDividends :: [Dividend Rational] -> P.Doc
summariseDividends =
  vcatWith (threeCol . prep)
  . groupAllWith dividendSource
  where
    prep :: NonEmpty (Dividend Rational) -> (P.Doc, Money Rational, Money Rational)
    prep l =
      ( P.text ("        " <> dividendSource (NE.head l))
      , view taxWithheld l
      , view taxableIncome l
      )

summariseESS :: [ESSStatement Rational] -> P.Doc
summariseESS [] = P.empty
summariseESS l =
  "  12  Employee share schemes"
  P.$+$ P.vcat
    [ omitIfZero twoCol
        ( "    D Discount from upfront schemes - eligible for reduction"
        , foldOf (traverse . essTaxedUpfrontReduction) l )
    , omitIfZero twoCol
        ( "    E Discount from upfront schemes - ineligible for reduction"
        , foldOf (traverse . essTaxedUpfrontNoReduction) l )
    , omitIfZero twoCol
        ( "    F Discount from deferral schemes"
        , foldOf (traverse . essDeferral) l )
    , omitIfZero threeColLeft
        ( "    C TFN amounts withheld from discounts"
        , foldOf (traverse . essTFNAmounts) l )
    , omitIfZero twoCol
        ( "    A Foreign source discounts"
        , foldOf (traverse . essForeignSourceDiscounts) l )
    ]

summariseOtherIncome :: OtherIncome Rational -> P.Doc
summariseOtherIncome oi | view taxableIncome oi == Money (0 :: Rational)
                        = P.empty
summariseOtherIncome oi =
  P.vcat
    [ "  24  Other income"
    , case view otherIncomeCategory1 oi of
        [] -> P.empty
        [(s, x)] -> twoCol ("    Y - Category 1 - " <> P.text s, x)
        l -> twoCol ("    Y - Category 1 - <multiple>", foldOf (traverse . _2) l)
    , case view otherIncomeCategory2 oi of
        [] -> P.empty
        [(s, x)] -> twoCol ("    X - Category 2 (ATO interest) - " <> P.text s, x)
        l -> twoCol ("    X - Category 2 (ATO interest) - <multiple>", foldOf (traverse . _2) l)
    , omitIfZero twoCol ("    R - Category 3 (FHSS)", view otherIncomeCategory3 oi)
    , case view otherIncomeCategory4 oi of
        [] -> P.empty
        [(s, x)] -> twoCol ("    V - Category 4 - " <> P.text s, x)
        l -> twoCol ("    V - Category 4 - <multiple>", foldOf (traverse . _2) l)
    , omitIfZero threeColLeft
        ("    E Tax withheld - LSPIA", view taxWithheldLumpSumPaymentsInArrears oi)
    , omitIfZero threeColLeft
        ("    Z Taxable professional income", view taxableProfessionalIncome oi)
    , omitIfZero threeColLeft
        ("    S Tax withheld - FHSS released amount", view taxWithheldAssessableFHSSReleasedAmount oi)
    ]

summariseBPI :: BusinessAndProfessionalItemsSchedule Rational -> P.Doc
summariseBPI bpi =
  views personalServicesIncome summarisePSI bpi

summarisePSI :: PersonalServicesIncome Rational -> P.Doc
summarisePSI psi | netPSI psi == mempty
                 = P.empty
summarisePSI psi =
  "  P1  Personal services income"
  P.$+$ vcatWith (omitIfZero twoCol)
    [ ("    M PSI - voluntary agreement", view psiVoluntaryAgreement psi)
    , ("    N PSI - where Australian business number not quoted", view psiWhereABNNotQuoted psi)
    , ("    O PSI - labour hire or other specified payments", view psiLabourHireOrOtherSpecifiedPayments psi)
    , ("    J PSI - other", view psiOther psi)
    , ("    K Deductions for payments to associates for principal work", view psiDeductionsForPaymentsToAssociatesForPrincipalWork psi)
    , ("    L Total amount of other deductions against PSI", view psiTotalAmountOfOtherDeductions psi)
    ]
  P.$+$ twoCol ("    Net PSI", netPSI psi)

summariseCGTSchedule :: CGTAssessment Rational -> P.Doc
summariseCGTSchedule o = P.vcat
  [ "1  Current year capital gains and capital losses"
  , let (g,l) = view (cgtGainsAndLossesByCategory . at CGTScheduleCategorySharesAUListed . traverse) o
    in print3 "A" "K" ("  Australian listed shares", g, l)
  , let (g,l) = view (cgtGainsAndLossesByCategory . at CGTScheduleCategorySharesOther . traverse) o
    in print3 "B" "L" ("  Other shares", g, l)
  , let (g,l) = view (cgtGainsAndLossesByCategory . at CGTScheduleCategoryUnitsAUListed . traverse) o
    in print3 "C" "M" ("  Australian listed units", g, l)
  , let (g,l) = view (cgtGainsAndLossesByCategory . at CGTScheduleCategoryUnitsOther . traverse) o
    in print3 "D" "N" ("  Other units", g, l)
  , let (g,l) = view (cgtGainsAndLossesByCategory . at CGTScheduleCategoryRealEstateAU . traverse) o
    in print3 "E" "O" ("  Australian real estate", g, l)
  , let (g,l) = view (cgtGainsAndLossesByCategory . at CGTScheduleCategoryRealEstateOther . traverse) o
    in print3 "F" "P" ("  Other real estate", g, l)
  , let (g,_) = view (cgtGainsAndLossesByCategory . at CGTScheduleCategoryTrust . traverse) o
    in print2_ "G" ("  Capital gains from trusts", g)
  , let (g,l) = view (cgtGainsAndLossesByCategory . at CGTScheduleCategoryCollectable . traverse) o
    in print3 "H" "Q" ("  Collectables", g, l)
  , let (g,l) = view (cgtGainsAndLossesByCategory . at CGTScheduleCategoryOther . traverse) o
    in print3 "I" "R" ("  Other CGT assets and CGT events", g, l)
  , let (g,_) = view (cgtGainsAndLossesByCategory . at CGTScheduleCategoryOther . traverse) o
    in print2_ "S" ("  Previously deferred CGT relief", g)
  , print2_ "J" ("  Total current year capital gains",  view cgtTotalCurrentYearGains o)

  , P.empty
  , "2  Capital losses"
  , print2 "A" ("  Total current year capital losses",  view cgtTotalCurrentYearLosses o)
  , print2 "B" ("  Total current year capital losses applied",   view cgtTotalCurrentYearLossesApplied o)
  , print2 "C" ("  Total prior year net capital losses applied", view cgtTotalPriorYearLossesApplied o)
  , let total = view cgtTotalCurrentYearLossesApplied o <> view cgtTotalPriorYearLossesApplied o
    in print2 "E" ("  Total capital losses applied", total)

  , P.empty
  , "3  Unapplied net capital losses carried forward"
  , print2 "A"
      ("  Net capital losses from collectables carried forward"
      , view (capitalLossCarryForward . capitalLossCarryForwardCollectables) o)
  , print2 "B"
      ("  Other capital losses carried forward"
      , view (capitalLossCarryForward . capitalLossCarryForwardOther) o)

  , P.empty
  , "4  CGT discount"
  , print2 "A" ("  Total CGT discount applied", view cgtTotalCGTDiscountApplied o)

  , P.empty
  , "6  Net capital gain"
  , print2 "A" ("  Net capital gain", view cgtNetGain o)
  ]
  where
    prepLabel s = "  " <> s <> " $"
    print3 labelL labelR =
      threeCol' id
        (prepend (prepLabel labelL) moneyFormatter)
        (prepend (prepLabel labelR) moneyFormatter)
    print2_ labelL (k, v) =
      threeCol' id
        (prepend (prepLabel labelL) moneyFormatter)
        (blank $ prepend (prepLabel labelL) moneyFormatter)
        (k, v, mempty)
    print2 label = twoCol' id (prepend (prepLabel label) moneyFormatter)

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
    o = assessCGTEvents Individual (view capitalLossCarryForward info) (view cgtEvents info)

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
  = omitIfZero twoCol (P.nest 2 (P.text desc), view (cloneLens l) a)


summariseAssessment :: TaxAssessment Rational -> P.Doc
summariseAssessment assessment =
  "Your taxable income is $" P.<> formatMoney (view taxableIncome assessment)
  P.$+$ P.text (replicate 80 '-')
  P.$+$ threeColLeft ("Tax on your taxable on net income"          , view taxDue assessment)
  P.$+$ "Less non-refundable tax offsets"
  P.$+$ vcatWith (omitIfZero twoCol)
    [ ("  Offset for super contributions on behalf of your spouse"
      , view offsetForSuperannuationContributionsOnBehalfOfYourSpouse assessment )
    , ("  Foreign income tax offsets"                 , view foreignIncomeTaxOffsets assessment)
    ]
  P.$+$ "Less refundable tax offsets"
  P.$+$ twoCol ("  Franking credit offset"            , view frankingCreditOffset assessment)
  P.$+$ "Plus other liabilities"
  P.$+$ vcatWith (omitIfZero threeColLeft)
    [ ("  Medicare levy"                              , view medicareLevyDue assessment)
    , ("  Medicare levy surcharge"                    , view medicareLevySurchargeDue assessment)
    , ("  Study and training loan repayment"          , view studyAndTrainingLoanRepayment assessment)
    , ("  Excess private health reduction or refund"  , view privateHealthInsuranceRebateAdjustment assessment)
    ]
  P.$+$ "Less Pay as you go (PAYG) credits and other entitlements"
  P.$+$ vcatWith (omitIfZero twoCol)
    [ ("  PAYG instalments"                           , view paygInstalmentsCredit assessment)
    , ("  PAYG withholding"                           , view taxWithheld assessment)
    ]
  P.$+$ P.text (replicate 80 '-')
  P.$+$ "Result of this notice" P.$$ P.nest colWidthLabel (views taxBalance formatMoney assessment)
  P.$+$ "Net capital loss to carry forward" P.$$ P.nest colWidthLabel (views (taxCGTAssessment . cgtNetLossesCarriedForward) formatMoney assessment)
