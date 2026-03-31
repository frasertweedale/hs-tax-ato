-- This file is part of hs-tax-ato
-- Copyright (C) 2026  Fraser Tweedale
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

Business and professional items (BPI) schedule

Only Personal services income (PSI) and deductions are implemented at the
current time.

-}

module Data.Tax.ATO.BPI
  ( BusinessAndProfessionalItemsSchedule
  , newBusinessAndProfessionalItemsSchedule
  , personalServicesIncome

  , PersonalServicesIncome
  , newPersonalServicesIncome
  , psiVoluntaryAgreement
  , psiWhereABNNotQuoted
  , psiLabourHireOrOtherSpecifiedPayments
  , psiOther
  , psiDeductionsForPaymentsToAssociatesForPrincipalWork
  , psiTotalAmountOfOtherDeductions
  ) where

import Control.Lens
import Data.Tax

import Data.Tax.ATO.Common

-- | Business and professional items (BPI) schedule
data BusinessAndProfessionalItemsSchedule a = BusinessAndProfessionalItemsSchedule
  { _personalServicesIncome :: PersonalServicesIncome a
  }

newBusinessAndProfessionalItemsSchedule :: (Num a) => BusinessAndProfessionalItemsSchedule a
newBusinessAndProfessionalItemsSchedule = BusinessAndProfessionalItemsSchedule
  { _personalServicesIncome = newPersonalServicesIncome
  }

instance (Num a, Ord a)
    => HasTaxableIncome BusinessAndProfessionalItemsSchedule a a where
  taxableIncome = personalServicesIncome . taxableIncome

-- | __P1__ Personal services income (PSI)
personalServicesIncome
  :: Lens' (BusinessAndProfessionalItemsSchedule a) (PersonalServicesIncome a)
personalServicesIncome =
  lens _personalServicesIncome (\s b -> s { _personalServicesIncome = b })


data PersonalServicesIncome a = PersonalServicesIncome
  { _psiVoluntaryAgreement :: Money a
  , _psiWhereABNNotQuoted :: Money a
  , _psiLabourHireOrOtherSpecifiedPayments :: Money a
  , _psiOther :: Money a
  , _psiDeductionsForPaymentsToAssociatesForPrincipalWork :: Money a
  , _psiTotalAmountOfOtherDeductions :: Money a
  }

newPersonalServicesIncome :: (Num a) => PersonalServicesIncome a
newPersonalServicesIncome = PersonalServicesIncome
  { _psiVoluntaryAgreement = mempty
  , _psiWhereABNNotQuoted = mempty
  , _psiLabourHireOrOtherSpecifiedPayments = mempty
  , _psiOther = mempty
  , _psiDeductionsForPaymentsToAssociatesForPrincipalWork = mempty
  , _psiTotalAmountOfOtherDeductions = mempty
  }

-- | __M__ PSI — voluntary agreement
psiVoluntaryAgreement :: Lens' (PersonalServicesIncome a) (Money a)
psiVoluntaryAgreement =
  lens _psiVoluntaryAgreement (\s b -> s { _psiVoluntaryAgreement = b })

-- | __N__ PSI — where Australian business number not quoted
psiWhereABNNotQuoted :: Lens' (PersonalServicesIncome a) (Money a)
psiWhereABNNotQuoted =
  lens _psiWhereABNNotQuoted (\s b -> s { _psiWhereABNNotQuoted = b })

-- | __O__ PSI — labour hire or other specified payments
psiLabourHireOrOtherSpecifiedPayments :: Lens' (PersonalServicesIncome a) (Money a)
psiLabourHireOrOtherSpecifiedPayments =
  lens _psiLabourHireOrOtherSpecifiedPayments (\s b -> s { _psiLabourHireOrOtherSpecifiedPayments = b })

-- | __J__ PSI — other
psiOther :: Lens' (PersonalServicesIncome a) (Money a)
psiOther =
  lens _psiOther (\s b -> s { _psiOther = b })

-- | __K__ Deductions for payments to associates for principal work
psiDeductionsForPaymentsToAssociatesForPrincipalWork
  :: Lens' (PersonalServicesIncome a) (Money a)
psiDeductionsForPaymentsToAssociatesForPrincipalWork =
  lens _psiDeductionsForPaymentsToAssociatesForPrincipalWork
    (\s b -> s { _psiDeductionsForPaymentsToAssociatesForPrincipalWork = b })

-- | __L__ Total amount of other deductions against PSI
psiTotalAmountOfOtherDeductions :: Lens' (PersonalServicesIncome a) (Money a)
psiTotalAmountOfOtherDeductions =
  lens _psiTotalAmountOfOtherDeductions (\s b -> s { _psiTotalAmountOfOtherDeductions = b })


instance (Num a, Ord a) => HasTaxableIncome PersonalServicesIncome a a where
  taxableIncome = to (max mempty . netPSI)

-- | Net PSI
netPSI :: (Num a) => PersonalServicesIncome a -> Money a
netPSI a =
  view psiVoluntaryAgreement a
  $+$ view psiWhereABNNotQuoted a
  $+$ view psiLabourHireOrOtherSpecifiedPayments a
  $+$ view psiOther a
  $-$ view psiDeductionsForPaymentsToAssociatesForPrincipalWork a
  $-$ view psiTotalAmountOfOtherDeductions a
