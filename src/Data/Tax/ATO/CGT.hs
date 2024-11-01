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

Types and calculations for /capital gains tax/ (CGT).

This module does not implement the /indexation method/ for cost base reduction.
If you have assets acquired before 1999-09-21 11:45:00+1000… file a ticket or
send a patch!

The main function you need is 'assessCGTEvents'.

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Tax.ATO.CGT
  (
  -- * CGT events
    CGTEvent(..)

  -- * CGT assessments for tax returns
  , assessCGTEvents
  , CGTAssessment
  , nullCGTAssessment
  , HasCapitalLossCarryForward(..)
  , cgtNetGain
  , cgtTotalCurrentYearGains
  , cgtNetLossesCarriedForward

  -- * CGT computations
  , HasCapitalGain(..)
  , capitalLoss
  , isCapitalGain
  , isCapitalLoss
  , discountApplicable
  ) where

import Data.Foldable (toList)
import Data.List (partition)

import Control.Lens (Getter, Lens', both, lens, over, to, view)
import Data.Time.Calendar (Day, diffDays)
import Data.Tax

-- | A CGT Event (usually an asset disposal)
--
data CGTEvent a = CGTEvent
  { assetDesc :: String
  , units :: a
  , acquisitionDate :: Day
  , acquisitionPrice :: Money a
  , acquisitionCosts :: Money a
  , disposalDate :: Day
  , disposalPrice :: Money a
  , disposalCosts :: Money a
  , capitalCosts :: Money a
  , ownershipCosts :: Money a
  }
  deriving (Show)

instance Functor CGTEvent where
  fmap f (CGTEvent k n t1 p1 b1 t2 p2 b2 cap own) =
    CGTEvent k (f n)
      t1 (f <$> p1) (f <$> b1)
      t2 (f <$> p2) (f <$> b2)
      (f <$> cap) (f <$> own)

reducedCostBase :: Num a => CGTEvent a -> Money a
reducedCostBase event =
  (units event *$ acquisitionPrice event)
  $+$ acquisitionCosts event
  $+$ disposalCosts event
  $+$ capitalCosts event

costBase :: Num a => CGTEvent a -> Money a
costBase event = reducedCostBase event $+$ ownershipCosts event

capitalGain' :: (Num a, Ord a) => CGTEvent a -> Money a
capitalGain' event =
  max mempty (units event *$ disposalPrice event $-$ costBase event)

-- | The capital loss as a /non-negative/ amount.
-- /$0/ if the event is not a loss.
--
capitalLoss :: (Num a, Ord a) => CGTEvent a -> Money a
capitalLoss event = over money abs $
  min mempty (units event *$ disposalPrice event $-$ reducedCostBase event)

-- | Whether the CGT event is a capital gain.  /Not the opposite
-- of 'isCapitalLoss'!/  A CGT event may be neither a loss nor a
-- gain.
--
isCapitalGain :: (Num a, Ord a) => CGTEvent a -> Bool
isCapitalGain = (> mempty) . capitalGain'

-- | Whether the CGT event is a capital loss.  /Not the opposite
-- of 'isCapitalGain'!/  A CGT event may be neither a loss nor a
-- gain.
--
isCapitalLoss :: (Num a, Ord a) => CGTEvent a -> Bool
isCapitalLoss = (> mempty) . capitalLoss

-- | Whether the 50% CGT discount is applicable to this event (only
-- with regard to duration of holding; acquisition date ignored).
--
discountApplicable :: CGTEvent a -> Bool
discountApplicable ev =
  diffDays (disposalDate ev) (acquisitionDate ev) > 365

-- | Types that may have a capital gain.  Non-discounted, losses ignored.
class HasCapitalGain a b c where
  capitalGain :: Getter (a b) (Money c)

-- | Capital gain as a positive amount.  /$0/ if the event not a gain.
instance (Num a, Ord a) => HasCapitalGain CGTEvent a a where
  capitalGain = to capitalGain'

-- | Sum of capital gains, ignoring losses.
-- Input __H__ at /item 18/ on tax return.
--
instance (Foldable t, HasCapitalGain x a a, Num a) => HasCapitalGain t (x a) a where
  capitalGain = to (foldMap (view capitalGain))


-- | @sub x y@ = subtract @y@ from @x@, clamping to 0 and
-- returning @(result, leftovers)@
--
sub :: (Num a, Ord a) => Money a -> Money a -> (Money a, Money a)
sub x y =
  let r = x $-$ y
  in (max mempty r, over money abs (min mempty r))

-- | Assess the total capital gains and net capital gain or loss.
--
-- Losses are used to offset non-discountable capital gains
-- first, then discountable gains, before the discount is applied
-- to discountable gains.
--
-- __Does not implement the indexation method for cost-base reduction.__
--
assessCGTEvents
  :: (Fractional a, Ord a, Foldable t)
  => Money a            -- ^ capital loss carried forward
  -> t (CGTEvent a)
  -> CGTAssessment a
assessCGTEvents carry events =
  let
    l = toList events
    (discountableGain, nonDiscountableGain) =
      over both (view capitalGain) (partition discountApplicable l)
    totalGain = discountableGain <> nonDiscountableGain
    totalLoss = foldMap capitalLoss l
    (nonDiscountableGainLossesApplied, unappliedLosses) = sub nonDiscountableGain (totalLoss <> carry)
    (discountableGainLossesApplied, finalUnappliedLosses) = sub discountableGain unappliedLosses
    discount = discountableGainLossesApplied $* 0.5
    discountedGain = nonDiscountableGainLossesApplied <> (discountableGainLossesApplied $-$ discount)

    lossesApplied = (totalLoss <> carry) $-$ finalUnappliedLosses
    priorYearLossesApplied = min carry lossesApplied
    currentYearLossesApplied = lossesApplied $-$ priorYearLossesApplied
  in
    CGTAssessment
      totalGain
      totalLoss
      currentYearLossesApplied
      priorYearLossesApplied
      finalUnappliedLosses
      discount
      discountedGain

-- | Total undiscounted gains and net gain/loss for tax assessment
data CGTAssessment a = CGTAssessment
  { _totalCurrentYearCapitalGains :: Money a
  , _totalCurrentYearCapitalLosses :: Money a
  , _totalCurrentYearCapitalLossesApplied :: Money a
  , _totalPriorYearCapitalLossesApplied :: Money a
  , _netCapitalLossesCarriedForward :: Money a
  , _totalCGTDiscountApplied :: Money a
  , _netCapitalGain :: Money a
  }
  deriving (Eq)

-- | A 'CGTAssessment' whose values are all zero
nullCGTAssessment :: (Num a) => CGTAssessment a
nullCGTAssessment = CGTAssessment mempty mempty mempty mempty mempty mempty mempty

instance Functor CGTAssessment where
  fmap f (CGTAssessment a b c d e g h) = CGTAssessment
    (fmap f a)
    (fmap f b)
    (fmap f c)
    (fmap f d)
    (fmap f e)
    (fmap f g)
    (fmap f h)

-- | __18A__ The net capital gain, or zero if a loss.
cgtNetGain :: Lens' (CGTAssessment a) (Money a)
cgtNetGain = lens _netCapitalGain (\s b -> s { _netCapitalGain = b })

-- | __18H__ Total current year capital gains
cgtTotalCurrentYearGains :: Lens' (CGTAssessment a) (Money a)
cgtTotalCurrentYearGains =
  lens _totalCurrentYearCapitalGains (\s b -> s { _totalCurrentYearCapitalGains = b })

-- | __18V__ Net capital losses carried forward to later income years
cgtNetLossesCarriedForward :: Lens' (CGTAssessment a) (Money a)
cgtNetLossesCarriedForward =
  lens _netCapitalLossesCarriedForward (\s b -> s { _netCapitalLossesCarriedForward = b })

-- | Types that have a carry-forward capital loss (either as an
-- input or an output).
class HasCapitalLossCarryForward a b where
  capitalLossCarryForward :: Lens' (a b) (Money b)

instance HasCapitalLossCarryForward CGTAssessment a where
  capitalLossCarryForward =
    lens _netCapitalLossesCarriedForward (\s b -> s { _netCapitalLossesCarriedForward = b })
