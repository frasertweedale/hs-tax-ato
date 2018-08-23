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
  , CGTAssessment(CGTAssessment)
  , CGTNetGainOrLoss(..)
  , HasCapitalLossCarryForward(..)
  , cgtTotalGain
  , cgtNetGainOrLoss
  , cgtNetGain

  -- * CGT computations
  , capitalGain
  , capitalLoss
  , isCapitalGain
  , isCapitalLoss
  , netCapitalGainOrLoss
  ) where

import Data.Foldable (toList)
import Data.List (partition)
import Data.Semigroup ((<>))
import Numeric.Natural (Natural)

import Control.Lens (Getter, Lens', both, lens, over, to, view)
import Data.Time.Calendar (Day, diffDays)
import Data.Tax

-- | A CGT Event (usually an asset disposal)
--
data CGTEvent a = CGTEvent
  { assetDesc :: String
  , units :: Natural
  , acquisitionDate :: Day
  , acquisitionPrice :: Money a
  , acquisitionCosts :: Money a
  , disposalDate :: Day
  , disposalPrice :: Money a
  , disposalCosts :: Money a
  , capitalCosts :: Money a
  , ownershipCosts :: Money a
  }

numUnits :: Num b => CGTEvent a -> b
numUnits = fromIntegral . units

reducedCostBase :: Num a => CGTEvent a -> Money a
reducedCostBase event =
  (numUnits event *$ acquisitionPrice event)
  $+$ acquisitionCosts event
  $+$ disposalCosts event
  $+$ capitalCosts event

costBase :: Num a => CGTEvent a -> Money a
costBase event = reducedCostBase event $+$ ownershipCosts event

capitalGain' :: (Num a, Ord a) => CGTEvent a -> Money a
capitalGain' event =
  max mempty (numUnits event *$ disposalPrice event $-$ costBase event)

-- | The capital loss as a /non-negative/ amount.
-- /$0/ if the event is not a loss.
--
capitalLoss :: (Num a, Ord a) => CGTEvent a -> Money a
capitalLoss event = over money abs $
  min mempty (numUnits event *$ disposalPrice event $-$ reducedCostBase event)

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

-- | Compute the capital gain.  Losses are ignored.
class HasCapitalGain a b c where
  capitalGain :: Getter (a b) (Money c)

-- | Capital gain as a positive amount.  /$0/ if the event not a gain.
instance (Num a, Ord a) => HasCapitalGain CGTEvent a a where
  capitalGain = to capitalGain'

-- | Sum of capital gains, ignoring losses.
--
-- Input /H/ at /item 18/ on tax return.
--
instance (Foldable t, HasCapitalGain x a a, Num a) => HasCapitalGain t (x a) a where
  capitalGain = to (foldMap (view capitalGain))



-- | Compute the /discounted/ gain or carry-forward loss
--
-- Losses are used to offset non-discountable capital gains
-- first, then discountable gains, before the discount is applied
-- to discountable gains.
--
-- *Does not implement the indexation method for cost-base reduction!*
--
netCapitalGainOrLoss
  :: (Fractional a, Ord a, Foldable t)
  => Money a                     -- ^ loss carried forward
  -> t (CGTEvent a)              -- ^ CGT events
  -> CGTNetGainOrLoss a
netCapitalGainOrLoss carry events =
  let
    l = toList events
    (discountableGain, nonDiscountableGain) =
      over both (view capitalGain) (partition discountApplicable l)
    loss = foldMap capitalLoss l
    (nonDiscLessLoss, remLoss) = sub nonDiscountableGain (loss <> carry)
    (discLessLoss, finalLoss) = sub discountableGain remLoss
    discGain = nonDiscLessLoss <> (discLessLoss $* 0.5)
  in
    if discGain > mempty
    then CGTNetGain discGain
    else CGTLoss finalLoss

-- | @sub x y@ = subtract @y@ from @x@, clamping to 0 and
-- returning @(result, leftovers)@
--
sub :: (Num a, Ord a) => Money a -> Money a -> (Money a, Money a)
sub x y =
  let r = x $-$ y
  in (max mempty r, over money abs (min mempty r))

-- | Assess the total capital gains and net capital gain or loss.
assessCGTEvents
  :: (Fractional a, Ord a, Foldable t)
  => Money a            -- ^ capital loss carried forward
  -> t (CGTEvent a)
  -> CGTAssessment a
assessCGTEvents carry evs = CGTAssessment
  (view capitalGain evs)
  (netCapitalGainOrLoss carry evs)

-- | Total undiscounted gains and net gain/loss for tax assessment
data CGTAssessment a = CGTAssessment
  { _cgtaTotal :: Money a
  , _cgtaNet :: CGTNetGainOrLoss a
  }
  deriving (Show)

instance Functor CGTAssessment where
  fmap f (CGTAssessment a b) = CGTAssessment (fmap f a) (fmap f b)

cgtTotalGain :: Lens' (CGTAssessment a) (Money a)
cgtTotalGain = lens _cgtaTotal (\s b -> s { _cgtaTotal = b })

cgtNetGainOrLoss :: Lens' (CGTAssessment a) (CGTNetGainOrLoss a)
cgtNetGainOrLoss = lens _cgtaNet (\s b -> s { _cgtaNet = b })

-- | The net capital gain, or zero if a loss.
cgtNetGain :: (Num a) => Getter (CGTAssessment a) (Money a)
cgtNetGain = cgtNetGainOrLoss . to f
  where
  f (CGTNetGain a) = a
  f _ = mempty

-- | A net (loss offset, discounted) gain, or the loss amount
data CGTNetGainOrLoss a = CGTNetGain (Money a) | CGTLoss (Money a)
  deriving (Show)

instance Functor CGTNetGainOrLoss where
  fmap f (CGTNetGain a) = CGTNetGain (fmap f a)
  fmap f (CGTLoss a)    = CGTLoss (fmap f a)

class HasCapitalLossCarryForward a b where
  capitalLossCarryForward :: Lens' (a b) (Money b)

instance (Num a, Eq a) => HasCapitalLossCarryForward CGTNetGainOrLoss a where
  capitalLossCarryForward = lens
    (\s -> case s of CGTLoss a -> a ; _ -> mempty)
    (\s b -> if b == mempty then s else CGTLoss b)

instance (Num a, Eq a) => HasCapitalLossCarryForward CGTAssessment a where
  capitalLossCarryForward = cgtNetGainOrLoss . capitalLossCarryForward
