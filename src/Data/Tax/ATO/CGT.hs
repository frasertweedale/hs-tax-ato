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

The main functions you need are 'totalCapitalGain' and 'netCapitalGainOrLoss'.

-}
module Data.Tax.ATO.CGT
  (
    CGTEvent(..)
  , capitalGain
  , capitalLoss
  , isCapitalGain
  , isCapitalLoss
  , totalCapitalGain
  , netCapitalGainOrLoss
  ) where

import Data.Foldable (toList)
import Data.List (partition)
import Data.Semigroup ((<>))
import Numeric.Natural (Natural)

import Control.Lens (over, both)
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

-- | Capital gain as a positive amount.
-- /$0/ if the event not a gain.
--
capitalGain :: (Num a, Ord a) => CGTEvent a -> Money a
capitalGain event =
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
isCapitalGain = (> mempty) . capitalGain

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

-- | Sum of all capital gains.  Losses ignored.
--
-- Input /H/ at /item 18/ on tax return.
--
totalCapitalGain :: (Num a, Ord a, Foldable t) => t (CGTEvent a) -> Money a
totalCapitalGain = foldMap (max mempty . capitalGain)

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
  -> Either (Money a) (Money a)  -- ^ 'Left loss' or 'Right gain'
netCapitalGainOrLoss carry events =
  let
    l = toList events
    (discountableGain, nonDiscountableGain) =
      over both (foldMap capitalGain) (partition discountApplicable l)
    loss = foldMap capitalLoss l
    (nonDiscLessLoss, remLoss) = sub nonDiscountableGain (loss <> carry)
    (discLessLoss, finalLoss) = sub discountableGain remLoss
    discGain = nonDiscLessLoss <> (discLessLoss $* 0.5)
  in
    if discGain > mempty
    then Right discGain
    else Left finalLoss

-- | @sub x y@ = subtract @y@ from @x@, clamping to 0 and
-- returning @(result, leftovers)@
--
sub :: (Num a, Ord a) => Money a -> Money a -> (Money a, Money a)
sub x y =
  let r = x $-$ y
  in (max mempty r, over money abs (min mempty r))
