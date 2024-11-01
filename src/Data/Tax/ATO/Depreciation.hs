
{-|

Types and computations for calculating decline in value.

-}

module Data.Tax.ATO.Depreciation
  ( DepreciatingAsset(..)
  , EffectiveLife
  , DepreciationMethod
  , diminishingValueMethod
  , primeCostMethod
  , depreciationSchedule
  , declineInValue
  ) where

import Data.Proxy (Proxy(..))

import Data.Time (Day, Year, diffDays, fromGregorian)

import Data.Tax
import Data.Tax.ATO.FY

-- | Effective life in years
type EffectiveLife = Rational

data DepreciatingAsset a = DepreciatingAsset
  Day             -- ^ Start date of decline in value
  (Money a)       -- ^ Asset's cost
  EffectiveLife   -- ^ Effective life, in years.
  (DepreciationMethod a)

-- | Given the start date, effective life and the pair
-- @(cost, baseValue)@, calculate the decline in value over
-- a full year (i.e. /days held/ = 365).
--
type DepreciationMethod a =
  Day -> EffectiveLife -> (Money a, Money a) -> Money a

-- | Decline in value = @remainingValue * r / effectiveLife@,
-- where @r = 1.5@ for assets acquired before 2006-05-10,
-- otherwise @2.0@.
diminishingValueMethod :: (Fractional a) => DepreciationMethod a
diminishingValueMethod start life = ($/ fromRational life) . ($* factor) . snd
  where
  factor  | start < fromGregorian 2006 05 10  = 1.5
          | otherwise                         = 2

-- | /Straight-line depreciation/.
-- Decline in value = cost / effective life.
primeCostMethod :: (Fractional a) => DepreciationMethod a
primeCostMethod _ life = ($/ fromRational life) . fst

-- | Calculate decline in value over financial years
depreciationSchedule
  :: (Fractional a, Ord a)
  => DepreciatingAsset a -> [(Year, Money a)]
depreciationSchedule (DepreciatingAsset t1 cost life method) =
  let
    fy = financialYear t1
    (_, eofy) = financialYearRange fy
    daysList = diffDays eofy t1 + 1 : fmap daysInYear [fy + 1..]

    step (_, value) nDays =
      let
        decline = (fromIntegral nDays *$ method t1 life (cost, value)) $/ 365
      in
        if decline > value
          then (value, Money 0)
          else (decline, value $-$ decline)
  in
    zip [fy..]
    . takeWhile (>= Money 1)
    . fmap fst
    . drop 1
    $ scanl step (Money 0, cost) daysList

-- | Calculate decline in value for the given financial year
-- (financial year given as type, via 'Proxy').
declineInValue
  :: (FinancialYear y, Fractional a, Ord a)
  => Proxy y -> DepreciatingAsset a -> Money a
declineInValue proxy asset =
  case dropWhile ((< fromProxy proxy) . fst) (depreciationSchedule asset) of
    (y,r):_ | y == fromProxy proxy  -> r
    _                               -> Money 0
