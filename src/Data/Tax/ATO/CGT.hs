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


{-# LANGUAGE LambdaCase #-}

{-|

Types and calculations for /capital gains tax/ (CGT).

This module does not implement the /indexation method/ for cost base reduction.
If you have assets acquired before 1999-09-21 11:45:00+1000… file a ticket or
send a patch!

The main function you need is 'assessCGTEvents'.

Things that are __not yet implemented__:

* Small business CGT concessions
* Exemptions, rollovers, and earnout arrangements

-}
module Data.Tax.ATO.CGT
  (
  -- * CGT assets
    CGTAssetType(..)
  , CGTAssetFlag(..)
  , CGTAsset
  , assetDescription
  , assetShare
  , assetType
  , assetFlag
  , assetUnits
  , assetAcquisitionDate
  , assetAcquisitionPrice
  , assetAcquisitionCosts
  , assetCapitalCosts
  , assetOwnershipCosts
  -- ** Constructors
  , newCGTAsset

  -- * CGT events
  , CGTEvent
  , eventAsset
  , eventCode
  , eventDate
  , eventPrice
  , eventCosts
  , CGTExemptionRolloverOrAdditionalDiscountCode(..)
  -- ** Constructors
  , cgtEventA1
  , cgtEventB1
  , cgtEventC1
  , cgtEventC2
  , cgtEventC3
  , cgtEventE10
  -- ** Helper functions
  , eventProceeds

  -- * Capital gains from trusts
  , CapitalGainsFromTrust
  , newCapitalGainsFromTrust
  , trustGrossCapitalGain
  , trustCapitalLossesApplied
  , trustCGTDiscountApplied
  , trustCGTSmallBusinessConcessionApplied
  , prepTrustCGT

  -- * CGT assessments for tax returns
  , assessCGTEvents
  , CGTAssessment
  , nullCGTAssessment
  , cgtNetGain
  , cgtTotalCurrentYearGains
  , cgtTotalCurrentYearLosses
  , cgtTotalCurrentYearLossesApplied
  , cgtTotalPriorYearLossesApplied
  , cgtTotalCGTDiscountApplied
  , cgtNetLossesCarriedForward
  , cgtGainsAndLossesByCategory

  -- ** Carry-forward
  , HasCapitalLossCarryForward(..)
  , CapitalLossCarryForward
  , newCapitalLossCarryForward
  , capitalLossCarryForwardCollectables
  , capitalLossCarryForwardOther

  -- ** Helper types for CGT Schedule
  , CGTScheduleCategory(..)

  -- * CGT computations
  , capitalGain
  , capitalLoss
  , isCapitalGain
  , isCapitalLoss
  , discountApplicable
  ) where

import Data.Foldable (toList)
import Data.List (partition)

import Control.Lens
  ( Getter, Lens', Traversal'
  , (&), _3, anyOf, both, lens, over, preview, set, to, view, views
  )
import qualified Data.Map as M
import Data.Time.Calendar (Day, diffDays)
import Data.Tax

import Data.Tax.ATO.Common (EntityType(..))


-- | The CGT schedule requires reporting of gains and losses for
-- different asset types.  Also, collectables and personal use
-- assets are subject to different rules from other assets.
-- Specifically, capital losses from personal use assets are
-- disregarded, and capital losses from collectables can only offset
-- capital gains from collectables.
--
data CGTAssetType
  = AssetTypeSharesAUListed
  | AssetTypeSharesOther
  | AssetTypeUnitsAUListed
  | AssetTypeUnitsOther
  | AssetTypeRealEstateAU
  | AssetTypeRealEstateOther
  | AssetTypeCollectable
  | AssetTypePersonalUse
  | AssetTypeOther
  deriving (Eq, Ord, Show)

-- | Flags for CGT assets that can affect CGT calculations.
--
data CGTAssetFlag
  = MainResidence
  | AffordableHousing
  | SmallBusinessActiveAsset
  | EarlyStageInnovationCompany
  deriving (Eq, Ord, Show)

data CGTAsset a = CGTAsset
  { _assetDescription       :: String
  , _assetShare             :: Rational     -- owner's share of asset
  , _assetType              :: CGTAssetType
  , _assetFlag              :: Maybe CGTAssetFlag
  , _assetUnits             :: Rational -- number of units
  , _assetAcquisitionDate   :: Day
  , _assetAcquisitionPrice  :: Money a  -- acquisition price (per unit)
  , _assetAcquisitionCosts  :: Money a  -- acquisition costs (of parcel)
  , _assetCapitalCosts      :: Money a
  , _assetOwnershipCosts    :: Money a
  }
  deriving (Functor, Show)

-- | Construct a new CGT asset.  Capital and ownership costs are set to zero.
-- Use 'assetFlag' to set a flag, if relevant.
newCGTAsset
  :: (Num a)
  => String   -- ^ asset description
  -> Rational -- ^ owner's share (a fraction between 0 and (typically) 1)
  -> CGTAssetType
  -> Rational -- ^ quantity of units/shares
  -> Day      -- ^ acquisition date
  -> Money a  -- ^ price per unit/share
  -> Money a  -- ^ acquisition costs (e.g. brokerage, settlement costs, etc)
  -> CGTAsset a
newCGTAsset desc share typ units date price cost =
  CGTAsset desc share typ Nothing units date price cost mempty mempty

assetDescription :: Lens' (CGTAsset a) String
assetDescription =
  lens _assetDescription (\s b -> s { _assetDescription = b })

assetShare :: Lens' (CGTAsset a) Rational
assetShare =
  lens _assetShare (\s b -> s { _assetShare = b })

assetType :: Lens' (CGTAsset a) CGTAssetType
assetType =
  lens _assetType (\s b -> s { _assetType = b })

assetFlag :: Lens' (CGTAsset a) (Maybe CGTAssetFlag)
assetFlag =
  lens _assetFlag (\s b -> s { _assetFlag = b })

assetUnits :: Lens' (CGTAsset a) Rational
assetUnits =
  lens _assetUnits (\s b -> s { _assetUnits = b })

assetAcquisitionDate :: Lens' (CGTAsset a) Day
assetAcquisitionDate =
  lens _assetAcquisitionDate (\s b -> s { _assetAcquisitionDate = b })

assetAcquisitionPrice :: Lens' (CGTAsset a) (Money a)
assetAcquisitionPrice =
  lens _assetAcquisitionPrice (\s b -> s { _assetAcquisitionPrice = b })

assetAcquisitionCosts :: Lens' (CGTAsset a) (Money a)
assetAcquisitionCosts =
  lens _assetAcquisitionCosts (\s b -> s { _assetAcquisitionCosts = b })

assetCapitalCosts :: Lens' (CGTAsset a) (Money a)
assetCapitalCosts =
  lens _assetCapitalCosts (\s b -> s { _assetCapitalCosts = b })

assetOwnershipCosts :: Lens' (CGTAsset a) (Money a)
assetOwnershipCosts =
  lens _assetOwnershipCosts (\s b -> s { _assetOwnershipCosts = b })


data CGTExemptionRolloverOrAdditionalDiscountCode
  = {- A -} SmallBusinessActiveAssetReduction
  | {- B -} SmallBusinessRetirementExemption
  | {- C -} SmallBusinessRollover
  | {- D -} SmallBusiness15YearExemption
  | {- E -} ForeignResidentCGTExemption
  | {- F -} ScripForScripRollover
  | {- I -} MainResidenceExemption
  | {- J -} CapitalGainsDisregardedAsAResultOfTheSaleOfAPreCGTAsset
  | {- K -} DisposalOrCreationOfAssetsInAWhollyOwnedCompany
  | {- L -} ReplacementAssetRollovers
  | {- M -} ExchangeOfSharesOrUnits
  | {- N -} ExchangeOfRightsOrOptions
  | {- O -} ExchangeOfSharesInOneCompanyForSharesInAnotherCompany
  | {- P -} ExchangeOfUnitsInAUnitTrustForSharesInACompany
  | {- R -} DemergerRollover
  | {- S -} SameAssetRollovers
  | {- T -} SmallBusinessRestructureRollover
  | {- U -} EarlyStageInvestor
  | {- V -} VentureCapitalInvestment
  | {- W -} AffordableHousingDiscount
  | {- X -} OtherExemptionsAndRollovers
  deriving (Eq, Ord, Show)

data CGTEventType a
  = CGTEventEnd (CGTAsset a)
    -- ^ Ownership of a CGT asset ends (or contracted)
  | CGTEventReAsset (CGTAsset a)
    -- ^ A CGT event relating to the asset, where cost base ignored.
    -- Acquisition date determines discounts, etc.
  | CGTEventNoAsset
    -- ^ A CGT event not relating to an owned asset
  | CGTEventTrustDistribution IsDiscountable
    -- ^ Special purpose constructor for representing distributions
    -- of capital gains from trusts.
  deriving (Functor, Show)

data IsDiscountable = Discountable | NotDiscountable
  deriving (Show)

-- | A CGT event (usually an asset disposal)
data CGTEvent a = CGTEvent
  { _eventType  :: CGTEventType a
  , _eventCode  :: Maybe CGTExemptionRolloverOrAdditionalDiscountCode
  , _eventDate  :: Day
  , _eventPrice :: Money a
  , _eventCosts :: Money a
  }
  deriving (Functor, Show)

eventType :: Lens' (CGTEvent a) (CGTEventType a)
eventType =
  lens _eventType (\s b -> s { _eventType = b })

-- | A CGT event could have zero or one related assets.
eventAsset :: Traversal' (CGTEvent a) (CGTAsset a)
eventAsset = eventType . l
  where
  l f = \case
    CGTEventEnd a     -> CGTEventEnd      <$> f a
    CGTEventReAsset a -> CGTEventReAsset  <$> f a
    s                 -> pure s

eventCode :: Lens' (CGTEvent a) (Maybe CGTExemptionRolloverOrAdditionalDiscountCode)
eventCode =
  lens _eventCode (\s b -> s { _eventCode = b })

eventDate :: Lens' (CGTEvent a) Day
eventDate =
  lens _eventDate (\s b -> s { _eventDate = b })

eventPrice :: Lens' (CGTEvent a) (Money a)
eventPrice =
  lens _eventPrice (\s b -> s { _eventPrice = b })

eventCosts :: Lens' (CGTEvent a) (Money a)
eventCosts =
  lens _eventCosts (\s b -> s { _eventCosts = b })

-- | A1 - Disposal of a CGT asset
cgtEventA1
  :: CGTAsset a
  -- ^ The asset the event is associated with.  If disposing of a
  -- share or fewer shares/units than held, the asset must be split
  -- beforehand.
  -> Maybe CGTExemptionRolloverOrAdditionalDiscountCode
  -- ^ Exemption, rollover or discount code applying to this event, if any.
  -> Day      -- ^ When the disposal contract is entered into or, if none,
              --   when the entity stops being the asset's owner
  -> Money a  -- ^ Capital proceeds (per unit)
  -> Money a  -- ^ Costs of disposal (consolidated; for whole asset)
  -> CGTEvent a
cgtEventA1 asset code date price cost =
  CGTEvent (CGTEventEnd asset) code date price cost

-- | B1 - Use and enjoyment before title passes
cgtEventB1
  :: CGTAsset a
  -> Maybe CGTExemptionRolloverOrAdditionalDiscountCode
  -- ^ Exemption, rollover or discount code applying to this event, if any.
  -> Day      -- ^ When use of the CGT asset passes
  -> Money a  -- ^ Capital proceeds (per unit)
  -> Money a  -- ^ Costs of arrangement
  -> CGTEvent a
cgtEventB1 = cgtEventA1

-- | C1 - Loss or destruction of a CGT asset
cgtEventC1
  :: CGTAsset a
  -> Maybe CGTExemptionRolloverOrAdditionalDiscountCode
  -- ^ Exemption, rollover or discount code applying to this event, if any.
  -> Day      -- ^ When compensation is first received or, if none,
              --   when the loss is discovered or destruction occurred
  -> Money a  -- ^ Capital proceeds (per unit)
  -> Money a  -- ^ Incidental costs relating to the event
  -> CGTEvent a
cgtEventC1 = cgtEventA1

-- | C2 - Cancellation, surrender and similar endings
cgtEventC2
  :: CGTAsset a
  -> Maybe CGTExemptionRolloverOrAdditionalDiscountCode
  -- ^ Exemption, rollover or discount code applying to this event, if any.
  -> Day      -- ^ When the contract ending an asset is entered into or,
              --   if none, when an asset ends
  -> Money a  -- ^ Capital proceeds (per unit)
  -> Money a  -- ^ Costs relating to the event
  -> CGTEvent a
cgtEventC2 = cgtEventA1

-- | C3 - End of an option to acquire shares etc
cgtEventC3
  :: (Num a)
  => CGTAsset a
  -- ^ The option.  Its acqusition price could be /negative/, i.e.
  -- capital proceeds from granting the option.  Acquisition costs
  -- record the expenditure in granting the option.
  -- granting the option is recorded in the a
  -> Day      -- ^ When the option ends
  -> CGTEvent a
cgtEventC3 asset date = CGTEvent (CGTEventEnd asset) Nothing date (Money 0) (Money 0)

-- | E10 - Annual cost base reduction exceeds cost base of interest
-- in attribution managed investment trust.
cgtEventE10
  :: (Num a)
  => CGTAsset a
  -- ^ The asset the event is associated with.
  -> Day
  -> Money a
  -- ^ Amount cost base reduction exceeds cost base (consolidated; for whole asset)
  -> CGTEvent a
cgtEventE10 asset date amount =
  CGTEvent (CGTEventReAsset asset) Nothing date amount (Money 0)

-- | Capital proceeds arising from CGT event
eventProceeds :: CGTEvent Rational -> Money Rational
eventProceeds ev = case preview (eventAsset . assetUnits) ev of
  Just n  -> n *$ view eventPrice ev  -- asset with units (possibly 1)
  Nothing ->      view eventPrice ev  -- bare capital proceeds


-- | @sub x y@ = subtract @y@ from @x@, clamping to 0 and
-- returning @(result, leftovers)@
--
sub :: (Num a, Ord a) => Money a -> Money a -> (Money a, Money a)
sub x y =
  let r = x $-$ y
  in (max mempty r, over money abs (min mempty r))


-- | Reduced cost base of the (whole) asset subject to this CGT event
assetReducedCostBase :: (Fractional a) => CGTAsset a -> Money a
assetReducedCostBase asset =
  ( views assetUnits fromRational asset
    *$ view assetAcquisitionPrice asset )
  $+$ view assetAcquisitionCosts asset
  $+$ view assetCapitalCosts asset

-- | Cost base of the (whole) asset subject to this CGT event
assetCostBase :: (Fractional a) => CGTAsset a -> Money a
assetCostBase asset =
  assetReducedCostBase asset
  $+$ view assetOwnershipCosts asset

-- | Take of the amount in the same proportion as the share of the given asset.
shareOf :: (Fractional a) => CGTAsset a -> Money a -> Money a
shareOf asset = ($* views assetShare fromRational asset)

-- | Capital gain, scaled to the owner's share (per 'assetShare').
-- __Zero__ if there is no capital gain.  Does not apply discounts
-- or exemptions.
capitalGain :: (Fractional a, Ord a) => CGTEvent a -> Money a
capitalGain event =
  max mempty
  $ case view eventType event of
    CGTEventEnd asset ->
      shareOf asset
      $ views assetUnits fromRational asset *$ view eventPrice event
        $-$ assetCostBase asset
        $-$ view eventCosts event
    CGTEventReAsset asset ->
      shareOf asset
      $ view eventPrice event
        $-$ view eventCosts event
    CGTEventNoAsset ->
      view eventPrice event
      $-$ view eventCosts event
    CGTEventTrustDistribution _ ->
      view eventPrice event

-- | The capital loss as a __non-negative__ amount, scaled to the
-- owner's share (per 'assetShare').  __Zero__ if there is no
-- capital loss.
--
-- Capital losses on personal use assets are disregarded;
-- if type is 'AssetTypePersonalUse' this will always be zero.
--
capitalLoss :: (Fractional a, Ord a) => CGTEvent a -> Money a
capitalLoss event =
  fmap abs . min mempty
  $ case view eventType event of
    CGTEventEnd asset ->
      shareOf asset
      $ ( case view assetType asset of AssetTypePersonalUse -> const (Money 0) ; _ -> id )
      $ views assetUnits fromRational asset *$ view eventPrice event
        $-$ assetReducedCostBase asset
        $-$ view eventCosts event
    CGTEventReAsset asset ->
      shareOf asset
      $ view eventPrice event
        $-$ view eventCosts event
    CGTEventNoAsset ->
      view eventPrice event
      $-$ view eventCosts event
    CGTEventTrustDistribution _ ->
      mempty  -- trust cannot distribute a capital loss


-- | Whether the CGT event is a capital gain.  /Not the opposite
-- of 'isCapitalLoss'!/  A CGT event may be neither a loss nor a
-- gain.
--
isCapitalGain :: (Fractional a, Ord a) => CGTEvent a -> Bool
isCapitalGain = (> mempty) . capitalGain

-- | Whether the CGT event is a capital loss.  /Not the opposite
-- of 'isCapitalGain'!/  A CGT event may be neither a loss nor a
-- gain.
--
isCapitalLoss :: (Fractional a, Ord a) => CGTEvent a -> Bool
isCapitalLoss = (> mempty) . capitalLoss

-- | Whether the CGT discount is (potentially) applicable to this event.
-- Whether it is actually applicable and the discount proportion depends
-- on the type of the entity being assessed.
--
discountApplicable :: CGTEvent a -> Bool
discountApplicable ev =
  case view eventType ev of
    CGTEventTrustDistribution    Discountable -> True
    CGTEventTrustDistribution NotDiscountable -> False
    _ -> case preview eventAsset ev of
      Nothing -> False
      Just a  -> diffDays (view eventDate ev) (view assetAcquisitionDate a) > 365


-- | Assess the total capital gains and net capital gain or loss.
--
-- __Assumes all events relate to a particular financial year.__
-- This function does not check or filter the input data.
--
-- Use 'prepTrustCGT' to process 'CapitalGainsFromTrust' values into
-- CGT events.
--
-- Losses are used to offset non-discountable capital gains
-- first, then discountable gains, before the discount is applied
-- to discountable gains.
--
-- __Does not implement the indexation method for cost-base reduction.__
--
assessCGTEvents
  :: (Fractional a, Ord a, Foldable t)
  => EntityType
  -> CapitalLossCarryForward a
  -> t (CGTEvent a)
  -> CGTAssessment a
assessCGTEvents etype carry events =
  let
    l = toList events
    (lC, lO) = partition (anyOf (eventAsset . assetType) (== AssetTypeCollectable)) l
    carryC = view capitalLossCarryForwardCollectables carry
    carryO = view capitalLossCarryForwardOther        carry
    r@(rC, rO) =
      ( applyLossesAndDiscounts etype carryC lC
      , applyLossesAndDiscounts etype carryO lO
      )
    appliedLossC        = view vectorTotalLoss rC $+$ carryC $-$ view vectorUnappliedLoss rC
    appliedLossO        = view vectorTotalLoss rO $+$ carryO $-$ view vectorUnappliedLoss rO
    curFYLossesAppliedC = min appliedLossC (view vectorTotalLoss rC)
    curFYLossesAppliedO = min appliedLossO (view vectorTotalLoss rO)
    priorLossesAppliedC = appliedLossC $-$ curFYLossesAppliedC
    priorLossesAppliedO = appliedLossO $-$ curFYLossesAppliedO
    byCategory =
      M.fromListWith (<>) $ flip fmap l $ \ev ->
        ( scheduleCategory ev, (capitalGain ev, capitalLoss ev) )
  in
    CGTAssessment
      (view (both . vectorTotalGain) r)
      (view (both . vectorTotalLoss) r)

      -- current year losses applied
      ( curFYLossesAppliedC <> curFYLossesAppliedO )

      -- prior year losses applied
      ( priorLossesAppliedC <> priorLossesAppliedO )

      ( newCapitalLossCarryForward
        & set capitalLossCarryForwardCollectables (view vectorUnappliedLoss rC)
        & set capitalLossCarryForwardOther        (view vectorUnappliedLoss rO) )
      (view (both . vectorDiscount) r)
      (view (both . vectorNetGain) r)

      byCategory

-- | Internal data type for asset type-partitioned CGT calculations
data CGTVector a = CGTVector
  { _vectorTotalGain :: Money a
  , _vectorTotalLoss :: Money a
  , _vectorNetGain   :: Money a
  , _vectorUnappliedLoss :: Money a
  , _vectorDiscount  :: Money a
  }
vectorTotalGain, vectorTotalLoss, vectorNetGain, vectorUnappliedLoss, vectorDiscount
  :: Lens' (CGTVector a) (Money a)
vectorTotalGain     = lens _vectorTotalGain     (\s b -> s { _vectorTotalGain = b })
vectorTotalLoss     = lens _vectorTotalLoss     (\s b -> s { _vectorTotalLoss = b })
vectorNetGain       = lens _vectorNetGain       (\s b -> s { _vectorNetGain = b })
vectorUnappliedLoss = lens _vectorUnappliedLoss (\s b -> s { _vectorUnappliedLoss = b })
vectorDiscount      = lens _vectorDiscount      (\s b -> s { _vectorDiscount = b })

-- | Amount of capital gains from a trust or trusts (including managed funds).
--
-- Use 'newCapitalGainsFromTrust' to construct.  Apply 'prepTrustCGT' to
-- produce CGT events representing the grossed-up capital gains.
--
-- The following optics give access to the fields (common use cases should
-- not need them):
--
-- * 'trustGrossCapitalGain'
-- * 'trustCapitalLossesApplied'
-- * 'trustCGTDiscountApplied'
-- * 'trustCGTSmallBusinessConcessionApplied'
--
data CapitalGainsFromTrust a = CapitalGainsFromTrust
  { _trustGrossGain   :: Money a
  , _trustLossesAppl  :: Money a
  , _trustCGTDiscAppl :: Money a
  , _trustSBConcAppl  :: Money a
  }
  deriving (Show)

-- Construct a 'CapitalGainsFromTrust'.
--
-- Values are accepted without checking.  It is possible to
-- construct a value that doesn't make sense (e.g. a CGT
-- discount that is more than half of the gross gain).
--
newCapitalGainsFromTrust
  :: Money a  -- ^ Gross capital gain
  -> Money a  -- ^ Capital losses applied
  -> Money a  -- ^ CGT discount applied
  -> Money a  -- ^ CGT small business concessions applied
  -> CapitalGainsFromTrust a
newCapitalGainsFromTrust = CapitalGainsFromTrust

trustGrossCapitalGain                   :: Lens' (CapitalGainsFromTrust a) (Money a)
trustGrossCapitalGain
  = lens _trustGrossGain (\s b -> s { _trustGrossGain = b })

trustCapitalLossesApplied               :: Lens' (CapitalGainsFromTrust a) (Money a)
trustCapitalLossesApplied
  = lens _trustLossesAppl (\s b -> s { _trustLossesAppl = b })

trustCGTDiscountApplied                 :: Lens' (CapitalGainsFromTrust a) (Money a)
trustCGTDiscountApplied
  = lens _trustCGTDiscAppl (\s b -> s { _trustCGTDiscAppl = b })

trustCGTSmallBusinessConcessionApplied  :: Lens' (CapitalGainsFromTrust a) (Money a)
trustCGTSmallBusinessConcessionApplied
  = lens _trustSBConcAppl (\s b -> s { _trustSBConcAppl = b })


-- | Produce CGT events representing grossed-up capital gains from
-- the trust distribution.  Could produce multiple events, in cases where
-- the CGT discount or small business concessions have been applied to
-- only a proportion of the distributed gain.
--
prepTrustCGT :: (Fractional a, Eq a) => CapitalGainsFromTrust a -> [CGTEvent a]
prepTrustCGT trust =
  let
    gross = view trustGrossCapitalGain trust
    loss = view trustCapitalLossesApplied trust

    discount = view trustCGTDiscountApplied trust
    discountable = discount $* 2
    nonDiscountable = gross $-$ loss $-$ discountable

    sbc = view trustCGTSmallBusinessConcessionApplied trust
    sbcEligible = sbc $* 2
    sbcProportion = sbcEligible $/$ (gross $-$ loss $-$ discount)
    sbcCode = Just SmallBusinessActiveAssetReduction

    someDay = toEnum 0 :: Day  -- ghastly hack, = 1858-11-17
    mk (t,c,x) = CGTEvent (CGTEventTrustDistribution t) c someDay x mempty
  in
    fmap mk . filter ((/=) mempty . view _3) $
      [ ( NotDiscountable, sbcCode, nonDiscountable $* sbcProportion )
      , ( NotDiscountable, Nothing, nonDiscountable $* (1 - sbcProportion) )
      , (    Discountable, sbcCode,    discountable $* sbcProportion )
      , (    Discountable, Nothing,    discountable $* (1 - sbcProportion) )
      ]


applyLossesAndDiscounts
  :: (Fractional a, Ord a)
  => EntityType
  -> Money a    -- ^ unapplied losses from previous income years
  -> [CGTEvent a]
  -> CGTVector a
applyLossesAndDiscounts etype carry l =
  let
    (discountableGain, nonDiscountableGain) =
      over both (foldMap capitalGain) (partition discountApplicable l)
    totalGain = discountableGain <> nonDiscountableGain
    totalLoss = foldMap capitalLoss l
    (nonDiscountableGainLossesApplied, unappliedLosses) = sub nonDiscountableGain (totalLoss <> carry)
    (discountableGainLossesApplied, finalUnappliedLosses) = sub discountableGain unappliedLosses
    discount = discountableGainLossesApplied $* discountProportion etype
    discountedGain = nonDiscountableGainLossesApplied <> (discountableGainLossesApplied $-$ discount)
  in
    CGTVector totalGain totalLoss discountedGain finalUnappliedLosses discount

discountProportion :: (Fractional a) => EntityType -> a
discountProportion etype = fromRational $ case etype of
  Individual  -> 1 / 2
  Trust       -> 1 / 2
  SuperFund   -> 1 / 3
  Company     -> 0


--
-- CGTAssessment data type
--

data CGTScheduleCategory
  = CGTScheduleCategorySharesAUListed
  | CGTScheduleCategorySharesOther
  | CGTScheduleCategoryUnitsAUListed
  | CGTScheduleCategoryUnitsOther
  | CGTScheduleCategoryRealEstateAU
  | CGTScheduleCategoryRealEstateOther
  | CGTScheduleCategoryTrust
  | CGTScheduleCategoryCollectable
  | CGTScheduleCategoryOther
  | CGTScheduleCategoryDeferred
  deriving (Eq, Ord, Show)

scheduleCategory :: CGTEvent a -> CGTScheduleCategory
scheduleCategory ev =
  case view eventType ev of
    CGTEventTrustDistribution _
      -> CGTScheduleCategoryTrust
    _
      -> case preview (eventAsset . assetType) ev of
        Just AssetTypeSharesAUListed  -> CGTScheduleCategorySharesAUListed
        Just AssetTypeSharesOther     -> CGTScheduleCategorySharesOther
        Just AssetTypeUnitsAUListed   -> CGTScheduleCategoryUnitsAUListed
        Just AssetTypeUnitsOther      -> CGTScheduleCategoryUnitsOther
        Just AssetTypeRealEstateAU    -> CGTScheduleCategoryRealEstateAU
        Just AssetTypeRealEstateOther -> CGTScheduleCategoryRealEstateOther
        Just AssetTypeCollectable     -> CGTScheduleCategoryCollectable
        Just AssetTypePersonalUse     -> CGTScheduleCategoryOther
        Just AssetTypeOther           -> CGTScheduleCategoryOther
        Nothing                       -> CGTScheduleCategoryOther
    -- TODO transitional relief deferrals?


-- | This data type stores the completed CGT computations for some
-- financial year.  It includes all data required for the /CGT schedule/
-- and the tax return CGT section.
--
data CGTAssessment a = CGTAssessment
  { _totalCurrentYearCapitalGains :: Money a
  , _totalCurrentYearCapitalLosses :: Money a
  , _totalCurrentYearCapitalLossesApplied :: Money a
  , _totalPriorYearCapitalLossesApplied :: Money a
  , _capitalLossCarryForward :: CapitalLossCarryForward a
  , _totalCGTDiscountApplied :: Money a
  , _netCapitalGain :: Money a
  , _byCategory :: M.Map CGTScheduleCategory (Money a, Money a)
  }
  deriving (Eq, Functor, Show)

-- | A 'CGTAssessment' whose values are all zero
nullCGTAssessment :: (Num a) => CGTAssessment a
nullCGTAssessment = CGTAssessment mempty mempty mempty mempty mempty mempty mempty mempty

-- | __18A__ The net capital gain, or zero if a loss.
cgtNetGain :: Lens' (CGTAssessment a) (Money a)
cgtNetGain = lens _netCapitalGain (\s b -> s { _netCapitalGain = b })

-- | __18H__ Total current year capital gains
cgtTotalCurrentYearGains :: Lens' (CGTAssessment a) (Money a)
cgtTotalCurrentYearGains =
  lens _totalCurrentYearCapitalGains (\s b -> s { _totalCurrentYearCapitalGains = b })

cgtTotalCurrentYearLosses :: Lens' (CGTAssessment a) (Money a)
cgtTotalCurrentYearLosses =
  lens _totalCurrentYearCapitalLosses (\s b -> s { _totalCurrentYearCapitalLosses = b })

cgtTotalCurrentYearLossesApplied :: Lens' (CGTAssessment a) (Money a)
cgtTotalCurrentYearLossesApplied =
  lens _totalCurrentYearCapitalLossesApplied (\s b -> s { _totalCurrentYearCapitalLossesApplied = b })

cgtTotalCGTDiscountApplied :: Lens' (CGTAssessment a) (Money a)
cgtTotalCGTDiscountApplied =
  lens _totalCGTDiscountApplied (\s b -> s { _totalCGTDiscountApplied = b })

cgtTotalPriorYearLossesApplied :: Lens' (CGTAssessment a) (Money a)
cgtTotalPriorYearLossesApplied =
  lens _totalPriorYearCapitalLossesApplied (\s b -> s { _totalPriorYearCapitalLossesApplied = b })

-- | __18V__ Net capital losses carried forward to later income years
cgtNetLossesCarriedForward :: (Num a) => Getter (CGTAssessment a) (Money a)
cgtNetLossesCarriedForward = to $ \o ->
  view (capitalLossCarryForward . capitalLossCarryForwardCollectables) o
  <> view (capitalLossCarryForward . capitalLossCarryForwardOther) o

cgtGainsAndLossesByCategory :: Lens' (CGTAssessment a) (M.Map CGTScheduleCategory (Money a, Money a))
cgtGainsAndLossesByCategory =
  lens _byCategory (\s b -> s { _byCategory = b })


data CapitalLossCarryForward a = CapitalLossCarryForward
  { _carryC :: Money a -- losses on collectables
  , _carryO :: Money a -- all other losses
  }
  deriving (Eq, Functor, Show)

instance (Num a) => Semigroup (CapitalLossCarryForward a) where
  CapitalLossCarryForward lc lo <> CapitalLossCarryForward rc ro =
    CapitalLossCarryForward (lc <> rc) (lo <> ro)

instance (Num a) => Monoid (CapitalLossCarryForward a) where
  mempty = CapitalLossCarryForward mempty mempty


-- | Create a new capital loss carry-forward object with
-- all components (collectables and other) set to zero.
--
-- @
-- newCapitalLossCarryForward = mempty
-- @
--
newCapitalLossCarryForward :: (Num a) => CapitalLossCarryForward a
newCapitalLossCarryForward = mempty

capitalLossCarryForwardCollectables :: Lens' (CapitalLossCarryForward a) (Money a)
capitalLossCarryForwardCollectables = lens _carryC (\s b -> s { _carryC = b })
capitalLossCarryForwardOther :: Lens' (CapitalLossCarryForward a) (Money a)
capitalLossCarryForwardOther = lens _carryO (\s b -> s { _carryO = b })


-- | Types that have a carry-forward capital loss (either as an
-- input or an output).
class HasCapitalLossCarryForward a b where
  capitalLossCarryForward :: Lens' (a b) (CapitalLossCarryForward b)

instance HasCapitalLossCarryForward CGTAssessment a where
  capitalLossCarryForward =
    lens _capitalLossCarryForward (\s b -> s { _capitalLossCarryForward = b })
