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

{-| Rounding functions used by ATO -}
module Data.Tax.ATO.Rounding
  ( wholeDollars
  , roundCents
  ) where

import Data.Tax

-- | Round half up
roundHalfUp :: (RealFrac a, Integral b) => a -> b
roundHalfUp x =
  let
    (n, r) = properFraction x
  in
    case signum (abs r - 0.5) of
      -1 -> n
      0 -> if r < 0 then n else n + 1
      _ -> if r < 0 then n - 1 else n + 1

-- | Discard cents
wholeDollars :: (RealFrac a) => Money a -> Money a
wholeDollars = fmap (fromInteger . truncate)

-- | Round money to the cent (half-up)
roundCents :: (RealFrac a) => Money a -> Money a
roundCents = fmap ((/ 100). fromInteger . roundHalfUp . (* 100))
