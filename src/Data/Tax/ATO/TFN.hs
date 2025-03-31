-- This file is part of hs-tax-ato
-- Copyright (C) 2025  Fraser Tweedale
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

Tax File Number (TFN) types and functions.

@
import qualified Data.Tax.ATO.TFN as TFN

main = do
  let
    abnStr = "53 004 085 616"
    Just abn = TFN.parse abnStr
    abnInt = TFN.asInt abn
    Just abn' = TFN.fromInt abnInt
  putStrLn $ TFN.asString abn'  -- prints "53 004 085 616"
@

It is recommended to import this module qualified.

You can use the @OverloadedStrings@ language extension or the 'parse' function
to convert string to TFNs.  Space characters are ignored, but other non-digits
will cause parse failure.

-}

module Data.Tax.ATO.TFN
  (
    TFN
  , parse
  , fromInt
  , asString
  , asInt
  ) where

import Data.Char (isDigit)
import Data.String
import Data.Maybe (fromJust)

-- | Tax File Number (TFN)
newtype TFN = TFN Int
  deriving (Eq, Ord, Show)

-- | Parse a TFN, throwing an error if it is not valid.
instance IsString TFN where
  fromString = fromJust . parse

-- | Construct a TFN, checking validity.
fromInt :: Int -> Maybe TFN
fromInt n | valid n = Just (TFN n)
fromInt _           = Nothing

-- | Parse a TFN.  Spaces characters are allowed, other non-digit
-- characters are not.
parse :: String -> Maybe TFN
parse s
  | all isDigit s'  = fromInt (read s')
  | otherwise       = Nothing
  where s' = filter (/= ' ') s

valid :: Int -> Bool
valid n =
  (== 0) . (`mod` 11) . snd $ foldr step (n, 0) weights
  where
    weights = [1,4,3,7,5,8,6,9,10]
    step weight (n',acc) = ((+ acc) . (* weight)) <$> divMod n' 10

asInt :: TFN -> Int
asInt (TFN n) = n

-- Format TFN in the standard way (XXX XXX XXX)
asString :: TFN -> String
asString (TFN n) =
  case s' of
    [a1,a2,a3,b1,b2,b3,c1,c2,c3]  -> [a1,a2,a3,' ',b1,b2,b3,' ',c1,c2,c3]
    _                             -> show n -- can't happen, but just in case...
  where
    n' = abs n  -- negative can't happen, but just in case...
    s = show n'
    s' = replicate (9 - length s) '0' <> s  -- left-pad to 9 digits
