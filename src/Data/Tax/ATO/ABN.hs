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

Australian Business Number (ABN) types and functions.

@
import qualified Data.Tax.ATO.ABN as ABN

main = do
  let
    abnStr = "53 004 085 616"
    Just abn = ABN.parse abnStr
    abnInt = ABN.asInt abn
    Just abn' = ABN.fromInt abnInt
  putStrLn $ ABN.asString abn'  -- prints "53 004 085 616"
@

It is recommended to import this module qualified.

You can use the @OverloadedStrings@ language extension or the 'parse' function
to convert string to ABNs.  Space characters are ignored, but other non-digits
will cause parse failure.

-}

module Data.Tax.ATO.ABN
  (
    ABN
  , parse
  , fromInt
  , asString
  , asInt
  ) where

import Data.Char (isDigit)
import Data.String
import Data.Maybe (fromJust)

-- | Australian Business Number (ABN)
newtype ABN = ABN Int
  deriving (Eq, Ord, Show)

-- | Parse an ABN, throwing an error if it is not valid.
instance IsString ABN where
  fromString = fromJust . parse

-- | Construct an ABN, checking validity.
fromInt :: Int -> Maybe ABN
fromInt n | valid n = Just (ABN n)
fromInt _           = Nothing

-- | Parse an ABN.  Spaces characters are allowed, other non-digit
-- characters are not.
parse :: String -> Maybe ABN
parse s
  | all isDigit s'  = fromInt (read s')
  | otherwise       = Nothing
  where s' = filter (/= ' ') s

valid :: Int -> Bool
valid n =
  (== 0) . (`mod` 89) . subtract 10 . snd $ foldr step (n, 0) weights
  where
    weights = [10,1,3,5,7,9,11,13,15,17,19]
    step weight (n',acc) = ((+ acc) . (* weight)) <$> divMod n' 10

asInt :: ABN -> Int
asInt (ABN n) = n

-- Format ABN in the standard way (XX XXX XXX XXX)
asString :: ABN -> String
asString (ABN n) =
  case s' of
    [a1,a2,b1,b2,b3,c1,c2,c3,d1,d2,d3]  -> [a1,a2,' ',b1,b2,b3,' ',c1,c2,c3,' ',d1,d2,d3]
    _                                   -> show n -- can't happen, but just in case...
  where
    n' = abs n  -- negative can't happen, but just in case...
    s = show n'
    s' = replicate (11 - length s) '0' <> s  -- left-pad to 11 digits
