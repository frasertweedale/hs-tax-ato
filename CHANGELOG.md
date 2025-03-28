## Version NEXT

- New module `Data.Tax.ATO.Depreciation` provides types and
  functions for calculating decline in value (for deductions).
  Use the `DepreciatingAsset` data type to define depreciating
  assets, and the `declineInValue` function for calculating the
  decline in value in a given financial year.

- Added some new helper functions to the `Data.Tax.ATO.FY` module:
  - `fromProxy :: (FinancialYear y) => Proxy y -> Year`
  - `daysInYear :: Year -> Integer`
  - `daysInYearFromProxy :: FinancialYear y => Proxy y -> Integer`

- Implement Medicare levy low income family thresholds, including
  increase based on number of dependents.  Senior and pensioners
  individual and family thresholds remain unimplemented.

- Fix FY2024 private health insurance rebate tier thresholds.

- Add `FY2025` module.  All rates and thresholds are up to date.


## Version 2024.1

- The minimum supported version of GHC is now 9.2, due to a
  dependency on more recent versions of the *time* library.

- Update the `Dividend` type to use store gross payment amount and
  tax withheld, rather than net amount, franking portion and (other)
  tax withheld.  Add new helper functions for construction:
  - `dividendFromGross` takes gross amount and tax withheld
  - `dividendFromNet` takes net amount and tax withheld
  - `dividendFromNetFranked` takes net amount, franked proportion and
    applicable corporate tax rate for working out the franking credit.
  - `dividendFromNetFranked30` is a shortcut that uses the standard
    corporate tax rate of 30%.

- Change the type of `dividendDate` field from `String` to
  `Data.Time.Day`.

- Rename the `HasIncome` class to `HasTaxableIncome`, and its
  member function `income` to `taxableIncome`.

- Add functions for getting the `Day` range of a financial year,
  and a function for getting the financial year of a given day.

- Move the `Data.Tax.ATO.Days` module to `Data.Tax.ATO.FY` and
  rename the `DaysInYear` type synonym to `FinancialYear`, to
  reflect the additional behaviour.

- Each `FY.<YYYY>` module now exports an `FY` type synonym for its
  type-level `Nat` representing the financial year, and the `fyProxy
  :: Proxy FY` value.

- Add `Functor` instance for `CGTEvent`.

- Add `FY2024` module.

- Add `Data.Tax.ATO.Pretty` module, which provides pretty printers
  for `TaxReturnInfo`, `TaxAssessment`, and other data types.


## Version 2023.2

- Add support for PAYG Instalments, which are specified in aggregate
  as a refundable tax offset via the `paygInstalments` field.

- Add the `Deductions` type, which expresses the various deduction
  types, as well as the (aggregate) amount of deductions related to
  foreign income.

- Implement the Foreign Income Tax Offset Limit.  The limit will be
  calculated and the `foreignTaxOffset` field in the `TaxReturnInfo`
  will be clamped to it.

## Older versions

See Git commit history
