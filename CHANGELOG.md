## Version NEXT

### Features and enhancements

- Overhauled payment summaries.  More comprehensive data types and
  optics are provided in module `Data.Tax.ATO.PaymentSummary`.

  The old `PaymentSummary` constructor persists as a (deprecated)
  pattern synonym, for backwards compatibility.  It will be removed
  in a future release.

- Implemented **reportable fringe benefits**. .  Use the
  `fringeBenefitsEmployerNotExempt` or
  `fringeBenefitsEmployerExempt` constructors, along with the
  `reportableFringeBenefits` lens to add it to the payment summary.
  The amount is properly included in the surcharge and student loan
  repayment income calculations.

- Implemented **allowances**.  A list of allowances can be added to
  a payment summary via the `allowances` lens.  Construct allowance
  values with `allowance :: String -> Money a -> Allowance a`.

- Implemented **non-super lump sum A, B, D and E** payments.

- Implemented additional payment summary types:
  - Foreign Employment
  - Business and personal services income
  - Withholding where ABN not quoted

- New module `Data.Tax.ATO.Depreciation` provides types and
  functions for calculating decline in value (for deductions).
  Use the `DepreciatingAsset` data type to define depreciating
  assets, and the `declineInValue` function for calculating the
  decline in value in a given financial year.

- Implement Medicare levy low income family thresholds, including
  increase based on number of dependents.  Senior and pensioners
  individual and family thresholds remain unimplemented.

- CGT assessment types were overhauled and now contain more
  information about total capital gain and applied / unapplied
  losses.  This is step toward a more complete CGT implementation.
  Remaining gaps include foreign withholding credits and separate
  treatment of collectables.

- Multiple ESS statements can now be added to a tax return.  Also,
  the `essEmployeeDetails` field has been added, and
  `newESSStatement` was updated to take a `PayerDetails`.  This is a
  breaking change.  ([#16])

- Added `division293Income` which calculates the income component of
  Division 293 tax calculations.  Some elements are not included
  because the relevant feature is not yet implemented (e.g. rental
  losses or FHSS releases).  ([#12])

- Added `paygInstalmentIncome`, which calculates the PAYG instalment
  income.  Some elements are not included because the relevant
  features are not yet implement (e.g. rent, partnership and trust
  income, PSI).

- Added the `Data.Tax.ATO.ABN` and `Data.Tax.ATO.TFN` modules, which
  have more representative data types, conversion functions, and
  proper validation.  `IsString` instances are provided for
  compatibility with existing code.

- `TaxReturnInfo` pretty printing now includes more detailed ESS and
  CGT data, to help with preparation of tax returns.

- `TaxAssessment` pretty printing now separately reports Medicare
  levy, Medicare levy surcharge, and the various non-refundable and
  refundable tax offsets.  The formatting closely resembles the ATO
  *notice of assessment* layout.

- Add `FY2025` module.  All rates and thresholds are up to date.

- Added some new helper functions to the `Data.Tax.ATO.FY` module:
  - `fromProxy :: (FinancialYear y) => Proxy y -> Year`
  - `daysInYear :: Year -> Integer`
  - `daysInYearFromProxy :: FinancialYear y => Proxy y -> Integer`

### Bug fixes

- Non-refundable offsets now cannot reduce liability below $0, and
  the order of application of the various liabilities, offsets and
  credits has been fixed.

- Employee share scheme TFN amounts withheld are now included in
  assessment calculations ([#8]).

- Fix FY2024 private health insurance rebate tier thresholds.

[#8]: https://github.com/frasertweedale/hs-tax-ato/issues/8
[#12]: https://github.com/frasertweedale/hs-tax-ato/issues/12
[#16]: https://github.com/frasertweedale/hs-tax-ato/issues/16


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
