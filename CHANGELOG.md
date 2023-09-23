## Version NEXT

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
