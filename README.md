# forex2ledger

This tool prints currency exchange rates in [plain text account
format](https://plaintextaccounting.org/), e.g.;

```bash
$ stack run -- --config_file config-sample.toml
P 2021-03-21 USD 0.929777 CHF
P 2021-03-21 USD 0.836925 EUR
```

## Installation

Run `stack install`. This command installs the `forex2ledger` binary in your
`PATH`.

## Usage

You will need a valid app ID from
[Open Exchange Rates](http://openexchangerates.org).

1. Add a valid app ID to `config-sample.toml`.
2. Run `forex2ledger --config_file config-sample.toml`.

### Sample config file

An example config file looks like this:

```TOML
# config-sample.toml
# The AppId for https://openexchangerates.org/
oer_app_id = 'SAMPLEAPPID'
currency.base = 'USD'
currency.targets = ['CHF', 'EUR']
```
