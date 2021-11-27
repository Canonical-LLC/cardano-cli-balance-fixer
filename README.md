`cardano-cli-balance-fixer` is a small utility executable for working around `cardano-cli transaction build` for testing.

Currently `build` will not balance non-Ada assets. `cardano-cli-balance-fixer change` accepts and address and transaction outputs and will output the remaining assets as a value for extra transaction output.

You can use it in scripts like so:

```bash
--tx-out "$sellerAddr + 1724100 lovelace + $(cardano-cli-balance-fixer change --address $sellerAddr --mainnet -o'1 policyId.tokenname')" \
```

It also includes several other commands that are helpful when writing integration tests using the `cardano-cli`.
