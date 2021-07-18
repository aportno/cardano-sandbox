# Updates
* To run a server with extended timeout section, run the `nix-shell` from `cardano/plutus/plutus-playground-client` and execute `plutus-playground-server -i 120s`

# Recap
In the UTXO model, in order to unlock a script address, the script attached to the address is run and that script consumes 3 pieces of input:
* datum
* redeemer
* context

# Lecture
Cardano adds a `POSIXTimeRange` field to a transaction that specifies a valid time interval for the transaction to occur.

When a transaction gets submitted to the blockchain, and validated by a node, before any scripts are run, some general checks are done:
* all inputs are present
* balances add up
* fee's included
* etc

One check that happens before validation is the **time range**. Nodes compare the current time with the **time range** specified on the transaction. If the current time does not fall within this interval than the transaction will fail immediately without ever running the validator script