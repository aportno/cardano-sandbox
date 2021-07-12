# Recap

There are two sides to a smart contract:
* On-chain part
    * Allows nodes to validate transactions involving the smart contract
    * Transactions are only allowed to consume valid UTXOs
* Off-chain part
    * Lives in users wallet and constracts/submits suitable transactions


# On-chain process
EUTXO model introduces a new type of addresses. The type of addresses used in the UTXO model is simply public-key addresses. If a UTXO sits at that public-key address than a transaction is able to consume that UTXO as an input if the signature belonging to that public-key is included in the transaction

The EUTXO model extends this by adding a new type of address, called a "script" address which are able to run arbitrary logic. Transactions looking to consume UTXOs sitting at a script address are validated by a node, the node will run the script and then depending on the result of the script, decide whether the transaction is valid or not

On the script side sits some "datum" which is essentially a small slice of the state of the blockchain

There are 3 pieces of data that a plutus script gets:
1. the datum
2. the redeemer
3. the context

All 3 pieces are the same **datatype** in Haskell

```
data Data =
      Constr Integer [Data]
    | Map [(Data, Data)]
    | List [Data]
    | I Integer
    | B BS.ByteString
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
```

We can see the datatype `Data` has 5 constructors and acts as a normal algebraic datatype:
1. Constr Integer - takes an integer and recursively a list of data
2. Map - takes a list of pairs of two data items. Think of it as a lookup table with key-value pairs
3. List - takes a list of data
4. I - takes an integer
5. B - takes a byte string
# Instructions to see datatype Data
1. Ensure you `git checkout` the correct `tag` for week 2
2. Start a `nix-shell` in `cardano/plutus`
3. `cd` to `cardano/plutus-pioneer-program/code/week02`
4. run `cabal repl`
    * run `cabal update` if repl fails
5. run `import PlutusTx`
6. Enter `i: Data` to get information on the datatype `Data`
7. Enter `:set -XOverloadedStrings` which allows us to use byte strings (and the B constructor)

# Instructions to write first validator
1. Create a new Haskell module named Week02.Gift
2. Validators get 3 pieces of information all represented by the Data type
    * Datum (comes from the output)
    * Redeemer (comes from the input)
    * Context (consuming transaction)
3. Validators output/returns a `unit` or a `()` in Haskell
    * run `cabal repl` in `cardano/plutus-pioneer-program/code/week02` to test
    * load `Gift.hs` using `:l src/Week02/Gift.hs` command

```
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = () -- always passes, doesn't care what the datum, redeemer or context are

-- boiler plate
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||]) -- quote a haskell expression using the oxford brackets || i.e., result will be something like the function defined above
```
4. `{-# INLINABLE mkValidator #-}`
    * "inlinable" pragma; generally this type of pragma is used for validators of on-chain code
    * it is used on the `mkValidator` function because it is the input between the `$$(PlutusTx.compile [|| mkValidator ||])`
5. 