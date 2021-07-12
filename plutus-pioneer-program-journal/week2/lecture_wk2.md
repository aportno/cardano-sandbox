# Prior Week Recap

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

# Gift Simulator
1. Start a local playground-server
2. Copy/paste `Gift.hs` code found in this directory
3. `Compile` the code and then click `Simulate`
4. Add another wallet
5. Give each 10m Lovelace = 10 Ada
6. Invoke the `give` endpoint for `wallet 1` and `wallet 2`
7. Wait for 1 block
8. Inboke the `grab` endpoint for `wallet 3`
9. Wait for 1 block
10. Click `evalulate` in the top right right corner of the playground
    * Slot 0, Tx 0 = genesis transaction that distributes the initial funds (10 ada to wallets 1, 2, and 3)
    * Slot 1, Tx 0 and 1 = these two are the `give` transactions. There was no wait between the transactions hence they are in the same slot
    * Slot 2, Tx 0 = this is where the `grab` transaction occurs. The output fee is higher than Slot 1 because the off-chain code scans the blockchain for UTXO's sitting at this script address and found the two we had created earlier i.e., the actual script is being run
11. We can see this **Gift smart contract** allows participants to **donate** or **give** ada amounts while another wallet can then **grab** the amounts

# Burn Simulator
1. Start a local playground-server
2. Copy/paste `Burn.hs` code found in this directory to the playground editor
3. `Compile` the code and then click `Simulate`
4. Use the same sequence of actions as the `Gift Simulator`
5. After `evaluate`, you will notice the `grab` transaction failed, while the `give` transactions were succesful.
    * The purpose in adding the error term was to show that the validator will never be able to retrieve funds sent to that specific script address
6. We can see this **Burn contract** allows ada to be spent to it, but at no point is anyone able to retrieve the ada using **grab**

# FortyTwo Simulator
1. This validator will expect a specific **redeemer**
2. Copy/paste the `FortyTwo.hs` code found in this directory to the playground editor
3. You will see the mkValidator function is changed 

From:
```
{-# INLINABLE mkValidator #-}
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = ()
```

To:
```
{-# INLINABLE mkValidator #-}
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ r _
    | r == I 42 = ()
    | otherwise = traceError "wrong redeemer"
```
4. We want are our function to expect a specific **redeemer** and note our mkValidator function expects 3 Data datatypes
    * Datum (d)
    * Redeemer (r)
    * Context (c)
5. Hence, we don't care about the datum or context inputs, only the redeemer at this point
    * so the function will only care about the redeemer (r) input
6. The functions says, if r is equal to I 42 then return a unit
    * Note, the use of the | is called a **guard** in Haskell
7. Otherwise, a traceError occurs (i.e., the transaction will fail)

# Typed Simulator
1. Similar to above, however we change our validator to:
```
{-# INLINABLE mkValidator #-}
mkValidator :: () -> Integer -> ScriptContext -> Bool
mkValidator _ r _ = traceIfFalse "wrong redeemer" $ r == 42
```
2. This will produce a similar result to the `Burn.hs` smart contract if the redeemer does not requal 42, otherwise the transaction will evaluate to true

# Notes
* If you fail to compile, make sure `{-# LANGUAGE OverloadedStrings #-}` is declared