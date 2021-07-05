# Auctions
Assume Alice wants to auction a non-fungible token (NFT) on Cardano. An NFT by definition exists only once. It can represent
some digital art or real world asset. The idea is that the auction is parameterized by the owner of the token and a minimal bid
is defined. If the minimum bid is not met by a specified deadline then the auction is waived.

Example, assume Alice auctions her NFT:
1. Alice creates a UTXO as the script output (where the script is the auction script)
2. The value of the UTXO is the NFT
3. The datum is nothing as of the origination of the auction but will eventually become the highest bid

As previously outlined as a feature of the EUTXO model; every transaction has a **script** and a **redeemer**. In this instance:
* The script is the Auction which contains its current value and then some datum.
* The redeemer is a bid for the NFT

Now assume Bob wants to bid 100 ada for the NFT. At this point, we now have two UTXO's that will serve as inputs to a transaction

1. Auction UTXO with no value or datum
2. Bob's bid of 100 ada UTXO (redeemer)

Bob should create this transaction using these two inputs in order to produce one output - a new Auction UTXO representing the NFT for 100 ada and then some data
that says (Bob, 100). The datum captures the highest bid and the name of the bidder. Since Bob is the sole bidder for this NFT as of now, his name will populate the 
datum.

The Auction UTXO script now contains:
1. The NFT + 100 ada
2. Datum that shows Bob and his bid

Next, let's assume Charlie wants to outbid Bob. To do so, Charlie must create a new transaction containing, similar to Bob, two UTXO's:
1. The Auction UTXO --> which contains 100 ada + NFT, and contains datum on the highest bid at the moment (from Bob)
2. Charlie's bid of 200 ada (redeemer)

Since Charlie _outbid_ Bob, then Bob should get his 100 ada back as a new UTXO. Next, Charlie should receive a UTXO that contains the Auction script
and the value that comes along with it.

To reiterate, there's two outputs (UTXO's) from Charlies transaction:
1. The NFT + 200 ada (from Charlies bid) and the datum that comes with it showing the highest bidder (Charlie, 200)
2. A UTXO to Bob for 100 ada since his bid is no longer competitive; Bob gets his money back

Assume no further bidders emerged and the deadline for the auction has been reached. Charlie is now declared the winner of the 
auction. Because the deadline was reached, the auction can be _closed_. This requires another transaction containing:
1. The Auction script and its value of 200 ada + NFT, as well as the datum of the highest bidder

Note the redeemer is now **close** instead of **bid**. The outputs for the transaction result in:
1. The NFT to the highest bidder i.e., Charlie
2. 200 ADA to the host of the auction i.e., Alice

But what happens if Alice starts an auction but doesn't receive any bids? In this case, Alice would create a transaction to close (redeemer value) the Auction script
but now because there is no highest bidder, only one output is produced from the transaction:

1. The NFT is given back to Alice

