Untraceable Electronic Cash
===========================

# Introduction #

This is a student project strictly for learning purposes. This is a proof of concept implementation of the concepts presented in the following paper (MLA citation format):

> Chaum, David, Amos Fiat, and Moni Naor. "Untraceable electronic cash." Proceedings on Advances in cryptology. Springer-Verlag New York, Inc., 1990.

# Quick Casual Paper Summary #

The original research paper explains how an anonymous payment system could be implemented.

A user can withdraw a digital "coin" from their bank, which is digitally signed and authenticated by the issuing bank, can use that digital coin to pay another person or vendor without direct bank involvement, and the receiving vendor can then deposit that coin in a bank with full anonymity for the principal user. The withdrawing bank must obviously know the user that withdraws the coin, but has no means to trace how it is spent, and the depositing bank knows the user depositing the money but has no means of tracing who withdrew the coin.

Also, if the user tries to double-spend and use the same withdrawn coin twice, the users full identity is exposed.

This works by two challenge response authentication steps. One challenge response authentication at withdraw time between the withdrawing user and the bank and a second challenge response authentication at payment time between the user spending the coin and the vendor or other person receiving the coin.

If the user pays a single person with the coin and goes through the payment challenge response step just once as intended, their identity is hidden. If the user attempts to erroneously use the same coin two times through the payment challenge response system their identity is exposed.

A simple overview of how this works: The withdrawing user generates a random number and uses this to mask or blind via binary xor their user id and a transaction id. With just the blinded value or just the random blinding value, the withdrawing identity is protected. When both values are available, the withdrawing identity is exposed. The withdrawing user generates a fixed constant $n$, say $64$ number of random numbers and blinded identification numbers. During payment challenge response, the recipient generates $n$ bit string challenge that asks for either the random number of the blinded indentification. If this process is done twice, the withdrawing user will have given both random number and corresponding blinded identification value for at least a single $i$ and will be identifiable.

# How to Run #

You will need haskell stack installed. On a Mac, with the Homebrew package manager, this would be installed with `brew install haskell-stack`.

```
stack build
```

### Simple Success Scenario ##

Alice withdraws a bank certified coin, issues peer-to-peer payment with Bob, Bob deposits in the bank.

To run:

```
stack exec paydemo-exe
```

Things to note:

- You should see `got coin` to indicate that the withdrawl was successful.
- `payment success`: The payer/payee handshake challenge succeeded.
- `deposit success`: The bank validated the coin and successfully deposited it.

### Double Deposit from Same Payment Transaction ###

Here Alice withdraws a bank certified coin, issues peer-to-peer payment with Bob, Bob deposits in the bank, and then erroneously attempts to do so a second time.

```
stack exec doubledeposit-exe
```

The withdrawl + payment proceeds successfully, the first deposit succeeds. The second deposit fails, as expected, with:

```
deposit error: conflict detected: Identical challenge sequence. Likely same recipient member
```

### Double Payment of Same Coin to Two Different People or Vendors ###

Here Alice withdraws a bank certified coin, issues peer-to-peer payment with Bob, uses the same coin to erroneously issue peer-to-peer payment ot a second recipient, Charlie. Both Bob and Charlie attempt to deposit the coins, the second user will trigger the error and since they use different challenge sequences, the bank is able to recover and unmask the identity of the withdrawl.

```
stack exec doublepayment-exe
```

The error message to look for is something like this:

```
deposit (2) error: conflict detected: different challenge sequence: u:699593523,v:25, u:699593523,v:6, u:699593523,v:21, u:699593523,v:16, u:699593523,v:19, u:699593523,v:1, u:699593523,v:24, u:699593523,v:13, u:699593523,v:27
```

Notice that the `u` user id and the `v` transaction ids have been successfully unmasked and reported.
