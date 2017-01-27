Guiding Principle:

Trust in your collaborators is the foremost design principle. Any attempt to
inhibit abuse or fraud within a company will ultimately be circumventable, so
it's more important to provide features that will be useful to friendly actors.
Any feature that can be used to retroactively punish a malicious actor can also
be used to abuse a friendly but unpopular actor, and so should be avoided. The
correct way to exclude a malicious actor is to fork the company to exclude that
actor.

Design Guidelines:
  * Do not discard information. Mutable caches of state are fine, but
    retain all information necessary to reproduce both the current state and
    all prior states of the cache. 
  * Timestamp EVERYTHING.
  * Keep a cryptographically verified audit trail.
  * Use cryptographic signatures for authentication of all requests to secured 
    resources.

Required for launch
===================

Library
-------

  * Events
    * User-identified events
      - Associate events with user identifier as well as BTC address, and add 
        late resolution of BTC addresses at point of payout calculation.
  * User
    * Payout Address Update
      - authenticate by asking the user to sign and broadcast a small txn with a specific
        amount from the old address to the new address
  * Payouts
    * Payouts should not include events younger than <commit_delay hours> to permit amends. 
    * Find current verified address for each payout.
      * Come up with a user-friendly and reliable way to ensure that users
        don't make errors in their BTC addresses. Maybe use very small 
        confirmation transactions, as is done when establishing ACH access
        to checking accounts?
    * Include hours won in resource auction - requires confirmation that contribution - Kris wip
      was actually made (observed in the confirmed blockchain)
    * Ensure that we avoid creating dust transactions on the blockchain. Any payout fragment
      falling below the estimated cost of redemption should be instead deducted from the 
      cost to the purchaser?
  * Resource Pooling
    * Resource auction bids should include the source address which will be used in the CoinJoin txn.
    * Create election for resource acquisition designee
  * Elections
    * Create Election
      - Options to be considered
      - Closing date
    * List Proposals
    * Record Vote

Server
------

  * Authentication
    * Integrate server-session package? https://github.com/yesodweb/serversession/blob/master/README.md
      We don't really use sessions at the moment, but this will be useful once there's a UI. 
      Alternately, need to look into JWT (http://jwt.io/) to figure out whether this approach
      is relevant for us.
  * Payouts
    * Use the BIP-70 Bitcoin Payment Protocol to create payment requests.
    * Record requested payments

Scheduled event Service
-----------------------

  * Based on blockchain transactions:
    * When a resource acquisition CoinJoin is observed, record time awards
    * Read history of payments and provide reconciliation and recordkeeping
      functionality.
    * Record BTC/USD (and other currencies) exchange rate at time of transaction
      to aid in recordkeeping requirements of U.S. tax law. Since BTC is treated
      as property rather than currency, one must track the basis price in order
      to correctly report capital gains, in much the same fasion as is done for
      stock.
  * Elections
    * Close voting window
    * Tabulate votes & randomly pick winner from weighted distribution
    * Record & announce winning option
  * Resource Pooling
    * Finalize resource pooling auction
    * Create CoinJoin transaction to award BTC to the resource acquisition designee
    * Notify auction winners so that input addresses & signatures may be collected.
