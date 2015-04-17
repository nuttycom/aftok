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

  * Invite
    * When a new participant is invited to the project, allow them to create an account.
      - user creates account ands provides initial payout address
      - inviting user asked to sign a txn that transfers a specific amount of btc from their
        current payout address to the invitee's payout address as confirmation of
        the invitation + script
#  * Timeline
#    * Amend Event
#      * Amend operations targeting events older than <commit_delay hours> fail.

  * User
    * Payout Address Update
      - authenticate by asking the user to sign and broadcast a small txn with a specific
        amount from the old address to the new address
  * Payouts
    * Payouts should not include events younger than <commit_delay hours> to permit amends.
    * Find current verified address for each payout.
    * Include hours won in resource auction - requires confirmation that contribution
      was actually made (observed in the confirmed blockchain)
  * Resource Pooling
    * Resource auction bids should include the source address which will be used in the CoinJoin txn.
    * Create election for resource acquisition designee
  * Elections
    * Create Election
      - Options to be considered
      - Closing date
    * List Proposals
    * Record Vote

Webserver
---------
  * Timeline
    * Amend Event

Payouts Service
---------------

  * Read blockchain transactions
    * Invitation validation
    * Payout Address Update validation
    * When a payment is observed, distribute it to participants.
    * When a resource acquisition CoinJoin is observed, record time awards
  * Elections
    * Close voting window
    * Tabulate votes & randomly pick winner from weighted distribution
    * Record & announce winning option
  * Resource Pooling
    * Finalize resource pooling auction
    * Create CoinJoin transaction to award BTC to the resource acquisition designee
    * Notify auction winners so that input addresses & signatures may be collected.

Future Work
===========

Library
-------

  * Timeline
    * Amend Event
      * MAYBE garnish/reimburse based approach? 
    * Secure the transaction log via inclusion of periodic hashes of the log
      into the public blockchain?
  * User
    * Add public keys that can be used to sign requests. How does this interact
      with certificate-based auth from browsers? Require openpgpjs?
  * Payouts
    * History of payouts (read from blockchain?)

Webserver
---------

  * Login
    * Evaluate OpenID options
  * Companion Creation
    * Require user to provide the PGP public key that will be used to authenticate requests
  * Authentication
    * Require bodies of all requests to be PGP-signed; this will take the place of
      other authentication.

Payouts Service
---------------
