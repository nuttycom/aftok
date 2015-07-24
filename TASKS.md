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
  * Authentication
    * Integrate server-session package? https://github.com/yesodweb/serversession/blob/master/README.md
      We don't really use sessions at the moment, but this will be useful once there's a UI. 
      Alternately, need to look into JWT (http://jwt.io/) to figure out whether this approach
      is relevant for us.
  * Payouts
    * Previously, I had thought it would be easiest for payments to be made directly to
      a per-aftok BTC address, and a subsequent transaction used to then distribute
      that transaction to the participants. However, I now think it makes more sense to
      present the payer with a transaction to complete and sign that sends funds directly
      from their wallet to the participants, as a multiparty txn requiring signatures
      of both the aftok server (which would sign in advance) and the payer. This avoids
      the central server even momentarily having control of any funds.

Payouts Service
---------------

  * Read blockchain transactions
    * Payout Address Update validation
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
    * Secure the event log via inclusion of periodic hashes of the log
      into the public blockchain?
  * User
    * Add public keys that can be used to sign requests. How does this interact
      with certificate-based auth from browsers? Require openpgpjs?
  * Payouts
    * History of payouts (read from blockchain?)

Webserver
---------

  * Login
    * Evaluate OpenID and jwt.io
  * User Creation
    * Require user to provide the PGP public key that will be used to authenticate requests
  * Authentication
    * Require bodies of all requests to be PGP-signed; this would take the place of
      other authentication.

Payouts Service
---------------
