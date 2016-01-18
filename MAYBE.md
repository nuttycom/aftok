Maybe!
======

This is a place to document crazy ideas of things that we could
implement. It is intended to serve as a source of inspiration
to people joining the fixpoint aftok. 

Big Ideas
---------

### Plan for merges as well as forks.

It's fully to be expected that some aftoks will splinter. But it's
equally possible that separate aftoks might want to join forces! 
The payout algorithm could take into account independent project 
histories in a way that allows payments to be allocated fairly 
irrespective of how projects of split and recombined.

### Build an integrated hosting platform.

The idea here is to build something like Heroku, or a Docker hosting
service, with additional support for users to make things like 
subscription-based services trivial to build. Hosted, secured account
management seems like something really useful for people building 
new applications.

Smaller Ideas
-------------

### Library Features

  * Timeline
    * Secure the event log via inclusion of periodic hashes of the log
      into a public blockchain?
  * User
    * Add public keys that can be used to sign requests. How does this interact
      with certificate-based auth from browsers? Require openpgpjs?

### Webapp / API Features

  * Login
    * Evaluate OpenID and jwt.io
  * User Creation
    * Require user to provide the PGP public key that will be used to authenticate requests
  * Authentication
    * Require bodies of all requests to be PGP-signed; this would take the place of
      other authentication.
