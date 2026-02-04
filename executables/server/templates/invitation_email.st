Hi there!

A friend of yours ($from_email$) has invited you to join the "$project_name$"
project at http://aftok.com. Here's what you need to do to accept their
invitation. At present, there's no user interface for the service other than
bare HTTP requests with a JSON payload. A couple of scripts have been provided
below that you can edit to include your own information. 

If you don't have an aftok.com account:

If you do not yet have an account on aftok.com, you can create a new account and
accept your invitation in one step. If you already have an aftok.com account that
you want to use, then then skip to the "For an existing account" section below.

To create a new account and accept the invitation, you'll need to replace
the following three tokens in the curl command below:

USER -- a username of your choosing
PASS -- the password you want to use for accessing the site
BTC_ADDR -- the bitcoin address that you want payouts to be initially sent to.
You'll be able to change this later, but because changes of payout addresses
are validated by checking transactions on the blockchain, it's important that
this be a usable address that you control.

Substitute your own values for the tokens in the script below, then run it!
Also, if you'd prefer that a different email address be associated with your
account, change that as well.

curl --header 'Content-Type: application/json' \\
     --data '{"username":"USER", "password":"PASS", "recoveryType": "email", "recoveryEmail":"$to_email$", "invitation_codes":["$inv_code$"]}' \\
     'https://aftok.com/api/register'

For an existing account:

To accept the invitation using your current aftok.com account, you'll simply need to
replace the <USER> token in the command below with your username:

curl -k -v -u <USER> -d '' https://aftok.com/api/accept_invitation?invCode=$inv_code$

