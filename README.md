# Notary 

[![CircleCI](https://circleci.com/gh/Coinberry/notary.svg?style=svg&circle-token=e4d47b8b402667233594988ac1565ddd5ca5ac8f)](https://circleci.com/gh/Coinberry/notary)

## Goals

* Allow new users to sign up without sending any secret.
* Confirm users' identity out of band.
* Allow users to sign arbitrary requests and validate them without having a shared secret.
* Allow users to change their password.
* Allow users to disable their account.
* Allow admins to disable arbitrary accounts.

## Sign-up and Authentiction flow (using email as proof of id)

1. Client send desired unique address and receive a randomly generated salt that will always be the same for that address.
1. Client derives key pair using salt and user secret, send JWT containing address, a public key and kid (derived from address + salt).
1. Email is persisted alongside the public key (which should match the kid being derived again on the server). Confirmation token is created and made available for external email system for confirmation.
1. Client signs the confirmation token and sends this new JWT
1. Email is confirmed through confirmation endpoint using confirmation JWT to ensure the Client still has access to the secret (password need to be typed when confirming email).
1. After confirmation the Client JWT becomes active. Now the email becomes unique as well as the public key. The verification endpoint will return a 200 while the account is active.

## High level overview of endpoints

### User side
```
POST salt
POST signup
POST confirm
PATCH signature
DELETE signature (mark confirmation as disabled)
```

All user side require the JWT in the POST body signed by the user. In the `PATCH signature` call the signature should use the new desired password.
This should give more work for attacker and be easy enough on the server.

### Admin side
```
GET verify (200 - user is go, claims are in the response body, 422 - invalid jwt, 404 - no confirmed signature found)
DELETE signature (mark confirmation as disabled)
```

### Caveats

* Resend confirmations is a new entry in signups relation.
* Changes in password/email require access to the old email and generate a new confirmation.
* There is only one admin secret and is used by another application.

## Configuration

* `NOTARY_DB_URI` - Database to connect to.
* `NOTARY_PUBLIC_URI` - address visible to the user during signup (stored as `aud` claim and be checked against referrer header)
* `NOTARY_PUBLIC_PORT` - where to listen for calls from users.

* `CONFIRMATION_TTL` - time confirmations can wait for the `confirm` call.
* `CONFIRMATION_URI` - URI to call with confirmation token (generated and never stored).
* `admin_interface` - where to listen for calls from admins.
* `max_calls_per_second` - calls/second/ip allowed before throtle.
* `max_calls_per_minute` - calls/minute/ip allowed before blocking ip for `block_ips_for`.
* `block_ips_for` - hours to block ips that exceed `max_calls_per_minute`.
