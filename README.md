# Notary

## Goals

* Allow new users to sign up.
* Confirm users' email address.
* Verify arbitrary tokens and decode their claims.
* Allow users to change their password.
* Allow users to change their email (always confirming).
* Block users that misbehave calling the endpoints too many times or too often.
* Ensure all used tokens have strong algorithms and expire within parameters.
* Allow users to disable their account.
* Allow admins to disable arbitrary accounts.
* Make it more expensive to generate valid requests.

## Sign-up and Authentiction flow

1. User send JWT containing email + jwk.
1. Email is persisted alongside the jwk. Confirmation token is created and made available for email system for confirmation.
1. Email is confirmed through confirmation endpoint using the same user creation JWT to confirm the user still has access to the secret.
1. After confirmation the user JWT becomes active. Now the email becomes unique as well as the public key. The verification endpoint will return a 200 while the account is active.

## High level overview of endpoints

### User side
```
POST signup
POST confirm
PATCH email
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

* Resend confirmations is a new signup.
* Changes in password/email require access to the old email and generate a new confirmation.
* There is only one admin secret and is used by another application.

## Data structures

* signups
	* Attributes: id (uuid), created_at, client info (ip, user agent, etc).
	* What would usually be called a user, but signup here is more accurate, since all signups are stored, even the ones that are never confirmed. The idea is that signups are events and users emerge from signups+confirmations.
* confirmations
	* Attributes: `signup_id`, `created_at`, `email` (unique when confirmed), `public_key`, `confirmation_token_hash`, `confirmed_at`, `disabled_at`.

## Configuration

* `confirmation_ttl` - time confirmations can wait for the `confirm` call.
* `secret` - used as seed to create random confirmation tokens.
* `confirmation_uri` - URI to call with confirmation token (generated and never stored).
* `public_interface` - where to listen for calls from users.
* `admin_interface` - where to listen for calls from admins.
* `max_calls_per_second` - calls/second/ip allowed before throtle.
* `max_calls_per_minute` - calls/minute/ip allowed before blocking ip for `block_ips_for`.
* `block_ips_for` - hours to block ips that exceed `max_calls_per_minute`.