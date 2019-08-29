CREATE ROLE notary_public;

CREATE SCHEMA notary;

CREATE TABLE notary.signups (
	id uuid PRIMARY KEY,
    created_at timestamp NOT NULL UNIQUE DEFAULT now(),
    salt text NOT NULL UNIQUE
);

GRANT INSERT ON notary.signups TO notary_public;

CREATE TABLE notary.confirmations (
    signup_id uuid NOT NULL REFERENCES notary.signups(id),
    created_at timestamp NOT NULL UNIQUE DEFAULT now(),
    address text NOT NULL,
    public_key text NOT NULL,
    confirmation_token_hash text NOT NULL,
    confirmed_at timestamp,
    disabled_at timestamp
);

GRANT INSERT ON notary.confirmations TO notary_public;
GRANT UPDATE (confirmed_at, disabled_at) ON notary.confirmations TO notary_public;