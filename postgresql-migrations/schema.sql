CREATE EXTENSION pgcrypto;
CREATE ROLE notary_public;
CREATE ROLE notary;

CREATE SCHEMA notary AUTHORIZATION notary;
SET ROLE notary;

CREATE TABLE notary.signups (
    created_at timestamp NOT NULL UNIQUE DEFAULT now(),
	address text PRIMARY KEY,
    salt bytea NOT NULL UNIQUE
);

CREATE OR REPLACE FUNCTION notary.signup(paddress text) 
RETURNS bytea
LANGUAGE sql
VOLATILE
AS $$
WITH 
first AS (
    INSERT INTO notary.signups (address, salt) VALUES (paddress, gen_random_bytes(16)) ON CONFLICT DO NOTHING RETURNING salt
)
SELECT coalesce(
    (SELECT f.salt FROM first f), 
    (SELECT salt FROM notary.signups s WHERE s.address = paddress)
);
$$;

GRANT EXECUTE
    ON FUNCTION notary.signup(paddress text)
    TO notary_public;

CREATE TABLE notary.confirmations (
    created_at timestamp NOT NULL UNIQUE DEFAULT now(),
    address text NOT NULL REFERENCES notary.signups(address),
    public_key text NOT NULL,
    confirmation_token_hash text NOT NULL,
    confirmed_at timestamp,
    disabled_at timestamp
);

GRANT INSERT ON notary.confirmations TO notary_public;
GRANT UPDATE (confirmed_at, disabled_at) ON notary.confirmations TO notary_public;