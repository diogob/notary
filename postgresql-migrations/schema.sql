CREATE EXTENSION pgcrypto;
CREATE ROLE notary_public LOGIN PASSWORD 'test';

CREATE SCHEMA notary AUTHORIZATION notary;
SET ROLE notary;

GRANT USAGE ON SCHEMA notary TO notary_public;

CREATE TABLE notary.signups (
    created_at timestamp NOT NULL UNIQUE DEFAULT now(),
	address text PRIMARY KEY,
    salt bytea NOT NULL UNIQUE
);

CREATE OR REPLACE FUNCTION notary.salt(paddress text) 
RETURNS bytea
LANGUAGE sql
VOLATILE
SECURITY DEFINER
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
    ON FUNCTION notary.salt(paddress text)
    TO notary_public;

CREATE TABLE notary.confirmations (
    created_at timestamp NOT NULL UNIQUE DEFAULT now(),
    public_key jsonb PRIMARY KEY,       
    address text NOT NULL REFERENCES notary.signups(address),
    confirmation_token_hash text NOT NULL,
    expires_at timestamp NOT NULL DEFAULT current_timestamp + '10 minutes'::interval,
    confirmed_at timestamp,
    disabled_at timestamp
);

CREATE OR REPLACE FUNCTION notary.signup(paddress text, ppublic_key jsonb) 
RETURNS text
LANGUAGE plpgsql
VOLATILE
SECURITY DEFINER
AS $$
DECLARE
    vkid text;
    vtoken bytea;
    vsalt text;
BEGIN
    vsalt := (SELECT encode(s.salt, 'base64') FROM notary.signups s WHERE s.address = paddress);
    vkid := (SELECT encode(digest(paddress || ' ' || vsalt, 'sha512'), 'hex'));
    IF coalesce(vkid <> ppublic_key->>'kid', true) THEN
        RAISE EXCEPTION '% kid error: % could not be derived from % and %', vkid, ppublic_key->>'kid', paddress, vsalt;
    END IF;

    vtoken := gen_random_bytes(16);
    INSERT INTO notary.confirmations 
        (address, public_key, confirmation_token_hash) 
    VALUES
        (paddress, ppublic_key, encode(digest(vtoken, 'sha512'), 'base64'))
    ON CONFLICT DO NOTHING;

    RETURN encode(vtoken, 'base64');
END;
$$;

GRANT EXECUTE
    ON FUNCTION notary.signup(paddress text, ppublic_key jsonb)
    TO notary_public;

CREATE OR REPLACE FUNCTION notary.jwk_for_kid(pkid text) 
RETURNS jsonb
LANGUAGE sql
VOLATILE
SECURITY DEFINER
AS $$
    SELECT c.public_key
    FROM 
        notary.signups s 
        JOIN notary.confirmations c ON c.address = s.address
    WHERE
        encode(digest(c.address || ' ' || encode(s.salt, 'base64'), 'sha512'), 'hex') = pkid
        AND c.expires_at > current_timestamp
        AND c.confirmed_at IS NULL
    ORDER BY
        c.created_at DESC
    LIMIT 1;
$$;

GRANT EXECUTE
    ON FUNCTION notary.jwk_for_kid(pkid text)
    TO notary_public;

CREATE OR REPLACE FUNCTION notary.confirm(ppublic_key jsonb) 
RETURNS timestamp
LANGUAGE sql
VOLATILE
SECURITY DEFINER
AS $$
    UPDATE notary.confirmations
    SET confirmed_at = current_timestamp
    WHERE 
        public_key = ppublic_key
        AND expires_at > current_timestamp
        AND confirmed_at IS NULL
    RETURNING confirmed_at;
$$;

GRANT EXECUTE
    ON FUNCTION notary.jwk_for_kid(pkid text)
    TO notary_public;