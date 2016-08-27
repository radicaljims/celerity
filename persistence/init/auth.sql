-- most of this is copypasta from the postgrest doc
-- see: http://postgrest.com/examples/users/

-- crypto stuff
create extension if not exists pgcrypto;
create schema if not exists basic_auth;

CREATE ROLE authenticator NOINHERIT LOGIN;
CREATE ROLE anon;
-- note: valhalla role created elsewhere!

GRANT anon, valhalla TO authenticator;
grant usage on schema public, basic_auth to anon;
