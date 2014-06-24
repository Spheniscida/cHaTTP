
DROP ROLE IF EXISTS chattp_admin;
CREATE ROLE chattp_admin NOLOGIN PASSWORD 'chattp_default';
DROP ROLE IF EXISTS chattp;
CREATE ROLE chattp LOGIN PASSWORD 'chattp_default';

DROP DATABASE IF EXISTS chattp;
CREATE DATABASE chattp WITH OWNER = chattp_admin;

\c chattp

CREATE EXTENSION pgcrypto;

-- Source commands for tables
\i schema.sql

ALTER TABLE chattp_users OWNER TO chattp_admin;
ALTER TABLE chattp_messages OWNER TO chattp_admin;
ALTER TABLE chattp_brokers OWNER TO chattp_admin;
ALTER TABLE chattp_locations OWNER TO chattp_admin;

GRANT ALL ON chattp_users, chattp_messages, chattp_brokers, chattp_locations TO chattp;
GRANT ALL ON chattp_brokers_broker_id_seq, chattp_users_user_id_seq TO chattp;

