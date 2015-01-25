CREATE ROLE authenticator NOLOGIN;
GRANT USAGE ON SCHEMA "1" TO authenticator;

CREATE ROLE author NOLOGIN;
GRANT USAGE ON SCHEMA "1" TO author;
GRANT author TO authenticator;
