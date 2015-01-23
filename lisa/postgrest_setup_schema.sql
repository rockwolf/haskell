CREATE SCHEMA “1”;
-- TODO: set the schema for all tables in finance
ALTER TABLE t_trade SET SCHEMA “1”;
ALTER TABLE t_rate SET SCHEMA “1”;
...
-- Note: the alter statements can be left out, if the initial setup contains:
-- create table "1".t_trade(...) etc.
