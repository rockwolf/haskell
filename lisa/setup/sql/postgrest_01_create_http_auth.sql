CREATE SCHEMA postgrest;
SET search_path = postgrest, pg_catalog;

CREATE TABLE postgrest.auth (
  id character varying NOT NULL,
  rolename name NOT NULL,
  pass character(60) NOT NULL,
  CONSTRAINT auth_pkey PRIMARY KEY (id)
) WITH ( OIDS=FALSE );

CREATE FUNCTION check_role_exists() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin 
if not exists (select 1 from pg_roles as r where r.rolename = new.rolename) then
   raise foreign_key_violation using message = 'Cannot create user with unknown role: ' || new.rolename;
   return null;
 end if;
 return new;
end
$$;

CREATE CONSTRAINT TRIGGER ensure_auth_role_exists
  AFTER INSERT OR UPDATE
  ON postgrest.auth
  FOR EACH ROW
  EXECUTE PROCEDURE postgrest.check_role_exists();
