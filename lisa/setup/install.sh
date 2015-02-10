#!/bin/sh
#
# Note: The user that executes this file, needs permission to create tables,
# views, roles and grant permissions.

HOSTNAME="debby_test"
DATABASE_SETUP="pgsql"
DATABASE="finance"
SQLDIR="sql/"

echo "Create.sh"
echo "---------"
echo
echo "HOSTNAME=$HOSTNAME"
echo "DATABASE=$DATABASE"
echo "Create database $DATABASE"
FILE=$SQLDIR"create_database.sql"
echo "Running $FILE"
psql -h $HOSTNAME $DATABASE_SETUP < $FILE
echo "[Done]"
echo "Install postgrest configuration..."
FILE=$SQLDIR"postgrest_01_create_http_auth.sql"
echo "Running $FILE"
psql -h $HOSTNAME $DATABASE < $FILE
echo "[Done]"
FILE=$SQLDIR"postgrest_02_create_setup_schema.sql"
echo "Running $FILE"
psql -h $HOSTNAME $DATABASE < $FILE
echo "[Done]"
FILE=$SQLDIR"postgrest_03_create_roles.sql"
echo "Running $FILE"
#psql -h $HOSTNAME $DATABASE < $FILE
echo "[Done]"
FILE=$SQLDIR"postgrest_04_create_readonly_access_everyone.sql"
echo "Running $FILE"
#psql -h $HOSTNAME $DATABASE < $FILE
echo "[Done]"
echo
echo "Creating tables..."
FILE=$SQLDIR"create_tables.sql"
#psql -h $HOSTNAME $DATABASE < $FILE
echo "Running $FILE [OK]"
echo
echo "Creating views..."
FILE=$SQLDIR"create_views.sql"
#psql -h $HOSTNAME $DATABASE < $FILE
echo "Running $FILE [OK]"
echo
echo "Creating extra views..."
FILE=$SQLDIR"create_views_extra.sql"
#psql -h $HOSTNAME $DATABASE < $FILE
echo "Running $FILE [OK]"
echo
echo "Filling tables..."
FILE=$SQLDIR"init_tables.sql"
#psql -h $HOSTNAME $DATABASE < $FILE
echo "Running $FILE [OK]"
echo
echo "Filling tables with extra values..."
FILE=$SQLDIR"init_tables_extra.sql"
#psql -h $HOSTNAME $DATABASE < $FILE
echo "Running $FILE [OK]"
echo
echo "Done."
