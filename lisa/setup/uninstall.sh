#!/bin/sh

HOSTNAME="debby_test"
DATABASE="finance"
SQLDIR="sql/"
SCRIPTV=$SQLDIR"drop_views.sql"
SCRIPTT=$SQLDIR"drop_tables.sql"
SCRIPTP01=$SQLDIR"postgrest_01_drop_readonly_access_everyone.sql"
SCRIPTP02=$SQLDIR"postgrest_02_drop_roles.sql"
SCRIPTP03=$SQLDIR"postgrest_03_drop_setup_schema.sql"
SCRIPTP04=$SQLDIR"postgrest_04_drop_http_auth.sql"
SCRIPTD=$SQLDIR"drop_database.sql"

echo "Drop.sh"
echo "-------"
echo
echo "HOSTNAME=$HOSTNAME"
echo "DATABASE=$DATABASE"
echo
echo "Uninstalling postgrest configuration..."
echo "Uninstalling $SCRIPT01..."
psql -h $HOSTNAME $DATABASE < $SCRIPTP01
echo "[Done]"
echo "Uninstalling $SCRIPT02..."
psql -h $HOSTNAME $DATABASE < $SCRIPTP02
echo "[Done]"
echo "Uninstalling $SCRIPT03..."
psql -h $HOSTNAME $DATABASE < $SCRIPTP03
echo "[Done]"
echo "Uninstalling $SCRIPT04..."
psql -h $HOSTNAME $DATABASE < $SCRIPTP04
echo "[Done]"
echo "Running $SCRIPT [OK]"
echo
echo "Dropping views..."
#psql -h $HOSTNAME $DATABASE < $SCRIPTV
echo "Running $SCRIPTV [OK]"
echo "Dropping tables..."
#psql -h $HOSTNAME $DATABASE < $SCRIPTT
echo "Running $SCRIPTT [OK]"
echo
echo "Dropping database $DATABASE"
echo "WARNING: Please execute the drop database script manually, if you really want to remove the database."
echo "You can find it at $SCRIPTD"
echo 'Done.'
