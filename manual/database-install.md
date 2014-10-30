PostgreSQL schema
=================

# Manually create the buddycloud server database

```bash
# switch to the postgres user
sudo su - postgres
``

Create a database user and assign it a password (it will not work with a blank password):

```bash
createuser buddycloud_server --pwprompt --no-superuser --no-createdb --no-createrole
```

Then just proceed as follows, entering the password you picked whenever asked:

```bash
# create the database
createdb --owner buddycloud_server --encoding UTF8 buddycloud_server

# install the schema file (and all upgrade files)
psql -h 127.0.0.1 -U buddycloud_server -d buddycloud_server < postgres/install.sql
psql -h 127.0.0.1 -U buddycloud_server -d buddycloud_server < postgres/upgrade.1.sql
psql -h 127.0.0.1 -U buddycloud_server -d buddycloud_server < postgres/upgrade.2.sql
# repeat for all upgrade files in numerical order
```

Now we're done, but we must test that we can connect to the database and that the schema was installed appropriately:

```
psql -h 127.0.0.1 --username buddycloud_server -d buddycloud_server -c "select * from nodes;"
```

If you got an output similar to (or exactly like) this, you're good to go. 

# Upgrade instructions

If you need to upgrade the schema version after upgrading the server software,
you'll need to be a little more careful.

First **back up your DB**. The simplest way to do this is to run `pg_dump -c -U <username> <db> > backup.sql`.

Once done you can apply the files needed for your upgrade: if your DB schema is
currently version 3 and you need version 5, you will apply `upgrade.4.sql` and
`upgrade-5.sql` but not `upgrade.3.sql` and below.

```bash
psql -h 127.0.0.1 -U buddycloud_server -d buddycloud_server < postgres/upgrade.3.sql
psql -h 127.0.0.1 -U buddycloud_server -d buddycloud_server < postgres/upgrade.4.sql
# repeat for all upgrade files in numerical order
```