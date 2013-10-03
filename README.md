# buddycloud Java Server implementation

A buddycloud server written in Java, implementing the [buddycloud channels](http://buddycloud.org/) protocol.

## Build status

Note this points to the main buddycloud repository for the java server.

[![Build Status](https://travis-ci.org/buddycloud/buddycloud-server-java.png?branch=master)](https://travis-ci.org/buddycloud/buddycloud-server-java)

## Database install

The java server purposefully uses the same database schema as the buddycloud node.js server. See here for database installation instructions https://github.com/buddycloud/buddycloud-server/tree/develop/postgres.

## Build and run

* install openjdk-6-jdk maven2
* `git clone https://github.com/buddycloud/buddycloud-server-java`
* `cd buddycloud-server-java`
* `mvn package`
* Edit configuration files as required
* Install database
* `java -jar target/channelserver-<VERSION>-jar-with-dependencies.jar

### Manually create the buddycloud server database

~~~~ {.bash}
# switch to the postgres user
sudo su - postgres
~~~~

Create a database user and assign it a password (it will not work with a
blank password); In further examples in this tutorial we are assuming
the password will be `postgres`. You will need to change that otherwise.

~~~~ {.bash}
createuser buddycloud_server --pwprompt --no-superuser --no-createdb --no-createrole
~~~~

Then just proceed as follows, entering the password you picked whenever
asked.

~~~~ {.bash}
# create the database
createdb --owner buddycloud_server --encoding UTF8 buddycloud_server

# install the schema file (and all upgrade files)
~~~~
