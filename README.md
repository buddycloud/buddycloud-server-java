# buddycloud Java Server implementation

Note: This server is not as fully featured as the nodejs server https://github.com/buddycloud/buddycloud-server.

Not yet implemented:

- Special nodes
- MAM/Sync (not caching so no need so far)

Currently the server has enough features that it is usable. It federates with remote servers but may appear to be slow due to lack of caching.

## Build status

Note this points to the main buddycloud repository for the java server.

[![Build Status](https://travis-ci.org/buddycloud/buddycloud-server-java.png?branch=master)](https://travis-ci.org/buddycloud/buddycloud-server-java)

## Database install

The java server purposefully uses the same database schema as the buddycloud node.js server. See here for database installation instructions https://github.com/buddycloud/buddycloud-server-java/tree/develop/postgres.

## Build and run

* `git clone https://github.com/buddycloud/buddycloud-server-java`
* `cd buddycloud-server-java`
* `mvn package`
* Edit configuration files as required
* Install database
* `java -jar target/channelserver-<VERSION>-jar-with-dependencies.jar
