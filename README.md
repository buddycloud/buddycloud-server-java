# buddycloud Java Server implementation

A buddycloud server written in Java, implementing the [buddycloud channels](http://buddycloud.org/) protocol.

## Build status
 
Note this points to the main buddycloud repository for the java server.

[![Build Status](https://travis-ci.org/buddycloud/buddycloud-server-java.png?branch=master)](https://travis-ci.org/buddycloud/buddycloud-server-java) [![Coverage Status](https://img.shields.io/coveralls/buddycloud/buddycloud-server-java.svg)](https://coveralls.io/r/buddycloud/buddycloud-server-java)

## Coding Standards

Checkstyle is used to confirm the preferred coding standards are used, these are based loosely on Google's OS Java guidelines.  There is support in maven, the build should fail on the introduction of errors and there is also support for automated formatting in Eclipse.  To setup do the following -

* Navigate to Eclipse->Preferences->Java ->Code Style->Formatter
* Select 'import' and there is a file named 'eclipse_formatter.xml' in src/main/resources.
* Import and set it as the active profile

N.B. Its also worth setting up the hooks that allow for formatting and organising of imports on save, to do this 

* Navigate to Eclipse->Preferences->Java->Code Style->Save Actions
* Select 'Format All Lines' and 'Organize Imports'

## Database install

The java server purposefully uses the same database schema as the buddycloud node.js server. See here for database installation instructions https://github.com/buddycloud/buddycloud-server/tree/develop/postgres.

## Build and run

* install openjdk-6-jdk maven
* `git clone https://github.com/buddycloud/buddycloud-server-java`
* `cd buddycloud-server-java`
* `mvn package`
* Edit configuration files as required
* Install database
* `java -jar target/channelserver-<VERSION>-jar-with-dependencies.jar`

### Manually create the buddycloud server database

~~~~ {.bash}
# switch to the postgres user
sudo su - postgres
~~~~

Create a database user and assign it a password (it will not work with a blank password);
~~~~ {.bash}
createuser buddycloud_server --pwprompt --no-superuser --no-createdb --no-createrole
~~~~

Then just proceed as follows, entering the password you picked whenever asked.
~~~~ {.bash}
# create the database
createdb --owner buddycloud_server --encoding UTF8 buddycloud_server

# install the schema file (and all upgrade files)
psql -h 127.0.0.1 -U buddycloud_server -d buddycloud_server < postgres/install.sql
psql -h 127.0.0.1 -U buddycloud_server -d buddycloud_server < postgres/upgrade-1.sql
psql -h 127.0.0.1 -U buddycloud_server -d buddycloud_server < postgres/upgrade-2.sql
~~~~

Now we're done, but we must test that we can connect to the database and that the schema was installed appropriately.
~~~~ {.bash}
psql -h 127.0.0.1 --username buddycloud_server -d buddycloud_server -c "select * from nodes;"
~~~~
If you got an output similar to (or exactly like) this, you're good to go. 

## Configuration 

Please see [the example configuration file](https://github.com/buddycloud/buddycloud-server-java/blob/master/configuration.properties.example) for an example.

| Property                                  | Default value  |  Required  | Description                                   |
| ----------------------------------------- | -------------- | :--------: |---------------------------------------------- |
| xmpp.host                                 |  127.0.0.1     |            | The XMPP server host (IP address or hostname) |
| xmpp.port                                 |  5347          |            | XMPP server component port                    |
| xmpp.secretkey                            |                |      ✓     | Component secret                              |
| server.domain                             |                |      ✓     | XMPP server domain                            |
| server.domain.channels                    |                |      ✓     | Buddycloud server domain / Component address  |
| server.domain.topics                      |                |            | Topics component address                      |
| jdbc.proxool.driver-url                   |                |      ✓     | Database connection string                    |
| jdbc.user                                 |                |      ✓     | Database username                             |
| jdbc.password                             |                |      ✓     | Database password                             |
| jdbc.proxool.maximum-connection-count     |                |      ✓     | Database connection pool size                 |
| users.admin                               |                |            | Admin users (list of jids). Are sent all notifications and are able to see everything in **/firehose** |
| notifications.sendTo                      |                |            | List of JIDs to send event messages to        |
| notifications.connected                   |                |            | Send event of component connecting to XMPP server |
| channels.autosubscribe                    |                |            | A list of channels (local or remote) to which to subscribe new users. Only the base JID is required. __Note:__ channels will not be created - they must already exist
| channels.autosubscribe.autoapprove        |                |            | If any of the 'channels.autosubscribe' channels are private local channels, then whether to automatically approve the user. __Note:__ This will only work on local private channels |
| channel.configuration.default.accessmodel | open           |            | The default access model for new nodes        |
| channel.configuration.default.affiliation | member         |            | The default affiliation for new nodes         |
| channel.configuration.[**posts** or **status** or **geo.next**, etc].accessmodel | | | Override default access model on the node type |
| channel.configuration.[**posts** or **status** or **geo.next**, etc].affiliation | | | Override default affiliation on the node type |
| channel.configuration.[**posts** or **status** or **geo.next**, etc].title       | %jid%'s status  |            | Override default node title on the node type |
| channel.configuration.[**posts** or **status** or **geo.next**, etc].description | %jid%'s status  |            | Override default node description on the node type |
| discovery.dns.enabled                     | true           |            | Allow DNS discovery of other channel servers  |
| sync.purge-on-start                       | false          |            | Purge remote data on server start             |
| users.presence.persist | false | | If **true** then user presence status is stored in the database rather than in memory |

### Database based configuration

If you prefer to load your configuration from a database then this is possible. Simply load all your configuration key/values into the "configuration" table within the database (__note:__ **jdbc.proxool.driver-url**, **jdbc.user**, and **jdbc.password** values will be ignored). When starting the server set an evironment variable of __DATABASE__ to the Postgres connection string which will connect to your database. For example:

```
DATABASE="jdbc:postgresql://localhost:5432/buddycloud-server?user=buddycloud&password=tellnoone"
```

The server will then use the database values to configure itself.

## Additional content-type plugins
The buddycloud server supports validation of custom content types by means of a plugin system. By default the buddycloud server supports Atom content. Additional content types can be supported by creating an appropriate validator and packaging it as a plugin.

The plugin system is not invoked if the content type is not supplied or is the default, Atom. If the plugin manager is queried then it locates an appropriate plugin based on the capabilities reported by each plugin. So we are able to directly request a plugin capable of validating a specific payload type.

### Plugin Creation
The buddycloud plugin system is based on [JSPF](https://code.google.com/p/jspf/). Plugins are added to the main application as JAR files, dropped into the `plugins/` directory.

Creating new validator plugins is simple, there are a few simple rules to follow. Validator plugins must implement the `PayloadValidator` interface and be annotated with `@PluginImplementation`. To publish the capabilities of your plugin you must include a `capabilities()` method and annotate it with `@Override`

Two example plugins are available. One provides support for [validating IODEF](https://github.com/surevine/buddycloud-iodef-validator) payloads while the other is based on the default [Atom validator](https://github.com/surevine/buddycloud-atom-validator). Note that Atom support is built into buddycloud server so there is no need to add the Atom plugin to your installation, it merely serves as an example.

### Plugin Installation
1. Create a `plugins` directory in the root of your buddycloud installation, e.g. `target/plugins/`.
2. Drop your plugin JAR into the `plugins` directory you created in step 1.
3. Restart your Buddycloud server.

### Setting the content-type for a node
To declare the content type for a node you must set the node configuration key `pubsub#type` to the appropriate validator value. For example, in the case of IODEF `urn:ietf:params:xml:ns:iodef-1.0`.

## Multi-domain deployment

The Buddycloud server can serve multiple domains in a single instance. For this to work, the Buddycloud server must distinguish the domains it serves from remote ones, so that it can properly route packets. This is done by calling an external executable, which path can be configured in the **server.domain.checker** property. 

This executable gets no arguments and returns a comma-separated-value list of domains that are served locally. A good example of such an executable is the [external-domain-checker](https://github.com/buddycloud/hosting/blob/develop/external-domain-checker) of the hosting platform, that lists all local domains from a SQL database.

A multiple domain deployment is not expected to have the **server.domain** property on its configuration.

## Node Configuration

* Title: Max length 128 characters
* Description: Max length 1024 characters
