# Configuration 

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

## Database based configuration

If you prefer to load your configuration from a database then this is possible. Simply load all your configuration key/values into the "configuration" table within the database (__note:__ **jdbc.proxool.driver-url**, **jdbc.user**, and **jdbc.password** values will be ignored). When starting the server set an evironment variable of __DATABASE__ to the Postgres connection string which will connect to your database. For example:

```
DATABASE="jdbc:postgresql://localhost:5432/buddycloud-server?user=buddycloud&password=tellnoone"
```

The server will then use the database values to configure itself, the `configuration.properties` file will be ignored.

## Node Configuration

* Title: Max length 128 characters
* Description: Max length 1024 characters