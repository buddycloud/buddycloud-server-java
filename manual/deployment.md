# Deployment

__Note:__ Always ensure that you have a database/XMPP server set up and running first.

## Running manually

```
java -jar channelserver-<VERSION>-jar-with-dependencies.jar
```

Configuration to be set as per [configuration page](https://github.com/buddycloud/buddycloud-server-java/blob/master/manual/configuration.md).

## Docker

```
docker run -d buddycloud/channel-server
```

Configuration can be provided by mounting a data volume or via a database environment variable. See the [configuration page](https://github.com/buddycloud/buddycloud-server-java/blob/master/manual/configuration.md) for database instructions.

### Mounted volume configuration

'Mounted volume' configuration works the same as including a `configuration.properties` file. In this case you need to mount your configuration directory at `/config/channel-server` on the docker image.  The channel server is set up to check this directory for config files.

## Multi-domain deployment

The Buddycloud server can serve multiple domains in a single instance. For this to work, the Buddycloud server must distinguish the domains it serves from remote ones, so that it can properly route packets. This is done by calling an external executable, which path can be configured in the **server.domain.checker** property. 

This executable gets no arguments and returns a comma-separated-value list of domains that are served locally. A good example of such an executable is the [external-domain-checker](https://github.com/buddycloud/hosting/blob/develop/external-domain-checker) of the hosting platform, that lists all local domains from a SQL database.

A multiple domain deployment is not expected to have the **server.domain** property on its configuration.
