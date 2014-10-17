# Plugins

## Additional content-type plugins

The buddycloud server supports validation of custom content types by means of a plugin system. By default the buddycloud server supports Atom content. Additional content types can be supported by creating an appropriate validator and packaging it as a plugin.

The plugin system is not invoked if the content type is not supplied or is the default, Atom. If the plugin manager is queried then it locates an appropriate plugin based on the capabilities reported by each plugin. So we are able to directly request a plugin capable of validating a specific payload type.

### Plugin Installation

1. Create a `plugins` directory in the root of your buddycloud installation, e.g. `target/plugins/`.
2. Drop your plugin JAR into the `plugins` directory you created in step 1.
3. Restart your Buddycloud server.

### Setting the content-type for a node

To declare the content type for a node you must set the node configuration key `pubsub#type` to the appropriate validator value. For example, in the case of IODEF `urn:ietf:params:xml:ns:iodef-1.0`.
