# Developing

## Build and run

* install openjdk-7-jdk maven
* `git clone https://github.com/buddycloud/buddycloud-server-java`
* `cd buddycloud-server-java`
* `mvn package`
* Edit configuration files as required
* Install database
* `java -jar target/channelserver-<VERSION>-jar-with-dependencies.jar`

## Testing

```bash
mvn test
```

Test reports are stored in __target/surefire-reports__.

## Coding Standards

Checkstyle is used to confirm the preferred coding standards are used, these are based loosely on Google's OS Java guidelines.  There is support in maven, the build should fail on the introduction of errors and there is also support for automated formatting in Eclipse.  To setup do the following -

* Navigate to Eclipse ↝ Preferences ↝ Java ↝Code Style ↝ Formatter
* Select 'import' and there is a file named `eclipse_formatter.xml` in ++src/main/resources++.
* Import and set it as the active profile

N.B. Its also worth setting up the hooks that allow for formatting and organising of imports on save, to do this 

* Navigate to Eclipse ↝ Preferences ↝ Java ↝ Code Style ↝ Save Actions
* Select 'Format All Lines' and 'Organize Imports'