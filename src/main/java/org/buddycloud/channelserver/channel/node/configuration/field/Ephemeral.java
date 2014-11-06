package org.buddycloud.channelserver.channel.node.configuration.field;

public class Ephemeral extends Field {
  public static final String FIELD_NAME = "buddycloud#ephemeral";
  public static final String DEFAULT_VALUE = Ephemeral.models.FALSE.toString();

  public Ephemeral() {
    name = FIELD_NAME;
  }

  public enum models {
    TRUE("true"), FALSE("false");
    String model = null;

    private models(String model) {
      this.model = model;
    }

    public String toString() {
      return model;
    }
  }

  public boolean isValid() {
    return (getValue().equals(Ephemeral.models.TRUE.toString()) || getValue().equals(
        Ephemeral.models.FALSE.toString()));
  }
}
