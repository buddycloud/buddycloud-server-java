package org.buddycloud.channelserver.channel.node.configuration.field;

public class Advertise extends Field {
  public static final String FIELD_NAME = "buddycloud#advertise_node";
  public static final String DEFAULT_VALUE = Advertise.models.TRUE.toString();

  public Advertise() {
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
    return (getValue().equals(Advertise.models.TRUE.toString()) || getValue().equals(
        Advertise.models.FALSE.toString()));
  }
}
