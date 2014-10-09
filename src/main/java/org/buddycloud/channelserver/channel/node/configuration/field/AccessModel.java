package org.buddycloud.channelserver.channel.node.configuration.field;

public class AccessModel extends Field {
    public static final String FIELD_NAME = "pubsub#access_model";
    public static final String DEFAULT_VALUE = AccessModel.models.OPEN.toString();

    public static final models open = models.OPEN;
    public static final models authorize = models.AUTHORIZE;
    public static final models whitelist = models.WHITELIST;
    public static final models local = models.LOCAL;

    public enum models {
        OPEN("open"), WHITELIST("whitelist"), AUTHORIZE("authorize"), LOCAL("local");
        String model = null;

        private models(String model) {
            this.model = model;
        }

        public String toString() {
            return model;
        }
    }

    public AccessModel() {
        name = FIELD_NAME;
    }

    public boolean isValid() {
        return (getValue().equals(AccessModel.models.OPEN.toString()) || getValue().equals(AccessModel.models.WHITELIST.toString())
                || getValue().equals(AccessModel.models.AUTHORIZE.toString()) || getValue().equals(AccessModel.models.LOCAL.toString()));
    }
}
