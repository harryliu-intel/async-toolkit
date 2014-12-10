package com.fulcrummicro.util.properties;

public interface PropReaderInterface {

    /**
     * called for every property that is read
     * from a source (commandline or file)
     */
    void handleProperty(String key, String value);
    void handleProperty(String key, String value, boolean override);

    /**
     * called whenever a property needs to be
     * evaluated for substitution
     */
    String evaluateProperty(String value) throws ExpansionErrorException;

    /**
     * called whenever a typed default is read from
     * the non config sections of a config file
     */
    void handlePropertyDefault(String key, String value, String type);
}
