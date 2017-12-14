package com.fulcrummicro.util.cmdline;

import com.avlsi.util.cmdlineargs.CommandLineArg;

/**
 * An argument that contains a key, value pair
 *
 * @author Naru Sundar
 */
public class PropertyArg implements CommandLineArg {

    /* the pair itself */
    protected final String key;
    protected final String value;

    public PropertyArg(String key, String value) {
        this.key = key;
        this.value = value;
    }


    /* commandline arg interface */

    public String getName() { return key; }

    public String getValue() { return value; }
}
