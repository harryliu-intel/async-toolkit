package com.fulcrummicro.util.cmdline;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Generic way of processing commandline arguments,
 * should replace the less flexible version in com.avlsi
 *
 * @author Naru Sundar
 */
public class PropCommandLineIterator extends CommandLineIterator<PropertyArg>
                                     implements ArgProcessingInterface<PropertyArg> {

    /* basic patterns */
    protected final Pattern propPat = Pattern.compile("--(.*?)=(.*)");
    protected final Pattern propPatDefault = Pattern.compile("--(.*)");

    public PropCommandLineIterator(List<String> args) {
        super(args);

        /* add basic patterns */
        addArgType(propPat, this);
        addArgType(propPatDefault, this);
    }


    /* argument processing interface */

    public PropertyArg processArg(String arg,
                                  CommandLineIterator<PropertyArg> cli,
                                  List<String> argList) {

        Matcher m = propPat.matcher(arg);
        Matcher mDef = propPatDefault.matcher(arg);

        if(m.find()) {
            return new PropertyArg(m.group(1), m.group(2));
        } else if(mDef.find()) {
            return new PropertyArg(mDef.group(1), "");
        }

        /* return nothing otherwise, should not happen */
        return null;
    }
}
