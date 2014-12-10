package com.fulcrummicro.util.cmdline;

import java.util.Iterator;
import java.util.NoSuchElementException;

import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.util.ArrayList;

import com.fulcrummicro.util.cmdline.ArgProcessingInterface;
import com.fulcrummicro.util.cmdline.CommandLineIterator;
import com.fulcrummicro.util.cmdline.PropertyArg;

/**
 * Generic way of processing commandline arguments,
 * should replace the less flexible version in com.avlsi
 *
 * @author Naru Sundar
 */
public class PropCommandLineIterator extends CommandLineIterator
                                     implements ArgProcessingInterface { 

    /* basic patterns */
    protected final Pattern propPat = Pattern.compile("--(.*?)=(.*)");
    protected final Pattern propPatDefault = Pattern.compile("--(.*)");

    public PropCommandLineIterator(ArrayList<String> args) {
        super(args);

        /* add basic patterns */
        addArgType(propPat, this);
        addArgType(propPatDefault, this);
    }


    /* argument processing interface */

    public Object processArg(String arg, 
                             CommandLineIterator cli, 
                             ArrayList<String> argList) {

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
