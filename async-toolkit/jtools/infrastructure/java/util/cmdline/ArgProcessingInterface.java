package com.fulcrummicro.util.cmdline;

import java.util.ArrayList;

public interface ArgProcessingInterface {

    /**
     * called for every arg that matches the associated
     * regular expression, returns some object; the function
     * may choose to move the argptr within the iterator 
     * if it is processing multiple arguments out of the list
     */
    Object processArg(String arg, CommandLineIterator cli,
        ArrayList<String> argList);
}
