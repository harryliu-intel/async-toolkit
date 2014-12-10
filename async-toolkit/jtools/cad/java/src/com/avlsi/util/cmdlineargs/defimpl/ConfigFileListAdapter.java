/*
 *      ConfigFileListAdapter -
 *              make a list of config files look like a CommandLineArgs
 *
 *      Copyright 2002 Fulcrum Microsystems, Inc.  All rights reserved.
 *
 *      $Id$
 */

package com.avlsi.util.cmdlineargs.defimpl;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgsIterator;
import java.util.NoSuchElementException;
import com.avlsi.util.container.StringContainerIterator;

/**
 * This class lets you supply a list of config files that you
 * want to parse, and it will make each one look like a "--config="
 * option, so that you can then wrap this class inside a
 * CommandLineArgsWithConfigFiles.
 *
 * @author Patrick Pelletier
 */

public class ConfigFileListAdapter extends CommandLineArgsBaseImpl {
    private final String[] configFiles;

    /**
     * Creates a new ConfigFileListAdapter.
     * @param configFiles an array of config file names
     */
    public ConfigFileListAdapter(String... configFiles) {
        this.configFiles = configFiles;
    }

    /**
     * Returns an iterator that iterates over all of the config files,
     * with each CommandLineArg having a name of "config" and a value
     * of the filename.
     */
    public CommandLineArgsIterator iterator() {
        return new CommandLineArgsIterator() {
                private int i = 0;

                public boolean hasNext() {
                    return (i < configFiles.length);
                }

                public CommandLineArg next() {
                    String result;

                    try {
                        result = configFiles[i++];
                    } catch (ArrayIndexOutOfBoundsException e) {
                        throw (NoSuchElementException)
                            new NoSuchElementException().initCause(e);
                    }

                    return new CommandLineArgDefImpl("config", result);
                }
            };
    }

    /**
     * Returns an empty iterator.
     */
    public StringContainerIterator nonParsedArgumentsIterator() {
        return new StringContainerIterator() {
                public boolean hasNext() {
                    return false;
                }

                public String next() {
                    throw new NoSuchElementException();
                }
            };
    }
}
