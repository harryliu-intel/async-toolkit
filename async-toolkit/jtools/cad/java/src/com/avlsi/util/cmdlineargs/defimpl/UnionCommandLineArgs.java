/*
 *      UnionCommandLineArgs - glue together several CommandLineArgs
 *
 *      Copyright 2002 Fulcrum Microsystems, Inc.  All rights reserved.
 *
 *      $Id$
 */

package com.avlsi.util.cmdlineargs.defimpl;

import java.util.ArrayList;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.CommandLineArgsIterator;
import java.util.NoSuchElementException;
import com.avlsi.util.container.StringContainerIterator;

/**
 * This class lets you take several CommandLineArgs objects,
 * and glue them together so that they act like one big
 * CommandLineArgs.
 *
 * @author Patrick Pelletier
 */

public class UnionCommandLineArgs extends CommandLineArgsBaseImpl {
    private final CommandLineArgs[] innerArgs;

    /**
     * Creates a new UnionCommandLineArgs.
     * @param innerArgs array of CommandLineArgs to glue together, in order
     */
    public UnionCommandLineArgs(CommandLineArgs... innerArgs) {
        this.innerArgs = innerArgs;
    }

    /**
     * Returns an iterator that iterates over all of the arguments
     * in each of innerArgs, in sequence.
     */
    public CommandLineArgsIterator iterator() {
        ArrayList a = new ArrayList();

        for (int i = 0; i < innerArgs.length; i++) {
            CommandLineArgsIterator clai = innerArgs[i].iterator();
            if (clai.hasNext())
                a.add(clai);
        }

        final CommandLineArgsIterator[] innerIterators =
            (CommandLineArgsIterator[])
            a.toArray(new CommandLineArgsIterator[a.size()]);

        return new CommandLineArgsIterator() {
                private int i = 0;

                public boolean hasNext() {
                    return (i < innerIterators.length);
                }

                public CommandLineArg next() {
                    CommandLineArg result;

                    try {
                        result = innerIterators[i].next();
                    } catch (ArrayIndexOutOfBoundsException e) {
                        throw (NoSuchElementException)
                            new NoSuchElementException().initCause(e);
                    }

                    if (!innerIterators[i].hasNext())
                        i++;

                    return result;
                }
            };
    }

    /**
     * Returns an iterator that iterates over all of the non-parsed
     * arguments in each of innerArgs, in sequence.
     */
    public StringContainerIterator nonParsedArgumentsIterator() {
        ArrayList a = new ArrayList();

        for (int i = 0; i < innerArgs.length; i++) {
            StringContainerIterator sci =
                innerArgs[i].nonParsedArgumentsIterator();
            if (sci.hasNext())
                a.add(sci);
        }

        final StringContainerIterator[] innerIterators =
            (StringContainerIterator[])
            a.toArray(new StringContainerIterator[a.size()]);

        return new StringContainerIterator() {
                private int i = 0;

                public boolean hasNext() {
                    return (i < innerIterators.length);
                }

                public String next() {
                    String result;

                    try {
                        result = innerIterators[i].next();
                    } catch (ArrayIndexOutOfBoundsException e) {
                        throw (NoSuchElementException)
                            new NoSuchElementException().initCause(e);
                    }

                    if (!innerIterators[i].hasNext())
                        i++;

                    return result;
                }
            };
    }
}
