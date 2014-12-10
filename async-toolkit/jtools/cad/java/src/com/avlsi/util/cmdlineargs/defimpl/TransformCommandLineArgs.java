package com.avlsi.util.cmdlineargs.defimpl;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.CommandLineArgsIterator;
import com.avlsi.util.container.StringContainerIterator;

/**
 * Allows each argument to be transformed to nothing, or 1 or more arguments.
 * Parsed arguments can only be transformed to parsed arguments; non-parsed
 * arguments can only be transformed to non-parsed arguments.
 **/
public class TransformCommandLineArgs extends CommandLineArgsBaseImpl {
    private final CommandLineArgs innerArgs;

    /**
     * Creates a new TransformCommandLineArgs.
     * @param innerArgs the commandline args to transform
     */
    public TransformCommandLineArgs(CommandLineArgs innerArgs) {
        this.innerArgs = innerArgs;
    }

    /**
     * Override to transform the given parsed argument to 0 or more parsed
     * arguments.
     **/
    protected CommandLineArg[] transform(final CommandLineArg arg) {
        return new CommandLineArg[] { arg };
    }

    /**
     * Override to transform the given non-parsed argument to 0 or more
     * non-parsed arguments.
     **/
    protected String[] transform(final String arg) {
        return new String[] { arg };
    }

    @Override
    public CommandLineArgsIterator iterator() {
        return new CommandLineArgsIterator() {
            final CommandLineArgsIterator inner = innerArgs.iterator();
            Iterator<CommandLineArg> subs =
                Collections.<CommandLineArg>emptyList().iterator();

            private void findNext() {
                while (inner.hasNext() && !subs.hasNext()) {
                    subs = Arrays.asList(transform(inner.next())).iterator();
                }
            }

            public boolean hasNext() {
                findNext();
                return subs.hasNext();
            }

            public CommandLineArg next() {
                findNext();
                return subs.next();
            }
        };
    }

    @Override
    public StringContainerIterator nonParsedArgumentsIterator() {
        return new StringContainerIterator() {
            final StringContainerIterator inner =
                innerArgs.nonParsedArgumentsIterator();
            Iterator<String> subs = Collections.<String>emptyList().iterator();

            private void findNext() {
                while (inner.hasNext() && !subs.hasNext()) {
                    subs = Arrays.asList(transform(inner.next())).iterator();
                }
            }

            public boolean hasNext() {
                findNext();
                return subs.hasNext();
            }

            public String next() {
                findNext();
                return subs.next();
            }
        };
    }
}
