package com.fulcrummicro.util.cmdline;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.regex.Pattern;

/**
 * Generic way of processing commandline arguments,
 * should replace the less flexible version in com.avlsi
 *
 * @author Naru Sundar
 */
public class CommandLineIterator<T> implements Iterator<T> {

    private class ArgType {

        public final ArgProcessingInterface<T> ap;

        public final Pattern ae;

        public ArgType(Pattern ae, ArgProcessingInterface<T> ap) {
            this.ae = ae;
            this.ap = ap;
        }

    }

    /* the comandline argument list */
    protected final List<String> args;

    /* index into the argument list */
    protected int argIndex = 0;

    /* list of regular expressions for arg processing */
    protected final List<ArgType> expr = new ArrayList<ArgType>();

    public CommandLineIterator(List<String> args) {
        this.args = args;
    }

    /* adds an argument processor to the list */
    public void addArgType(Pattern argExpr, ArgProcessingInterface<T> api) {
        /* add a tuple of expression and processor */
        expr.add(new ArgType(argExpr, api));
    }

    /* functions to modify the arg index */

    public int getIndex() { return argIndex; }

    public void setIndex(int i) throws IndexOutOfBoundsException {
        if(i >= args.size())
            throw new IndexOutOfBoundsException();
        else
            argIndex = i;
    }

    /* iterator interface functions */

    public boolean hasNext() {
        return argIndex < args.size();
    }

    public T next() throws NoSuchElementException, IllegalArgumentException {
        if (hasNext()) {
            /* grab current argument */
            String arg = args.get(argIndex);

            /* check each pattern against the arg */
            for (int i = 0; i < expr.size(); i++) {
                if (expr.get(i).ae.matcher(arg).find()) {
                    argIndex++;

                    return expr.get(i).ap.processArg(arg, this, args);
                }
            }

            /* update pointer */
            argIndex++;

            /* no matches, throw exception */
            throw new IllegalArgumentException(arg);
        }
        throw new NoSuchElementException();
    }

    /* we don't implement remove in this iterator */
    public void remove() {
        throw new UnsupportedOperationException();
    }
}
