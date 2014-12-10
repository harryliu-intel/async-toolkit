package com.fulcrummicro.util.cmdline;

import java.util.Iterator;
import java.util.NoSuchElementException;

import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.util.ArrayList;

import com.fulcrummicro.util.cmdline.ArgProcessingInterface;

/**
 * Generic way of processing commandline arguments,
 * should replace the less flexible version in com.avlsi
 *
 * @author Naru Sundar
 */
public class CommandLineIterator implements Iterator {

    /* the comandline argument list */
    protected final ArrayList<String> args;

    /* index into the argument list */
    protected int argIndex = 0;

    /* list of regular expressions for arg processing */
    protected final ArrayList<Object[]> expr = new ArrayList<Object[]>();

    public CommandLineIterator(ArrayList<String> args) {
        this.args = args;
    }

    /* adds an argument processor to the list */
    public void addArgType(Pattern argExpr, ArgProcessingInterface api) {
        /* add a tuple of expression and processor */
        expr.add(new Object[] { argExpr, api });
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

    public Object next() throws NoSuchElementException,
                                IllegalArgumentException {
        if(hasNext()) {
            /* grab current argument */
            String arg = args.get(argIndex);

            /* check each pattern against the arg */
            for(int i = 0; i < expr.size(); i++) {
                Object[] tuple = (Object[]) expr.get(i);
                Pattern ae = (Pattern) tuple[0];
                ArgProcessingInterface ap = (ArgProcessingInterface) tuple[1];

                if(ae.matcher(arg).find()) {
                    argIndex++;

                    return ap.processArg(arg, this, args);
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
