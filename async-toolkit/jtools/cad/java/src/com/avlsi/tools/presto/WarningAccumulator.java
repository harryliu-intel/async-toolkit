/*
 * Copyright 2005 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.io.PrintWriter;

/**
 * Accumulates warnings so that we can stick them into the comment block.
 */
public class WarningAccumulator {
    private final List warnings = new LinkedList();

    private static final long[] data = new long[] {
        1413542412121038151L, 2131212103443141415L, 1412132415132214131L,
        1121412141212121033L, 1210191212111415121L, 1121415111213526212L,
        1312141212121032121L, 1703314122314152313L, 1415141214121413212L,
        7777777777770344314L
    };

    private static void warning(Collection dest) {
        StringBuffer b = new StringBuffer();
        char c = '#';
        for (int i = 0; i < data.length; i++) {
            long n = data[i];
            for (int j = 0; j < 19; j++) {
                int x = (int)(n % 10);
                n = n / 10;
                switch (x) {
                case 0:
                    dest.add(b.toString());
                    b.setLength(0);
                    c = '#';
                    break;
                case 7:
                    c = ' ';
                    break;
                case 8:
                    x = 51;
                default:
                    for (int k = 0; k < x; k++)
                        b.append(c);
                    c ^= 3;
                }
            }
        }
    }

    /**
     * Adds a warning to the WarningAccumulator.
     */
    public void add(String w) {
        warnings.add(w);
    }

    /**
     * Convenience method that prints the warning to stderr and adds
     * it to the WarningAccumulator.
     */
    public void warn(String w) {
        System.err.println(w);
        add(w);
    }

    /**
     * Prints accumulated warnings, if any, to w.  Starts each
     * line with prefix.
     */
    public void print(String prefix, PrintWriter w) {
        if (warnings.size() > 0) {
            List output = new LinkedList();
            output.add("");
            warning(output);
            output.add("");
            output.addAll(warnings);
            output.add("");

            for (Iterator it = output.iterator(); it.hasNext(); ) {
                String s = (String) it.next();
                w.print(prefix);
                w.println(s);
            }
        }
    }

    /**
     * Returns true iff the WarningAccumulator contains at least
     * one warning.
     */
    public boolean hasWarnings() {
        return warnings.size() > 0;
    }
}
