/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.util.container;

import java.util.Comparator;

/**
 * This class compares two objects using their natural order.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 * @review kiniry 16-23 July 2002
 **/
public final class NaturalOrderComparator implements Comparator {
    /**
     * Compares <code>o1<code> and <code>o2<code> using the natural order
     * of <code>o1<code> (<code>o1.compareTo(o2)<code>)
     *
     * @throws ClassCastException  If <code>o2</code> is not comparable
     *   to <code>o1</code> according to <code>o1</code>'s
     *   <code>compareTo</code> method.
     *
     * @bug kiniry 23 July 2002 - Key method for performance evaluation as a
     * moderate run of PrsToNet calls compare() >1.25 times and accounts for
     * approximately 19% of run-time.  The only potential optimization I can see
     * is making this method (or its refinements) final so that it can be
     * inlined better. [Class has been made final. -- jmr]
     * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1172">Bug#1172</a>
     * 
     **/
    public int compare(final Object o1, final Object o2) {
        return ((Comparable) o1).compareTo(o2);
    }

    public boolean equals(final Object o) {
        return o instanceof NaturalOrderComparator;
    }

    public boolean equals(final NaturalOrderComparator o) {
        return true;
    }

    public int hashCode() {
        return 0;
    }
}
