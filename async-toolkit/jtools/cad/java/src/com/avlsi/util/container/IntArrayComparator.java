/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.util.container;

import java.util.Comparator;

public class IntArrayComparator implements Comparator {
    public int compare(Object o1, Object o2) {
        int[] a1 = (int[]) o1;
        int[] a2 = (int[]) o2;
        int c = a1.length - a2.length;
        for (int i = 0; c == 0 && i < a1.length; i++)
            c = a1[i] - a2[i];
        return c;
    }
}
