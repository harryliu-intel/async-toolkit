/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

public interface NodeName {
    boolean isParameterized();
    String toStringWithIndex(int index);
    String toStringWithVariable(String var);
    NodeName unparameterize(int index);
    /* note: classes that implement this interface also need to
     * implement the equals() and hashCode() methods correctly. */
}
