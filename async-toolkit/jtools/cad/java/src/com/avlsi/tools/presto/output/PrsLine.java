/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

/**
 * This represents a production rule.  It is possible for the production
 * rule to be parameterized, which means that there is a rail number which
 * can be filled in (either with a constant integer, or with a variable
 * for looping) when the rule is converted to a string.
 */

public interface PrsLine {
    boolean isParameterized();
    boolean isIndented();
    String toStringWithIndex(int index);
    String toStringWithVariable(String var);
    /* note: classes that implement this interface also need to
     * implement the equals() and hashCode() methods correctly. */
}
