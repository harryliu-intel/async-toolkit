/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

/**
 * This makes it more convenient to implement PrsLine, by
 * providing implementations for equals() and hashCode().
 * (These implementations work by calling toStringWithVariable(),
 * which the subclass must implement, with Unicode smiley-face
 * as the variable.  This makes the assumption that you are not
 * already using Unicode smiley-face as an array index in your
 * production rules, which is hopefully a pretty safe assumption.)
 */

public abstract class AbstractPrsLine implements PrsLine {
    public abstract boolean isParameterized();
    public abstract String toStringWithIndex(int index);
    public abstract String toStringWithVariable(String var);

    public boolean isIndented() {
        return false;
    }

    private static final String FUNNY_CHARACTER = "\u263a";

    public boolean equals(Object o) {
        if (!(o instanceof PrsLine))
            return false;
        PrsLine p = (PrsLine) o;
        String a = toStringWithVariable(FUNNY_CHARACTER);
        String b = p.toStringWithVariable(FUNNY_CHARACTER);
        return a.equals(b);
    }

    public int hashCode() {
        return toStringWithVariable(FUNNY_CHARACTER).hashCode();
    }
}
