/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.util.rename;


import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;
import com.avlsi.file.cdl.util.rename.IdentityNameInterface;

/**
 * A name interface to hack around an LVS issue with biased GDS2 at PMC.  This
 * appends a constant string at the end of a cell name, which makes them look
 * different than the cell names in GDS2, thus forcing LVS to flatten the
 * entire hierarchy.
 **/
public class PMCHackNameInterface {
    private static Impl forward, reverse;

    public static class Impl extends IdentityNameInterface {
        private static final String SUFFIX = "_PMC_";
        // direction of the mapping
        private boolean forward;
        public Impl(final boolean forward) {
            this.forward = forward;
        }

        public String renameCell(final String oldCellName)
            throws CDLRenameException {
            if (forward) {
                return oldCellName + SUFFIX;
            } else {
                if (oldCellName.endsWith(SUFFIX)) {
                    final int l = oldCellName.length();
                    return oldCellName.substring(0, l - SUFFIX.length());
                } else {
                    throw new CDLRenameException("Cell name: " + oldCellName +
                                                 " does not end in " + SUFFIX);
                }
            }
        }
    }

    /**
     * This class cannot be constructed.  Use {@link getForwardNamer} and
     * {@link getReverseNamer} to get an instance that does the forward and
     * reverse mapping respectively.
     **/
    private PMCHackNameInterface() { }

    public static CDLNameInterface getForwardNamer() {
        if (forward == null) forward = new Impl(true);
        return forward;
    }

    public static CDLNameInterface getReverseNamer() {
        if (reverse == null) reverse = new Impl(false);
        return reverse;
    }
}
