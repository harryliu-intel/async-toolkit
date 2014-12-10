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

package com.avlsi.cell;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import com.avlsi.file.common.HierName;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;

/**
 * Class to represent exclhi / excllo specifications.  If
 * <code>hiLo == HI</code>, then only one node in the set can be 
 * simultaneously high.  Similiarly for <code>LO</code>.  If
 * <code>hiLo == CC</code> then only one node in the set can flip
 * in a direction at any time.  If <code>hiLo == NOCC</code>, then
 * the nodes will not be considered as a cap-coupling aggressor.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class ExclusiveNodeSet {
    public static final int LO = 0;
    public static final int HI = 1;
    public static final int CC = 2;
    public static final int NOCC = 3;

    private final int hiLo;
    private final ArrayList nodes;

    public ExclusiveNodeSet(final int hiLo,
                            final Collection nodes) {
        if (hiLo != LO && hiLo != HI && hiLo != CC && hiLo != NOCC)
            throw new IllegalArgumentException("bad value for hiLo:" + hiLo);

        this.hiLo = hiLo;

        // check that everything in the collection is a HierName
        for (final Iterator i = nodes.iterator(); i.hasNext(); ) {
            final Object o = i.next();

            if (!(o instanceof HierName))
                throw new IllegalArgumentException("bad type "
                        + o.getClass().getName() + " in nodes");
        }

        this.nodes = new ArrayList(nodes);
    }

    /**
     * Return an iterator of HierNames that cannot be simultaneously high
     * or low.
     **/
    public Iterator getNodes() {
        return nodes.iterator();
    }

    public int getNumNodes() {
        return nodes.size();
    }

    /**
     * Returns either HI (if only one node in the set can be high
     * at one time) or LO (if only one node can be low at the same time),
     * CC (if only one node can flip in a direction at any time), or NOCC
     * (if no nodes in the set are cap-coupling aggressors).
     **/
    public int getHiLo() {
        return hiLo;
    }

    /**
     * Returns true if two or more nodes in the collection <code>c</code> are
     * prevented from being simultaneously high or low by one of the exclhi /
     * excllo rules, or are prevented from changing to the same value
     * simultaneously by one of the exclcc rules.
     *
     * @param c  Collection of HierNames to be checked for exclusion
     **/
    public boolean areExclusive(final Collection c) {
        boolean found = false;
        for (final Iterator i = c.iterator(); i.hasNext(); ) {
            final HierName hn = (HierName) i.next();

            if (nodes.contains(hn)) {
                if (found)
                    return true;
                else
                    found = true;
            }

        }
        return false;
    }

    /**
     * Returns a new ExclusiveNodeSet such that the nodes use only the
     * canonical node name, as defined in <code>aliases</code>.  If a
     * name does not appear in aliases, do not add it to the
     * ExclusiveNodeSet.
     **/
    public ExclusiveNodeSet canonicalizeNames(final AliasedSet aliases) {
        final ArrayList canonNodes = new ArrayList(nodes.size());

        for (final Iterator i = getNodes(); i.hasNext(); ) {
            final HierName n = (HierName) i.next();
            final HierName cn = (HierName) aliases.getCanonicalKey(n);
            if (cn != null)
                canonNodes.add(cn);
        }

        return new ExclusiveNodeSet(getHiLo(), canonNodes);
    }

    /**
     * Returns a new ExclusiveNodeSet with the names prefixed
     * by the specified subcell name.
     **/
    public ExclusiveNodeSet prefixNames(final HierName subCellName) {
        final ArrayList prefixedNodes = new ArrayList(nodes.size());

        for (final Iterator i = getNodes(); i.hasNext(); ) {
            final HierName n = (HierName) i.next();
            prefixedNodes.add(HierName.prefixName(subCellName, n));
        }

        return new ExclusiveNodeSet(getHiLo(), prefixedNodes);
    }

    /**
     * Returns a string representation of the object for debugging use
     * only.
     **/
    public String toString() {
        final StringBuffer sb = new StringBuffer();

        if (getHiLo() == HI)
            sb.append("exclhi(");
        else if (getHiLo() == LO)
            sb.append("excllo(");
        else if (getHiLo() == CC)
            sb.append("exclcc(");
        else if (getHiLo() == NOCC)
            sb.append("nocc(");
        else
            throw new AssertionFailure("Bad hi / lo value: " + getHiLo());

        for (final Iterator iNode = getNodes(); iNode.hasNext(); ) {
            final HierName hn = (HierName) iNode.next();
            sb.append(' ').append(hn.toString());
        }

        sb.append(" )");
        return sb.toString();
    }
}
