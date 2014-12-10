/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.cell;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

import com.avlsi.util.container.AliasedSet;

/**
 * Aggregates exclusive hi / lo information together, so that queries
 * can be made.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class ExclusiveNodeSets {
    /**
     * List of the exclusion sets (type <code>ExclusiveNodeSet</code>).  
     **/
    private final ArrayList exclusiveNodeSetList;

    public ExclusiveNodeSets() {
        this.exclusiveNodeSetList = new ArrayList();
    }

    public Iterator getIterator() {
        return Collections.unmodifiableList(exclusiveNodeSetList).iterator();
    }

    public void addExclusiveNodeSet(final ExclusiveNodeSet ens) {
        exclusiveNodeSetList.add(ens);
    }

    /**
     * Returns true if two or more nodes in the collection <code>c</code>
     * are prevented from being simultaneously low by one of the exclhi 
     * or excllo rules, or are prevented from switching in the same
     * direction simultaneously by one of the exclcc rules, as specified by
     * <code>hiLo</code>.
     *
     * @param c  Collection of HierNames to be checked for exclusion
     * @param hiLo  ExclusiveNodeSet.HI, ExclusiveNodeSet.LO, or
     * ExclusiveNodeSet.CC depending on whether exclusive hi, lo, or switching
     * in the same direction, should be checked.
     **/
    public boolean areExclusive(final int hiLo, final Collection c) {
        for (final Iterator i = getIterator(); i.hasNext(); ) {
            final ExclusiveNodeSet ens = (ExclusiveNodeSet) i.next();

            if (ens.getHiLo() == hiLo && ens.areExclusive(c))
                return true;
        }

        return false;
    }

    /**
     * Ensures that all nodes use only the canonical node name, as defined
     * in <code>aliases</code>.
     **/
    public ExclusiveNodeSets canonicalizeNames(final AliasedSet aliases) {
        final ExclusiveNodeSets enss = new ExclusiveNodeSets();

        for (int i = 0; i < exclusiveNodeSetList.size(); ++i) {
            final ExclusiveNodeSet ens =
                (ExclusiveNodeSet) exclusiveNodeSetList.get(i);

            enss.addExclusiveNodeSet(ens.canonicalizeNames(aliases));
        }

        return enss;
    }

    /**
     * Absorbs exclusive node sets from the refinement parent.
     **/
    public void refineFrom(final ExclusiveNodeSets parent) {
        merge(parent);
    }

    /**
     * Adds in all exclusive node sets from another ExclusiveNodeSets.
     **/
    public void merge(final ExclusiveNodeSets other) {
        exclusiveNodeSetList.addAll(other.exclusiveNodeSetList);
    }

    /**
     * Returns a string representation of the ExclusiveNodeSets for 
     * debugging purposes.
     **/
    public String toString() {
        final StringBuffer sb = new StringBuffer();

        for (final Iterator i = getIterator(); i.hasNext(); ) {
            final ExclusiveNodeSet ens = (ExclusiveNodeSet) i.next();

            sb.append(ens.toString()).append('\n');
        }

        return sb.toString();
    }
}
