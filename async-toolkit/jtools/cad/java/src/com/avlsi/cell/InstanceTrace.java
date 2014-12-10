/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cell;

import java.util.Iterator;
import java.util.LinkedList;

import com.avlsi.file.common.HierName;
import com.avlsi.util.container.Pair;

/**
 * A class that represents a path in the instantiation tree.  It is similar to
 * Java stack traces.
 **/
public final class InstanceTrace {
    public final class Element {
        private final String cellName, instanceName;
        /**
         * Construct a new element.
         *
         * @param cellName Name of the cell being instantiated
         * @param instanceName Name of the instance
         **/
        public Element(final String cellName, final String instanceName) {
            this.cellName = cellName;
            this.instanceName = instanceName;
        }
        public String getCellName() {
            return cellName;
        }
        public String getInstanceName() {
            return instanceName;
        }
        public String toString() {
            return "\tin type " + cellName + " (instantiated as " + instanceName + ")";
        }
    }

    private final LinkedList /*<Element>*/ trace;

    public InstanceTrace() {
        this.trace = new LinkedList /*<Element>*/ ();
    }

    public Element[] getInstanceTrace() {
        return (Element[]) trace.toArray(new Element[0]);
    }

    public void enter(final Element elem) {
        trace.addFirst(elem);
    }

    public void enter(final String cellName, final String instanceName) {
        enter(new Element(cellName, instanceName));
    }

    public void enter(final String cellName, final HierName instanceName) {
        enter(cellName, instanceName.getAsString('.'));
    }

    public void enter(final Pair subcellInstance) {
        final HierName instance = (HierName) subcellInstance.getFirst();
        final CellInterface cell = (CellInterface) subcellInstance.getSecond();
        enter(cell.getFullyQualifiedType(), instance);
    }

    public Element leave() {
        return (Element) trace.removeFirst();
    }

    public String toString() {
        final StringBuffer buf = new StringBuffer();
        for (Iterator i = trace.iterator(); i.hasNext(); ) {
            final Element elem = (Element) i.next();
            buf.append(elem.toString() + "\n");
        }
        return buf.toString();
    }
}
