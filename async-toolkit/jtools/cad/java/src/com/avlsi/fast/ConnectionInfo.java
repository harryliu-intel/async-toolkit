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

package com.avlsi.fast;

import com.avlsi.file.common.HierName;
import com.avlsi.cell.CellInterface;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;

import java.util.Comparator;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;
import java.util.TreeMap;

import com.avlsi.util.container.ObjectUtils;

/**
 * Class to represent linkages in the instantiation hierarchy.
 *
 * Encapsulates an instantiation of a cell in another cell.
 * Can map the name of a net in the parent cell to the name
 * of the net in the instantiated cell that it is connected to.
 * Can map the name of a net in the instantiated cell to the name of
 * of the net in the instantiating cell that it is connected to.
 * 
 *
 * Part of the subcells block.
 *
 * Combines the role of the original connection info in the jauto
 * datastructure design document, and that of the "location info",
 * though the location information may be non-existent.
 *
 * Edges on the instantiation DAG.
 *
 *
 *
 * @author Aaron Denney
 * @version $Date$
 **/

public class ConnectionInfo {
    /** Type of parent. **/
    public final CellType        parent;
    /** Type of child. **/
    public final CellType        child;
    /** Name of child cell in parent. **/
    public final HierName        nameInParent;
    /**
     * Offset in parent. These are valid iff the parent and the child have
     * location information.  It is an error for only one to.
     * (floorplanned cells must be contained in floorplanned cells,
     * sized cells in sized cells, and layed out cells in layed out cells.)
     **/
    public        float xOffset, yOffset;
    // It is always the case that, 
    // \forall(x \in childToParent.keySet();
    //         childToParent.get(parentToChild.get(x)) == x)
    // and
    // \forall(x \in parentToChild.keySet();
    //         parentToChild.get(childToParent.get(x)) == x)
    // ie, the maps are inverses of each other
    /** name map. **/
    private final Map/*<HierName,HierName>*/ childToParent;
    /** name map. **/
    private final Map/*<HierName,HierName>*/ parentToChild;

    private static Comparator comparator = null;

    /** Possible orientation values, the same as in Cadence. All rotations are
     * counterclockwise. */
    public static final int R0 = 0;    /* No rotation */
    public static final int R90 = 1;   /* Rotate 90 degrees */
    public static final int R180 = 2;  /* Rotate 180 degrees */
    public static final int R270 = 3;  /* Rotate 270 degrees */
    public static final int MY = 4;    /* Mirror about y */
    public static final int MYR90 = 5; /* Mirror about y, rotate 90 degrees */
    public static final int MX = 6;    /* Mirror about x */
    public static final int MXR90 = 7; /* Mirror about x, rotate 90 degrees */

    /** Compose two orientations, and return the result orientation. */
    public static int composeOrientation(int first, int second) {
        switch (first) {
          case R0:
            return second;

          case R90:
            switch (second) {
              case R0: return R90;
              case R90: return R180;
              case R180: return R270;
              case R270: return R0;
              case MY: return MYR90;
              case MYR90: return MX;
              case MX: return MXR90;
              case MXR90: return MY;
              default:
                 throw new AssertionError("Unknown orientation " + second);
            }

          case R180:
            switch (second) {
              case R0: return R180;
              case R90: return R270;
              case R180: return R0;
              case R270: return R90;
              case MY: return MX;
              case MYR90: return MXR90;
              case MX: return MY;
              case MXR90: return MYR90;
              default:
                throw new AssertionError("Unknown orientation " + second);
            }

          case R270:
            switch (second) {
              case R0: return R270;
              case R90: return R0;
              case R180: return R90;
              case R270: return R180;
              case MY: return MXR90;
              case MYR90: return MY;
              case MX: return MYR90;
              case MXR90: return MX;
              default:
                throw new AssertionError("Unknown orientation " + second);
            }

          case MY:
            switch (second) {
              case R0: return MY;
              case R90: return MXR90;
              case R180: return MX;
              case R270: return MYR90;
              case MY: return R0;
              case MYR90: return R270;
              case MX: return R180;
              case MXR90: return R90;
              default:
                throw new AssertionError("Unknown orientation " + second);
            }

          case MYR90:
            switch (second) {
              case R0: return MYR90;
              case R90: return MY;
              case R180: return MXR90;
              case R270: return MX;
              case MY: return R90;
              case MYR90: return R0;
              case MX: return R270;
              case MXR90: return R180;
              default:
                throw new AssertionError("Unknown orientation " + second);
            }

          case MX:
            switch (second) {
              case R0: return MX;
              case R90: return MYR90;
              case R180: return MY;
              case R270: return MXR90;
              case MY: return R180;
              case MYR90: return R90;
              case MX: return R0;
              case MXR90: return R270;
              default:
                throw new AssertionError("Unknown orientation " + second);
            }

          case MXR90:
            switch (second) {
              case R0: return MXR90;
              case R90: return MX;
              case R180: return MYR90;
              case R270: return MY;
              case MY: return R270;
              case MYR90: return R180;
              case MX: return R90;
              case MXR90: return R0;
              default:
                throw new AssertionError("Unknown orientation " + second);
            }

          default:
            throw new AssertionError("Unknown orientation " + first);
        }
    }

    /** Orientation of the instance */
    public int orientation = R0;

    /** Base index of independent variable relative to the parent. **/
    private int baseIndex = 0;

    /**
     * Constructor.
     **/
    ConnectionInfo(CellType parent, CellType child, HierName name, Cadencize c) {
        this.parent = parent;
        this.child = child;
        this.nameInParent = name;
        xOffset = 0; yOffset = 0;
        NameHelper h = new NameHelper(c);
        childToParent = h.createChildToParent(); 
        parentToChild = h.createParentToChild(); 
        orientation = R0;
    }

    ConnectionInfo factory() {
        return null;
    }

    /**
     * Given the name of a net in the instantiated cell returns
     * the name of a net in the instantiating cell connected to it.
     * @param c the name of a node in the child's portlist.
     * @return the name of child's node c in the parent.
     **/
    public HierName getParentName(HierName c) {
        return (HierName) childToParent.get(c);
    }
    
    /**
     * Given a net in the instantiated cell returns
     * the name of a net in the instantiating cell connected to it.
     * @param cNet A net in the child's portlist.
     * @return the name of child's node c in the parent.
     **/
    public HierName getParentName( CellNet cNet ) {
        Debug.assertTrue( child == cNet.container );
        return getParentName( cNet.canonicalName );
    }

    /**
     *  Given the name of a net in the instantiating cell returns
     *  the name of a net in the instantiated cell connected to it.
     * @param p the name of a node in the parent that is connected
     *          to this child.
     * @return the name of parent's node p in the child.
     **/
    public HierName getChildName(HierName p) {
        return (HierName) parentToChild.get(p);
    }
    
    /**
       Given a net in the instantiating cell returns
       the name of a net in the instantiated cell connected to it.
     * @param p A net in the parent that is connected
     *          to this child.
     * @return the name of parent's node p in the child.
     **/
    public HierName getChildName( CellNet pNet ) {
        Debug.assertTrue( parent == pNet.container );
        return getChildName( pNet.canonicalName );
    }

    /**
     * resolves names to build name &lt;-&gt; name mapping.
     *
     * Should perhaps be using cadencizer?
     **/
    private class NameHelper {
        Cadencize c;
        TreeMap/*<HierName,HierName>*/ parentNames = new TreeMap/*<HierName,HierName>*/();
        TreeMap/*<HierName,HierName>*/ childNames = new TreeMap/*<HierName,HierName>*/();

        NameHelper(Cadencize c) {
            this.c = c;
            final HierName prefix = nameInParent;
            // cadencize not only unifies nodes, but it also converts globals used in cell or subcell to ports.
            final CadenceInfo ci = c.convert(child.cast_cell);
            final AliasedMap portNodes = ci.getPortNodes();
            for (Iterator i = portNodes.getKeys(); i.hasNext(); ) {
                HierName h = (HierName) i.next();
                // Only add the nodes that are to be included in the port list
                if (((Boolean) portNodes.getValue(h)).booleanValue() ||
                    child.isWiringCell ||
                    child.isInternalEnv()) {
                    HierName parentLocal = HierName.append(prefix, h);
                    parentNames.put(parentLocal, h);
                    childNames.put(h, parentLocal);
                }
            }
        }

        Map/*<HierName,HierName>*/ createParentToChild() {
            return parentNames;
        }

        Map/*<HierName,HierName>*/ createChildToParent() {
            return childNames;
        }
    }

    /**
       Generates a string representation of the ConnectionInfo.
       @return A string representing the ConnectionInfo.
     */
    public String toString() {
        StringBuffer temp = new StringBuffer("Parent = ");
        temp.append(parent);
        temp.append(", child = ");
        temp.append(child);
        temp.append(" named ");
        temp.append(nameInParent);
        for (Iterator i = parentToChild.entrySet().iterator(); i.hasNext(); ) {
            Map.Entry e = (Map.Entry) i.next();
            HierName parent = (HierName) e.getKey();
            HierName child = (HierName) e.getValue();
            temp.append("\n");
            temp.append(parent).append(" = ").append(child);
        }
        return temp.toString();
    }

    public static final class NameIterator {
        private Iterator i;
        private NameIterator(Map m) {
            this.i = m.keySet().iterator();
        }

        public boolean hasNext() {
            return i.hasNext();
        }

        /** @throws NoSuchElementException **/
        public HierName next() {
            return (HierName) i.next();
        }
    }

    /**
       Constructs an iterator that will iterate over all the net names
       that reference nets in the instantiating cell that are connected to
       nets in the instantiated cell.
       @return An iterator that will iterate over all the net names
       that reference nets in the instantiating cell that are connected to
       nets in the instantiated cell.
     */
    public NameIterator parentIterator() {
        return new NameIterator(parentToChild);
    }

    /**
       Constructs an iterator that will iterate over all the net names
       that reference nets in the instantiated cell that are connected to
       nets in the instantiating cell.
       @return An iterator that will iterate over all the net names
       that reference nets in the instantiated cell that are connected to
       nets in the instantiating cell.
     */
    public NameIterator childIterator() {
        return new NameIterator(childToParent);
    }


    public final Set/*<CellNet>*/ getParentNet(CellNet cn1)
    {
        HashSet/*<CellNet>*/ seta = new HashSet/*<CellNet>*/();

        for (Iterator ita = cn1.portNames.iterator(); ita.hasNext(); ){
            HierName hna = (HierName)ita.next();

            seta.add(parent.getNet(getParentName(hna)));
        }


        return seta;
    }

    public static Comparator getComparator() {
        if (comparator == null) {
            comparator =
                new Comparator() {
                    public int compare(Object o1, Object o2) {
                        final ConnectionInfo ci1 = (ConnectionInfo) o1;
                        final ConnectionInfo ci2 = (ConnectionInfo) o2;
                        int x;
                        x = ObjectUtils.compare(ci1.child.typeName,
                                                ci2.child.typeName);
                        if (x != 0) return x;
                        x = ObjectUtils.compare(ci1.parent.typeName,
                                                ci2.parent.typeName);
                        if (x != 0) return x;
                        x = ObjectUtils.compare(ci1.nameInParent,
                                                ci2.nameInParent);
                        return x;
                    }
                    public boolean equals(Object o) {
                        return this == o;
                    }
                };
        }
        return comparator;
    }

    public int getIndex() {
        return baseIndex;
    }

    public void setIndex(int index) {
        baseIndex = index;
    }
}
