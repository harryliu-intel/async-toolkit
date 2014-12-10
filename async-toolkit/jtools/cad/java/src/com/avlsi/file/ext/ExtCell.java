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

package com.avlsi.file.ext;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import com.avlsi.file.common.Capacitor;
import com.avlsi.file.common.HierName;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.FilteringIterator;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.util.functions.BinaryAction;
import com.avlsi.util.functions.UnaryPredicate;

/**
 * This class represents information from a .ext file.  Rather than
 * simply present an abstract syntax tree of a .ext file, try to
 * present the information in "useful" data structures.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class ExtCell {
    private final String name;
    private Environment environment = null;
    private final List fetList = new ArrayList();
    private final List capList = new ArrayList();

    /**
     * Global repository for subcell definitions.  Passed in
     * from ExtParser.
     **/
    private final Map subcellNameToDefnMap;

    /**
     * Map from subcell use name (String) to ExtCell
     **/
    private final Map subcellUseMap = new TreeMap();

    /**
     * Namespace info.  Map from set of names to node.  Names
     * may be in this cell or of the form sub/name for captured
     * nodes from subcells.
     **/
    private final AliasedMap asm
        = new AliasedMap(
                new Node.NodeMergeFunction(), HierName.getComparator());

    /**
     * Constructs an ExtCell with the given name.
     **/
    public ExtCell(final String name, final Map subcellNameToDefnMap) {
        this.name = name;
        this.subcellNameToDefnMap = subcellNameToDefnMap;
    }

    /**
     * Return the name of the cell.
     **/
    public String getName() {
        return name;
    }

    //
    // environment related methods
    //

    /**
     * set the environment
     **/
    public void setEnvironment(final Environment environment) {
        this.environment = environment;
    }

    /**
     * get the environment
     **/
    public Environment getEnvironment() {
        return environment;
    }

    //
    // fet related methods
    //

    /**
     * Adds a fet.
     **/
    public void addFET(final FET fet) {
        // add to namespace
        addName(fet.getSubstrateNode());
        addName(fet.getGate().getConnectingNode());
        addName(fet.getSource().getConnectingNode());
        addName(fet.getDrain().getConnectingNode());

        fetList.add(fet);
    }

    /**
     * Returns an iterator of FETs.
     **/
    public Iterator getFETs() {
        return fetList.iterator();
    }

    //
    // cap related methods
    //

    /**
     * Adds a capacitor.  Adds its source and drain to the namespace.
     **/
    public void addCapacitor(final Capacitor capacitor) {
        // add to namespace
        addName(capacitor.getSource());
        addName(capacitor.getDrain());

        capList.add(capacitor);
    }

    /**
     * Returns an iterator of Capacitors.
     **/
    public Iterator getCapacitors() {
        return capList.iterator();
    }

    //
    // namespace related methods
    //

    /**
     * return an Iterator of Nodes.  
     **/
    public Iterator getNodes() {
        return new FilteringIterator(asm.getValues(),
                new UnaryPredicate() {
                    public boolean evaluate(final Object o) {
                        return ((Node) o) != null;
                    }
                });
    }

    /**
     * returns the canonical name for a node,
     * or null if that nodename is not known.
     **/
    public HierName getCanonicalName(final HierName nodeName) {
        return (HierName) asm.getCanonicalKey(nodeName);
    }

    /**
     * Returns the node with the given name.
     **/
    public Node getNodeByName(HierName name) {
        return (Node) asm.getValue(name);
    }

    /**
     * Adds the node using the given name.   If the node already
     * exists, merge the contents with Node.NodeMergeFunction.
     *
     * @param nodeName (WHERE nodeName != null) the name of the node
     * @param node (WHERE node != null || node == null)  the node data,
     *   pass null to indicate no data.
     **/
    public void addData(final HierName nodeName, final Node node)
        throws AliasedMap.MergeFailedException
    {
        Debug.assertTrue(nodeName != null);
        asm.addData(nodeName, node);
    }

    /**
     * Adds a name, with no data.
     **/
    public void addName(final HierName nodeName)
    {
        try {
            addData(nodeName, null);
        } catch (AliasedMap.MergeFailedException e) {
            throw new AssertionFailure(e);
        }
    }

    /**
     * Connects the two names, adding them if they don't both
     * exist.
     **/
    public void connectNames(final HierName name1, final HierName name2)
        throws AliasedMap.MergeFailedException
    {
        Debug.assertTrue(name1 != null);
        Debug.assertTrue(name2 != null);
        asm.makeEquivalent(name1, name2);
    }

    /**
     * Returns an Iterator of HierName of the equivalent names to 
     * the node.  Includes name in the list.
     **/
    public Iterator getEquivalentNames(final HierName name) {
        return asm.getAliases(name);
    }

    /**
     * Returns whether two names are equivalent.
     **/
    public boolean areEquivalent(final HierName hn1, final HierName hn2) {
        return asm.areEquivalent(hn1, hn2);
    }


    //
    // subcell related methods
    //

    /**
     * Adds a subcell.
     **/
    public void addSubcell(final String useName, final ExtCell subExt) {
        final Object o = subcellUseMap.put(useName, subExt);

        if (o != null)
            throw new RuntimeException("multiple use of " + useName
                    + " old defn replaced.");
    }

    /**
     * returns an Iterator of Strings of the subcell use names.
     **/
    public Iterator getSubcellNames() {
        return subcellUseMap.keySet().iterator();
    }

    /**
     * returns the definition of the subcell based on its usage name
     **/
    public ExtCell getSubcellDefForName(final String name) {
            return (ExtCell) subcellUseMap.get(name);
    }

    /**
     * returns the type of the subcell based on its name
     **/
    public String getSubcellTypeForName(final String name) {
        return getSubcellDefForName(name).getName();
    }


    //
    // Subcell traversal methods
    //

    /**
     * Recursively applies visitSubcells on all subcells of
     * this cell, then calls f on this cell itself.
     * The first argument to f.execute is a String representing
     * the path of cells above in the hierarchy.  The second
     * argument is the ExtCell itself.
     *
     * @param prefix  path indicating cells above in hierarchy
     * @param f  the BinaryAction to be applied to all cells
     **/
    private void visitSubcells(final String prefix, final BinaryAction f) {
        for (final Iterator i = getSubcellNames(); i.hasNext(); ) {
            final String n = (String) i.next();

            // this would be nicer if local names started with /
            final String newPrefix
                = prefix.length() == 0 ? n : prefix + "/" + n;
            getSubcellDefForName(n).visitSubcells(newPrefix, f);
        }

        f.execute(prefix, this);
    }

    /**
     * Recursively applies visitSubcells on all subcells of
     * this cell, then calls f on this cell itself.
     * The first argument to f.execute is a String representing
     * the path of cells above in the hierarchy.  The second
     * argument is the ExtCell itself.
     **/
    public void visitSubcells(final BinaryAction f) {
        visitSubcells("", f);
    }

    /**
     * Applies a function to all the capacitors in the tree.
     * The first argument to f.execute is a String representing
     * the path of cells above in the hierarchy.  The second
     * argument is the Capacitor itself.
     **/
    public void visitCapacitors(final BinaryAction f) {
        visitSubcells(new BinaryAction() {
            public void execute(final Object a, final Object b) {
                final String prefix = (String) a;
                final ExtCell e = (ExtCell) b;

                for (final Iterator i = e.getCapacitors(); i.hasNext(); ) {
                    final Capacitor c = (Capacitor) i.next();
                    f.execute(prefix, c);
                }
            }
        });
    }

    /**
     * Applies a function to all the resistors in the tree.
     * The first argument to f.execute is a String representing
     * the path of cells above in the hierarchy.  The second
     * argument is the Resistor itself.
     **/
    /*
    public void visitResistors(final BinaryAction f) {
        visitSubcells(new BinaryAction() {
            public void execute(final Object a, final Object b) {
                final String prefix = (String) a;
                final ExtCell e = (ExtCell) b;

                for (final Iterator i = e.getCapacitors(); i.hasNext(); ) {
                    final Resistor r = (Capacitor) i.next();
                    f.execute(prefix, r);
                }
            }
        });
    }
    */

    /**
     * Applies a function to all the nodes in the tree.
     * The first argument to f.execute is a String representing
     * the path of cells above in the hierarchy.  The second
     * argument is the Resistor itself.
     **/
    public void visitNodes(final BinaryAction f) {
        visitSubcells(new BinaryAction() {
            public void execute(final Object a, final Object b) {
                final String prefix = (String) a;
                final ExtCell e = (ExtCell) b;

                for (final Iterator i = e.getNodes(); i.hasNext(); ) {
                    final Node n = (Node) i.next();
                    f.execute(prefix, n);
                }
            }
        });
    }

    /**
     * Applies a function to all the FETs in the tree.
     * The first argument to f.execute is a String representing
     * the path of cells above in the hierarchy.  The second
     * argument is the FET itself.
     **/
    public void visitFETs(final BinaryAction f) {
        visitSubcells(new BinaryAction() {
            public void execute(final Object a, final Object b) {
                final String prefix = (String) a;
                final ExtCell e = (ExtCell) b;

                for (final Iterator i = e.getFETs(); i.hasNext(); ) {
                    final FET fet = (FET) i.next();
                    f.execute(prefix, fet);
                }
            }
        });
    }

    //
    // toString
    //

    public String toString() {
        final StringWriter sw = new StringWriter();
        final Writer w = new PrintWriter(sw);

        /*
        private final FET[] fets;
        private final Subcell[] subcells;
        private final Map pairToResistanceMap;
        private final Map pairToCapacitanceMap;
        private final Namespace namespace;
        */

        /*
        w.println("ExtCell {");
        w.println(environment);
        w.println(namespace);
        */

        // xxx

        return w.toString();
    }
}
