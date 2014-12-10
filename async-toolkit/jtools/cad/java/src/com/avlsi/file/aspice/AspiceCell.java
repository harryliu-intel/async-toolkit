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

package com.avlsi.file.aspice;

import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

//import com.avlsi.file.common.Capacitor;
import com.avlsi.file.common.HierName;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.Namespace;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.util.functions.BinaryFunction;
import com.avlsi.util.functions.UnaryFunction;

import com.avlsi.circuit.AbstractCircuit;
import com.avlsi.circuit.ResistorInterface;
import com.avlsi.circuit.DiodeInterface;
import com.avlsi.circuit.TransistorInterface;
import com.avlsi.circuit.CapacitorInterface;

/**
 * Class to represent a parsed .aspice file.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class AspiceCell extends AbstractCircuit {
    
    /**
     * List of Diodes.
     **/
    private final List diodeList = new ArrayList();

    /**
     * List of Resistors
     **/
    private final List resistorList = new ArrayList();

    /**
     * List of Transistors.
     **/
    private final List transistorList = new ArrayList();

    /**
     * List of capacitors.
     **/
    private final List capacitorList = new ArrayList();
    
    /**
     * Map from subcell use name (String) to AspiceCell
     **/
    private final Map subcellUseMap = new TreeMap();

    
    /**
     * Namespace info.  Map from set of names to null object.
     * Names may be in this sell or of the form sub.name for
     * captured names from subcells.
     **/
    private final AliasedMap asm
        = new AliasedMap(new AliasedMap.MergeFunction() {
            public Object merge(final Object a, final Object b) {
                if (a == null)
                    return b;
                else if (b == null)
                    return a;
                else {
                    Debug.assertTrue(a == EXISTS);
                    Debug.assertTrue(b == EXISTS);
                    return EXISTS;
                }
            }
        }, HierName.getComparator());

    private static final Object EXISTS = new Object();
    
    /*************************************************
     * Static nested subcircuit repository subclass.
     *************************************************/
    public static class Repository extends AbstractCircuit.Repository {
        /**
         *  AspiceCell generator method.
         **/
        protected AbstractCircuit newCircuitGenerator(final String cellType) {
            return new AspiceCell(cellType);
        }
    }
    
    //
    // class constructor
    //

    /**
     * Class constructor.  Use mutators to build up an AspiceCell.
     **/
    public AspiceCell(final String type) {
        super(type);
    }

    
    //
    // subcellUseMap related methods
    //

    /**
     * Returns an Iterator of Strings representing subcell names.
     **/
    public Iterator getSubcellNames() {
        return subcellUseMap.keySet().iterator();
    }

    /**
     * Adds a definition defn for subName, replacing any existing definition.
     **/
    public void addSubcell(final String subName, final AspiceCell defn) {
        final Object o = subcellUseMap.put(subName, defn);

        if (o != null)
            throw new RuntimeException("duplicate defn for " + subName
                    + " old defn replaced");
    }

    /**
     * returns the type of the subcell based on its name, or null
     * if there is no subcell of that name.
     **/
    public String getSubcellTypeForName(final String name) {
        final AbstractCircuit a = getSubcellDefForName(name);

        if (a == null)
            return null;
        else
            return a.getType();
    }

    //
    // diodeList related methods
    //

    /**
     * Returns an Iterator of the defined Diodes.
     **/
    public Iterator getDiodes() {
        return diodeList.iterator();
    }

    /**
     * Adds a diode to the list of diodes.
     **/
    public void addDiode(final DiodeInterface diode) {
        addName(diode.getSource());
        addName(diode.getDrain());
        diodeList.add(new Diode(diode.getType(),diode.getSource(),
                                diode.getDrain(),
                                diode.getWidth(), diode.getLength(),
                                diode.getArea(), diode.getPerimeter()));
    }

    //
    // resistorList related methods
    //

    /**
     * Returns an Iterator of the defined Resistors.
     **/
    public Iterator getResistors() {
        return resistorList.iterator();
    }

    /**
     * Adds a resistor to the list of resistors.
     **/
    public void addResistor(final ResistorInterface r) {
        addName(r.getSource());
        addName(r.getDrain());
        resistorList.add(new Resistor(r.getSource(),
                                      r.getDrain(),
                                      r.getConductance()));
    }

    /**
     * Returns the number of resistors in the file.
     * (Not including resistors in subcells.)
     **/
    public int getResistorCount() {
        return resistorList.size();
    }

    //
    // transistorList related methods
    //

    /**
     * Returns an Iterator of the defined Transistors
     **/
    public Iterator getTransistors() {
        return transistorList.iterator();
    }

    /**
     * Adds a transistor to the list of transistors.
     **/
    public void addTransistor(final TransistorInterface t) {
        addName(t.getSource());
        addName(t.getDrain());
        addName(t.getGate());
        addName(t.getBulk());
        transistorList.add(new Transistor( t.getType(), t.getSource(),
                                           t.getDrain(),t.getGate(),
                                           t.getBulk(),
                                           t.getWidth(), t.getLength()));
    }

    /**
     * Returns the number of transistors in the file.
     * (Not including transistors in subcells.)
     **/
    public int getTransistorCount() {
        return transistorList.size();
    }


    /**
     * Returns the number of transistors in the file.
     * (Not including transistors in subcells.)
     **/
    public int getDiodeCount() {
        return diodeList.size();
    }
    

    //
    // capacitorList related methods
    //

    /**
     * Returns an Iterator of the defined Capacitors
     **/
    public Iterator getCapacitors() {
        return capacitorList.iterator();
    }

    /**
     * Adds a capacitor to the list of capacitors.
     **/
    public void addCapacitor(final CapacitorInterface capacitor) {
        addName(capacitor.getSource());
        addName(capacitor.getDrain());
        capacitorList.add(new Capacitor(capacitor.getSource(),
                                        capacitor.getDrain(),
                                        capacitor.getCapacitance()));
    }

    /**
     * Returns the number of capacitors in the file.
     * (Not including capacitors in subcells.)
     **/
    public int getCapacitorCount() {
        return capacitorList.size();
    }


    //
    // namespace related methods
    //

    /**
     * Returns an iterator of HierNames representing the canonical names.
     **/
    public Iterator getCanonicalNames() {
        return asm.getCanonicalKeys();
    }

    /**
     * returns the canonical name for a node
     **/
    public HierName getCanonicalName(final HierName nodeName) {
        return (HierName) asm.getCanonicalKey(nodeName);
    }

    /**
     * Adds a name.
     **/
    public void addName(final HierName nodeName)
    {
        try {
            if (nodeName.isGlobal())
                addGlobal(nodeName);
            asm.addData(nodeName, EXISTS);
        } catch (AliasedMap.MergeFailedException e) {
            throw new AssertionFailure(e);
        }
    }

    /**
     * Connects the two names, adding them if they don't both
     * exist.
     **/
    public void connectNames(final HierName name1, final HierName name2)
    {
        try {
            if (name1.isGlobal())
                addGlobal(name1);
            if (name2.isGlobal())
                addGlobal(name2);
            asm.makeEquivalent(name1, name2);
        } catch (AliasedMap.MergeFailedException e) {
            throw new AssertionFailure(e);
        }
    }

    /**
     * Returns an Iterator of HierName of the equivalent names to 
     * the node.  Includes name in the list.
     **/
    public Iterator getEquivalentNames(final HierName name) {
        return asm.getAliases(name);
    }

    /**
     * String representation of namespace for debugging.
     **/
    public String getNamespaceString() {
        return asm.toString();
    }

    //
    // misc
    //

    /**
     * Returns true if this cell or any of its subcells as a fet.
     **/
    public boolean hasTransistors() {
        if (transistorList.size() > 0)
            return true;

        for (final Iterator i = getSubcellNames(); i.hasNext(); ) {
            final String subcellName = (String) i.next();

            if (((AspiceCell)getSubcellDefForName(subcellName)).hasTransistors())
                return true;
        }

        return false;
    }


    //
    // output methods
    //

    public void printToStream(final OutputStream out, final AspiceCell.Repository repos) {
        final PrintWriter pw = new PrintWriter(out);
        printToWriter(pw, 0, repos);
        pw.close();
    }

    private void printToWriter(final PrintWriter pw, int nestingLevel,
			       final AspiceCell.Repository repos) {

        // print subcell definitions, if at top level
        if (nestingLevel == 0) {
            // find the set of reachable types
            final List typeList = new ArrayList();
            foldSubcells(new BinaryFunction() {
                public Object execute(final Object a, final Object b) {
                    final AspiceCell af = (AspiceCell) a;

                    // add my type to the set if it is not already there,
                    // except for type of root
                    if (!af.getType().equals(getType())
                        && !typeList.contains(af.getType()))
                        typeList.add(af.getType());

                    return null;
                }
            }, null);

            // print the definitions for the types we have found
            // @review jmr XXX they will not be printed in any specific
            // order, maybe this is a problem
            for (final Iterator i = typeList.iterator(); i.hasNext(); ) {
                final String type = (String) i.next();
                final AspiceCell af
                    = (AspiceCell) repos.getCell(type);

                af.printToWriter(pw, nestingLevel + 1, repos);
            }
        }

        pw.println("define " + getType());
        pw.println("{");

        // print subcell instantiations
        for (final Iterator i = getSubcellNames(); i.hasNext(); ) {
            final String name = (String) i.next();
            final String type = getSubcellTypeForName(name);

            pw.println(type + " " + name + ";");
        }

        // print wires
        for (final Iterator i = getCanonicalNames(); i.hasNext(); ) {
            final HierName canonicalName = (HierName) i.next();
            final Wire wire = new Wire();

            // get all the aliases for this name
            for (final Iterator iAlias = asm.getAliases(canonicalName); 
                    iAlias.hasNext(); ) {
                final String s = ((HierName) iAlias.next()).getAspiceString();
                wire.addNode(s);
            }

            // if the wire actually connects something
            if (wire.getNumNodes() > 1)
                pw.println(wire.getAspiceString());
        }

        // print caps
        for (final Iterator i = getCapacitors(); i.hasNext(); ) {
            final Capacitor cap = (Capacitor) i.next();

            Debug.assertTrue(!asm.equivalent(cap.getSource(), cap.getDrain()));

            pw.println(cap.getAspiceString());
        }

        // print diodes
        for (final Iterator i = getDiodes(); i.hasNext(); ) {
            final Diode d = (Diode) i.next();

            pw.println(d.getAspiceString());
        }

        // print resistors
        for (final Iterator i = getResistors(); i.hasNext(); ) {
            final Resistor r = (Resistor) i.next();

            pw.println(r.getAspiceString());
        }

        // print transistors
        for (final Iterator i = getTransistors(); i.hasNext(); ) {
            final Transistor t = (Transistor) i.next();

            pw.println(t.getAspiceString());
        }

        pw.println("}\n");

        // if at top level, print globals
        if (nestingLevel == 0) {
            for (final Iterator i = getGlobals(); i.hasNext(); ) {
                final HierName global = (HierName) i.next();

                pw.println(".global " + global.getAspiceString() + ";");
            }
        }
    }

    /**
     * If cell has children c1, c2, c3, ..., cN, computes
     * f(this, f(c1, f(c2, ..., f(cN, unit)...))).  The first argument of
     * f is type AspiceCell, the second is type Object, and represents 
     * the state of the traversal.  The traversal order of subcells is
     * not defined.
     **/
    public Object foldSubcells(final BinaryFunction f, Object unit) {
        for (final Iterator i = getSubcellNames(); i.hasNext(); ) {
            final String subcellName = (String) i.next();
	    
            unit = ((AspiceCell)getSubcellDefForName(subcellName)).foldSubcells(f, unit);
        }

        return f.execute(this, unit);
    }

    /**
     * Imports the contents of another aspice file, prefixing
     * all node names with prefix.
     **/
    private void importCell(final String prefix, final AspiceCell af) {

        throw new AssertionFailure("FIXME XXX");
        /*
        // import diodes
        for (final Iterator i = af.getDiodes(); i.hasNext(); ) {
            final Diode d = (Diode) i.next();

            addDiode(new Diode(d.getType(),
                        prefix + d.getSource(), prefix + d.getDrain(),
                        d.getWidth(), d.getLength(),
                        d.getArea(), d.getPerimeter()));
        }

        // import transistors
        for (final Iterator i = af.getTransistors(); i.hasNext(); ) {
            final Transistor t = (Transistor) i.next();

            addTransistor(new Transistor(t.getType(),
                    prefix + t.getSource(), prefix + t.getDrain(),
                    prefix + t.getGate(), prefix + t.getBulk(),
                    t.getWidth(), t.getLength()));
        }

        // import caps
        for (final Iterator i = af.getCapacitors(); i.hasNext(); ) {
            final Capacitor c = (Capacitor) i.next();

            addCapacitor(new Capacitor(prefix + c.getSource(),
                        prefix + c.getDrain(), c.getCap()));
        }

        // XXX import resistors

        // import globals
        for (final Iterator i = af.getGlobals(); i.hasNext(); ) {
            final String g = (String) i.next();

            addGlobal(prefix + g);
        }

        // import namespace 
        try {
            asm.adopt(prefix, new UnaryFunction() {
                public Object execute(final Object a) {
                    Debug.assertTrue(a == null);
                    return null;
                }
            }, af.asm);
        } catch (AliasedMap.MergeFailedException e) {
            throw new AssertionFailure("How can merge fail when"
                    + " it doesn't do anything?");
        }
        */
    }

    /**
     * Flattens the AspiceCell and all its subcells into a flat
     * representation, with no subcells.  Returns a new AspiceCell,
     * and does not modify any existing ones.  Does not modify the 
     * subcellNameToDefnMap.
     **/
    public AspiceCell flatten() {
        // @todo jmr XXX abstract out this recursion

        final AspiceCell af = new AspiceCell(getType());

        for (final Iterator i = getSubcellNames(); i.hasNext(); ) {
            final String subcellName = (String) i.next();
            final AspiceCell flat
                = ((AspiceCell)getSubcellDefForName(subcellName)).flatten();

            af.importCell(subcellName + ".", flat);
        }

        af.importCell("", this);

        return af;
    }
}
