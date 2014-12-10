/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.circuit;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import com.avlsi.util.container.AliasedMap;
import com.avlsi.file.common.HierName;
//import com.avlsi.file.common.Capacitor;
//import com.avlsi.file.aspice.Diode;
//import com.avlsi.file.aspice.Resistor;
//import com.avlsi.file.aspice.Transistor;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.util.debug.Debug;

/**
 * Abstract Hierarchical Circuit class.  Intended to be extended
 * by different circuit structures, such as {@link CircuitGraph}, 
 * {@link com.avlsi.file.aspice.AspiceCell}, 
 * {@link com.avlsi.tools.lvs.NetGraph}, 
 * {@link com.avlsi.tools.aspice.JaspiceCircuit}, etc., so common parsing 
 * code can be used to construct them.
 *
 * @author Mike Davies, based on AspiceFile code by Jesse Rosenstock.
 * @version $Revision$ $Date$
 **/

public abstract class AbstractCircuit {

    //
    // Data members
    //

    /**
     * Cell type
     **/
    private final String type;

    /**
     * Map from subcell instantiation name (String) to AbstractCircuit
     **/
    private final Map subcellUseMap = new HashMap();

    /**
     * Set of global names used in this cell or subcells
     **/
    private final Set globalsSet = new HashSet();

    /**
     * Namespace alias map.  Map from set of names to null object.
     * Names may be in this cell or of the form sub.name for nodes
     * defined in subcells.
     **/
    private static class DummyMerge
            implements AliasedMap.MergeFunction, java.io.Serializable {
        public Object merge(final Object a, final Object b) {
            if (a == null)
                return null;
            else if (b == null)
                return a;
            else {
                Debug.assertTrue(a == EXISTS);
                Debug.assertTrue(b == EXISTS);
                return EXISTS;
            }
        }
    }
        
    protected final AliasedMap asm
        = new AliasedMap(new DummyMerge(), HierName.getComparator());/*new MergeFunction() {
            public Object merge(final Object a, final Object b) {
                if (a == null)
                    return null;
                else if (b == null)
                    return a;
                else {
                    Debug.assertTrue(a == EXISTS);
                    Debug.assertTrue(b == EXISTS);
                    return EXISTS;
                }
            }
        }, HierName.getComparator());*/

    private static final SerializedObject EXISTS = new SerializedObject();

    private static class SerializedObject extends Object 
                                   implements java.io.Serializable {}

    protected Repository repository;
    //
    // Constructor
    //

    /**
     * Constructs a circuit of the specified type.
     **/

    public AbstractCircuit(final String type) {
        this.type = type;
        repository = null;
    }

    // 
    // General class methods
    //

    /**
     * Returns circuit's cell type.
     **/
    public String getType() {
        return type;
    }

    // 
    // Subcircuit routines
    //

    /**
     * Adds an instantiation of a subcell.
     **/
    public void addSubcell(final String subName, final AbstractCircuit defn) {
        final Object o = subcellUseMap.put(subName, defn);
        if (o != null)
            throw new RuntimeException("Duplicate instantiation of '"+subName
                                       +"'. Old instantiation replaced.");
    }

    /**
     * Returns the definition of the specified instantiated subcell.
     **/
    public AbstractCircuit getSubcellDefForName(final String name) {
        return (AbstractCircuit) subcellUseMap.get(name);
    }
    
    /**
     * Returns an Iterator of HierNames referencing the circuit's subcells.
     **/
    public Iterator getSubcellNames() {
        return subcellUseMap.keySet().iterator();
    }

    /**
     * Returns a cell type name for the specified subcell, or null
     * if it doesn't exist.
     **/
    public String getSubcellTypeForName(final String name) {
        final AbstractCircuit a = getSubcellDefForName(name);
        if (a == null)
            return null;
        else
            return a.getType();
    }

    /**
     * Clears the subcell list (used after a flatten)
     **/
    public void removeAllSubcells() {
        subcellUseMap.clear();
    }

    //
    // Device creation & access methods.
    //

    /**
     * Adds a diode to the circuit (to be defined by subclass).
     **/
    public abstract void addDiode(final DiodeInterface diode);

    /**
     * Adds a resistor to the circuit (to be defined by subclass).
     **/
    public abstract void addResistor(final ResistorInterface resistor);

    /**
     * Adds a capacitor to the circuit (to be defined by subclass).
     **/
    public abstract void addCapacitor(final CapacitorInterface capacitor);

    /**
     * Adds a transistor to the circuit (to be defined by subclass).
     **/
    public abstract void addTransistor(final TransistorInterface transistor);

    public void addSource(final SourceInterface source)
            throws AbstractCircuit.Exception{
        throw new AbstractCircuit.Exception("Sources from Spice Not implemented");
    }
    
    /**
     * Returns the number of diodes in the circuit (to be defined by subclass).
     **/
    public abstract int getDiodeCount();

    /**
     * Returns the number of resistors in the circuit
     * (to be defined by subclass).
     **/
    public abstract int getResistorCount();

    /**
     * Returns the number of capacitors in the circuit
     * (to be defined by subclass).
     **/
    public abstract int getCapacitorCount();

    /**
     * Returns the number of MOSFET transistors in the circuit
     * (to be defined by subclass).
     **/
    public abstract int getTransistorCount();

    //
    // Namespace related methods
    //

    /**
     * Adds a cell or node name to the circuit
     **/
    public void addName(final HierName name) 
    {
        try {
            if (name.isGlobal())
                addGlobal(name);
            asm.addData(name, EXISTS);
        } catch (AliasedMap.MergeFailedException e) {
            throw new AssertionFailure(e);
        }
    }

    /**
     * Connects two names, adding them if they don't exist in the circuit.
     **/
    public void aliasNames(final HierName name1, final HierName name2) 
    {
        try {
            if (name1.isGlobal())
                addGlobal(name1);
            if (name2.isGlobal())
                addGlobal(name2);
            System.out.println("Making connection: "+name1+" to "+name2);
            asm.makeEquivalent(name1, name2);
        } catch (AliasedMap.MergeFailedException e) {
            throw new AssertionFailure(e);
        }
    }

    /**
     * Returns an iterator over the circuit's canonical HierNames.
     **/
    public Iterator getCanonicalNames() {
        return asm.getCanonicalKeys();
    }

    /**
     * Returns an Iterator over the HierName equivalency set of
     * the specified name.  The specified name is included in the list.
     **/
    public Iterator getEquivalentNames(final HierName name) {
        return asm.getAliases(name);
    }

    /**
     * Adds name to the set of global names.
     **/
    protected void addGlobal(final HierName name) {
        globalsSet.add(name);
    }

    /**
     * Returns an Iterator of HierName, the globals in this cell or
     * its subcells.
     **/
    public Iterator getGlobals() {
        return globalsSet.iterator();
    }

    /**
     * Returns the Repository containing circuits defined locally in this
     * circuit.
     **/
    public Repository getRepository() {
        return repository;
    }

    /*****************************************************************
     * Subcell repository class.  Defines a subcell type to definition
     * map.  Also defines an (abstract) AbstractCircuit generator 
     * method, to be used by parsing code to generate new subcells
     * of the appropriate AbstractCircuit subtype.
     *****************************************************************/

    public abstract static class Repository {

        /**
         * CellType (String) -> cellDefinition (AbstractCircuitGenerator) map.
         **/
        private final HashMap repos = new HashMap();

        /**
         * AbstractCircuit factory abstract method.
         **/
        protected abstract AbstractCircuit 
            newCircuitGenerator(final String cellType);

        /**
         * Returns a new empty AbstractCircuit and adds it to the
         * repository map.
         **/
        public AbstractCircuit newCell(final String cellType)
            throws AbstractCircuit.Exception {
            AbstractCircuit abstractCell = newCircuitGenerator(cellType);
            Object exists = repos.put(cellType,abstractCell);
            if (exists != null) 
                throw new AbstractCircuit.Exception("Cell '"+cellType+
                                                    "' redefined.");
            return abstractCell;
        }

        /**
         * Returns the set of all cell types in the repository.
         * @return Set of cell type Strings.
         **/
        public Set getCellTypesSet() {
            return repos.keySet();
        }

        /**
         * Returns an iterator over all defined cell types in the repository.
         * @return Iterator over a set of String cell type names.
         **/
        public Iterator getCellTypesIterator() {
            return repos.keySet().iterator();
        }

        /**
         * Returns the definition of the specified cell from the repository.
         * @return Specified cell type definition, or null undefined.
         **/
        public AbstractCircuit getCell(String cellType) {
            return (AbstractCircuit) repos.get(cellType);
        }
    }

    /*****************************************************************
     * AbstractCircuit exception class
     *****************************************************************/
    public static class Exception extends java.lang.Exception { 
        public Exception() { super(); }
        public Exception(final String desc) { super(desc); }
        public Exception(final Throwable cause) { super(cause); }
    }
}
