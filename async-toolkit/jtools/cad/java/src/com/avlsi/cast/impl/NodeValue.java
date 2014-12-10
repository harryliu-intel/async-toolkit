/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.cast.impl;

import com.avlsi.cell.CellImpl;
import com.avlsi.cell.CellInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.util.debug.Debug;

/**
 * Class to represent node values.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class NodeValue extends Value {

    /**
     * Don't use static initializers because they are evil.
     **/
    private static CellInterface NODE_CELL = null;

    /**
     * The name of the variable that this node value represents.
     * Used for connections.
     **/
    private final HierName instanceName;

    // maybe this will have an Alias reference or something someday?

    public static NodeValue valueOf(final Value val)
        throws InvalidOperationException {
        if (val instanceof NodeValue)
            return (NodeValue) val;
        else
            throw new InvalidOperationException(val + " is not a node");
    }

    public static NodeValue castFrom(final Value v)
        throws InvalidOperationException {
        if (v instanceof NodeValue)
            return (NodeValue) v;
        else
            throw new InvalidOperationException(
                "Cannot cast " + v.getType().getString() + " to " +
                TYPE.getString());
    }

    public NodeValue(final HierName instanceName) {
        super(true);

        this.instanceName = instanceName;
    }

    public Value duplicate() {
        // What do we want to do here?
        Debug.assertTrue(isDefined());
        return new NodeValue(instanceName);
    }

    public Value assign(final Value v, final CellImpl cell)
        throws InvalidOperationException {
        Debug.assertTrue(isDefined());
        final NodeValue nv = valueOf(v);
        cell.addConnection(getInstanceName(), nv.getInstanceName());
        return this;
    }

    public HierName getInstanceName() {
        return instanceName;
    }

    public Value newInstanceName(final HierName newInstanceName) {
        Debug.assertTrue(isDefined());
        return new NodeValue(newInstanceName);
    }

    public static final Type TYPE = new ClassType("node");

    public Type getType() {
        return TYPE;
    }

    public String toString() {
        return "NodeValue(" + getInstanceName() + ")";
    }

    public CellInterface getCell() {
        if (NODE_CELL == null)
            NODE_CELL = new CellImpl(CellImpl.NODE_TYPE);

        return NODE_CELL;
    }

    public boolean eventuallyRefinesFrom(Value v)
        throws InvalidOperationException {
        if (! (v instanceof NodeValue))
            throw new InvalidOperationException("Can't refine an node value from " + v.getType().getString());
        return true;            // All nodes are the same; no problem
    }
}
