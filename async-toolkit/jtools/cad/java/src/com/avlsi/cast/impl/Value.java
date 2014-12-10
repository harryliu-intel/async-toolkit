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

package com.avlsi.cast.impl;

import com.avlsi.cell.CellImpl;
import com.avlsi.file.common.HierName;
import com.avlsi.util.exception.AssertionFailure;

/**
 * Each object of this class represents the value of an expression.
 * Every variable has a Value. Every value has a type. Each value type
 * corresponds to a subclass of Value: ie FloatValue, IntValue,
 * BooleanValue, etc.
 *
 * The default implementation of every operations just throws
 * InvalidOperationException. If a subclass wants to support the
 * operation, it should override the method.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public abstract class Value {

    /**
     * Specifies if the value is defined.
     **/
    private boolean definedP;

    /**
     * Class constructor, specifies if the value is defined.
     **/
    protected Value(final boolean definedP) { this.definedP = definedP; }

    /**
     * Tells if the value is defined.
     **/
    public boolean isDefined()  { return definedP; }

    /**
     * Throws InvalidOperationException if the value is not defined.
     **/
    public void ensureDefined() throws InvalidOperationException {
        if (!isDefined())
            throw new InvalidOperationException("Undefined value");
    }

    /**
     * Makes the value defined.
     **/
    protected void setDefined() {
        definedP = true;
    }

    /**
     * Returns a duplicate of the value.  Changes made to the duplicate
     * should not affect the original in any way.  This probably means a
     * deep copy.
     **/
    public abstract Value duplicate();

    // logical
    public Value or       (final Value v) throws InvalidOperationException
        { throw new InvalidOperationException(); }
    public Value and      (final Value v) throws InvalidOperationException
        { throw new InvalidOperationException(); }
    public Value not      ()              throws InvalidOperationException
        { throw new InvalidOperationException(); }
    public Value xor      (final Value v) throws InvalidOperationException
        { throw new InvalidOperationException(); }

    // arithmetic
    public Value add      (final Value v) throws InvalidOperationException
        { throw new InvalidOperationException(); }
    public Value subtract (final Value v) throws InvalidOperationException
        { throw new InvalidOperationException(); }
    public Value multiply (final Value v) throws InvalidOperationException
        { throw new InvalidOperationException(); }
    public Value divide   (final Value v) throws InvalidOperationException
        { throw new InvalidOperationException(); }
    public Value mod      (final Value v) throws InvalidOperationException
        { throw new InvalidOperationException(); }
    public Value pow      (final Value v) throws InvalidOperationException
        { throw new InvalidOperationException(); }
    public Value unaryPlus()              throws InvalidOperationException
        { throw new InvalidOperationException(); }
    public Value negate   ()              throws InvalidOperationException
        { throw new InvalidOperationException(); }

    // relational
    public Value lt       (final Value v) throws InvalidOperationException
        { throw new InvalidOperationException(); }
    public Value le       (final Value v) throws InvalidOperationException
        { throw new InvalidOperationException(); }
    public Value eq       (final Value v) throws InvalidOperationException
        { throw new InvalidOperationException(); }
    public Value ne       (final Value v) throws InvalidOperationException
        { throw new InvalidOperationException(); }
    public Value ge       (final Value v) throws InvalidOperationException
        { throw new InvalidOperationException(); }
    public Value gt       (final Value v) throws InvalidOperationException
        { throw new InvalidOperationException(); }
    
    // container
    // public Value accessField(Ident xxx)
    public Value accessArray(final SubscriptSpecInterface accessSpec)
        throws InvalidOperationException
        { throw new InvalidOperationException(); }

    public Value accessTuple(final int i) throws InvalidOperationException
        { throw new InvalidOperationException(); }

    // user defined type instantiation
    // would it make sense to do int/bool/float/... similiarly?
    // public InstanceValue instantiate(final TupleValue v1,
    public Value instantiate(final TupleValue v1,
                                     final TupleValue v2)
        throws InvalidOperationException {
        throw new InvalidOperationException();
    }

    /**
     * Assigns <code>v</code> to <code>this</code>.
     *
     * @param v  the value to assign
     * @param cell  the cell in which the assignment occured.
     *     This is needed if the assignment is of a <code>NodeValue</code>
     *     or <code>InstanceValue</code>, because a connection must
     *     be added to the connection list of the cell.
     **/
    public abstract Value assign   (final Value v, final CellImpl cell)
        throws InvalidOperationException;

    /**
     * Returns the instance name of the NodeValue, InstanceValue, 
     * or ArrayValue; otherwise returns null.  Ie "a[0].b[2].c.d"
     *
     * @design jmr  Consider a NamedValue wrapper class.  This 
     * could avoid constructing new instances just to change the name.
     **/
    public HierName getInstanceName() {
        return null;
    }

    /**
     * Returns a value alike in all ways, but with a different instance
     * name.  
     **/
    public Value newInstanceName(final HierName newInstanceName) {
        return this;
    }

    /**
     * Throws the exception if getType() isn't implemented on that
     * type of Value yet.
     **/
    public abstract Type getType() throws InvalidOperationException;

    /**
     * Returns true if this is v or refined from v or refined from a
     * Value which is refined from v or....  Meaningless for any Value
     * which doesn't correspond to a chunk of silicon.
     **/
    public boolean eventuallyRefinesFrom(Value v)
        throws InvalidOperationException {
        throw new InvalidOperationException("This value doesn't correspond to actual hardware: " + this);
    }
}
