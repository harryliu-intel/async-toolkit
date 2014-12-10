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
import com.avlsi.fast.metaparameters.BooleanMetaParam;
import com.avlsi.fast.metaparameters.MetaParamTypeInterface;

/**
 * This class represents boolean values.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class BoolValue
    extends Value
    implements BooleanMetaParam, MetaParamValueInterface {

    /**
     * Value, a boolean.
     **/
    private boolean val;

    /**
     *  Returns a BoolValue whose value is equal to that of the specified
     *  boolean.
     **/
    public static BoolValue valueOf(final boolean val) {
        return new BoolValue(val);
    }

    /**
     *  Returns a BoolValue whose value is equal to that of the specified
     *  Value.  Thrown InvalidOperationException if the value is not
     *  an BoolValue
     **/
    public static BoolValue valueOf(final Value v)
        throws InvalidOperationException {
        if (v instanceof BoolValue)
            return (BoolValue) v;
        else
            throw new InvalidOperationException(v + " is not a bool");
    }

    public static BoolValue castFrom(final Value v)
        throws InvalidOperationException {
        if (v instanceof BoolValue)
            return (BoolValue) v;
        else if (v instanceof IntValue)
            return valueOf(((IntValue) v).getValue().compareTo(java.math.BigInteger.ZERO) != 0);
        else if (v instanceof FloatValue)
            return valueOf(((FloatValue) v).getValue() != 0);
        else
            throw new InvalidOperationException(
                "Cannot cast " + v.getType().getString() + " to " +
                TYPE.getString());

    }

    /**
     * Class constructor.
     **/
    private BoolValue(final boolean val) {
        super(true);
        this.val = val;
    }

    /**
     * Class constructor, constructs an undefined BoolValue.
     **/
    public BoolValue() {
        super(false);
        this.val = false;
    }

    public Value duplicate() {
        if (isDefined())
            return new BoolValue(val);
        else
            return new BoolValue();
    }

    /**
     * Returns the value, throws InvalidOperationException if
     * value is not defined.
     **/
    public boolean getValue() throws InvalidOperationException {
        ensureDefined();
        return val;
    }

    // logical
    public Value or       (final Value v) throws InvalidOperationException
        { return valueOf(getValue() | valueOf(v).getValue()); }
    public Value and      (final Value v) throws InvalidOperationException
        { return valueOf(getValue() & valueOf(v).getValue()); }
    public Value not      ()              throws InvalidOperationException
        { return valueOf(!getValue()); }
    public Value xor      (final Value v) throws InvalidOperationException 
        { return valueOf(getValue() ^ valueOf(v).getValue()); }


    // relational, we only want eq, ne

    public Value eq       (final Value v) throws InvalidOperationException
        { return valueOf(getValue() == valueOf(v).getValue()); }
    public Value ne       (final Value v) throws InvalidOperationException
        { return valueOf(getValue() != valueOf(v).getValue()); }

    public Value assign(final Value v, final CellImpl cell)
        throws InvalidOperationException {
        if (isDefined())
            throw new InvalidOperationException("value already assigned");

        // throw InvalidOperationException if v not defined
        val = valueOf(v).getValue();
        setDefined();

        return this;
    }

    
    public static final Type TYPE = new ClassType("bool");

    public Type getType() {
        return TYPE;
    }

    public String toString() {
        return "BoolValue("
            + (isDefined() ? String.valueOf(val) : "?")
            + ")";
    }

    /**
     * Two BoolValues are equal if they are both defined,
     * and have the same value.
     **/
    public boolean equals(final Object o) {
        if (!(o instanceof BoolValue))
            return false;
        else
            return equals((BoolValue) o);
    }

    /**
     * Two BoolValues are equal if they are both defined,
     * and have the same value.
     **/
    public boolean equals(final BoolValue bv) {
        if (!isDefined())
            return false;
        else if (!bv.isDefined())
            return false;
        else
            return val == bv.val;
    }

    public int hashCode() {
        return val ? 1 : 0;
    }

    //
    // implements BooleanMetaParam
    //

    public boolean toBoolean() {
        return val;
    }

    //
    // implements MetaParamValueInterface
    //

    public MetaParamTypeInterface toMetaParam() {
        return this;
    }
}
