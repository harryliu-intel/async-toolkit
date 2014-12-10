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
import com.avlsi.fast.metaparameters.FloatMetaParam;
import com.avlsi.fast.metaparameters.MetaParamTypeInterface;

/**
 * This class represents floating point values.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class FloatValue
    extends Value
    implements FloatMetaParam, MetaParamValueInterface {

    /**
     * Value, a double.
     **/
    private double val;

    /**
     *  Returns a FloatValue whose value is equal to that of the specified
     *  double.
     **/
    public static FloatValue valueOf(double val) {
        return new FloatValue(val);
    }

    /**
     *  Returns an FloatValue whose value is equal to that of the specified
     *  Value.  Throws InvalidOperationException if the value is not
     *  an FloatValue or an IntValue.
     **/
    public static FloatValue valueOf(Value v)
        throws InvalidOperationException {
        if (v instanceof IntValue)
            return valueOf(((IntValue) v).doubleValue());
        if (v instanceof FloatValue)
            return (FloatValue) v;
        else
            throw new InvalidOperationException
                (v.getType().getString() + " is not an integer or a float");
    }
    
    public static FloatValue castFrom(final Value v)
        throws InvalidOperationException {
        if (v instanceof IntValue || v instanceof FloatValue)
            return valueOf(v);
        else if (v instanceof BoolValue)
            return valueOf(((BoolValue) v).getValue() ? 1 : 0);
        else
            throw new InvalidOperationException(
                "Cannot cast " + v.getType().getString() + " to " +
                TYPE.getString());
    }

    public static FloatValue valueOf(final String s)
        throws InvalidOperationException {
        try {
            return valueOf(Double.parseDouble(s));
        } catch (NumberFormatException e) {
            throw new InvalidOperationException("bad float val;"
                    + e.getMessage(), e);
        }
    }

    /**
     * Class constructor.
     **/
    public FloatValue(double val) {
        super(true);
        this.val = val;
    }

    public FloatValue() {
        super(false);
        this.val = Double.NaN;
    }

    public double getValue() throws InvalidOperationException {
        ensureDefined();
        return val;
    }

    public Value duplicate() {
        if (isDefined())
            return new FloatValue(val);
        else
            return new FloatValue();
    }


    // arithmetic

    public Value add      (final Value v) throws InvalidOperationException {
        return valueOf(getValue() + valueOf(v).getValue());
    }

    public Value subtract (final Value v) throws InvalidOperationException {
        return valueOf(getValue() - valueOf(v).getValue());
    }

    public Value multiply (final Value v) throws InvalidOperationException {
        return valueOf(getValue() * valueOf(v).getValue());
    }

    public Value divide   (final Value v) throws InvalidOperationException {
        return valueOf(getValue() / valueOf(v).getValue());
    }

    public Value unaryPlus()              throws InvalidOperationException {
        return valueOf(getValue());
    }

    public Value negate   ()              throws InvalidOperationException {
        return valueOf(-getValue());
    }


    // relational, screwey floating point ordering, but that's hidden

    public Value lt       (final Value v) throws InvalidOperationException
        { return BoolValue.valueOf(getValue() <  valueOf(v).getValue()); }
    public Value le       (final Value v) throws InvalidOperationException
        { return BoolValue.valueOf(getValue() <= valueOf(v).getValue()); }
    public Value eq       (final Value v) throws InvalidOperationException
        { return BoolValue.valueOf(getValue() == valueOf(v).getValue()); }
    public Value ne       (final Value v) throws InvalidOperationException
        { return BoolValue.valueOf(getValue() != valueOf(v).getValue()); }
    public Value ge       (final Value v) throws InvalidOperationException
        { return BoolValue.valueOf(getValue() >= valueOf(v).getValue()); }
    public Value gt       (final Value v) throws InvalidOperationException
        { return BoolValue.valueOf(getValue() >  valueOf(v).getValue()); }

    public Value assign(final Value v, final CellImpl cell)
        throws InvalidOperationException {
        if (isDefined())
            throw new InvalidOperationException("value already assigned");

        // throw InvalidOperationException if v not defined
        val = valueOf(v).getValue();
        setDefined();

        return this;
    }   
    
    public static final Type TYPE = new ClassType("float");

    public Type getType() {
        return TYPE;
    }

    public String toString() {
        return "FloatValue("
            + (isDefined() ? String.valueOf(val) : "?")
            + ")";
    }

    /**
     * Two FloatValues are equal if they are both defined,
     * and have the same value.
     **/
    public boolean equals(final Object o) {
        if (!(o instanceof FloatValue))
            return false;
        else
            return equals((FloatValue) o);
    }

    /**
     * Two FloatValues are equal if they are both defined,
     * and have the same value.
     **/
    public boolean equals(final FloatValue fv) {
        if (!isDefined())
            return false;
        else if (!fv.isDefined())
            return false;
        else
            return val == fv.val;
    }

    public int hashCode() {
        return (int) (Double.doubleToRawLongBits(val) & 0xFFFFFFFFL);
    }

    //
    // implements FloatMetaParam
    //

    public float toFloat() {
        return (float) val;
    }

    //
    // implements MetaParamValueInterface
    //

    public MetaParamTypeInterface toMetaParam() {
        return this;
    }

}
