/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.cast.impl;

import java.math.BigInteger;

import com.avlsi.cell.CellImpl;
import com.avlsi.fast.metaparameters.IntegerMetaParam;
import com.avlsi.fast.metaparameters.MetaParamTypeInterface;

/**
 * This class represents integer values.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class IntValue
    extends Value
    implements IntegerMetaParam, MetaParamValueInterface {

    /**
     * Value, an arbitary precision integer.
     **/
    private BigInteger val;

    /**
     *  Returns an IntValue whose value is equal to that of the specified
     *  BigInteger.
     **/
    public static IntValue valueOf(BigInteger val) {
        return new IntValue(val);
    }

    /**
     *  Returns an IntValue whose value is equal to that of the specified
     *  int.
     **/
    public static IntValue valueOf(int val) {
        return new IntValue(val);
    }

    /**
     *  Returns an IntValue whose value is represented by the specified 
     *  String.
     **/
    public static IntValue valueOf(final String s)
        throws InvalidOperationException {
        try {
            return valueOf(new BigInteger(s));
        } catch (NumberFormatException e) {
            throw new InvalidOperationException
                ("bad int;" + e.getMessage(), e);
        }
    }

    public static IntValue hexValueOf(final String s)
        throws InvalidOperationException {
        try {
            return valueOf(new BigInteger(s, 16));
        } catch (NumberFormatException e) {
            throw new InvalidOperationException
                ("bad int;" + e.getMessage(), e);
        }
    }

    /**
     *  Returns an IntValue whose value is equal to that of the specified
     *  Value.  Thrown InvalidOperationException if the value is not
     *  an IntValue.
     **/
    public static IntValue valueOf(Value v) throws InvalidOperationException {
        if (v instanceof IntValue)
            return (IntValue) v;
        else
            throw new InvalidOperationException(v.getType().getString() +
                                                " is not an integer");
    }

    public static IntValue castFrom(Value v) throws InvalidOperationException {
        if (v instanceof IntValue)
            return (IntValue) v;
        else if (v instanceof FloatValue)
            return valueOf((int) ((FloatValue) v).getValue());
        else if (v instanceof BoolValue)
            return valueOf(((BoolValue) v).getValue() ? 1 : 0);
        else
            throw new InvalidOperationException(
                "Cannot cast " + v.getType().getString() + " to " +
                TYPE.getString());
    }

    /**
     * Class constructor.
     **/
    public IntValue(int val) {
        super(true);
        this.val = BigInteger.valueOf(val);
    }

    public IntValue(BigInteger val) {
        super(true);
        this.val = val;
    }

    public IntValue() {
        super(false);
        this.val = null;
    }

    public Value duplicate() {
        if (isDefined())
            return new IntValue(val);
        else
            return new IntValue();
    }

    // accessor
    public double doubleValue() throws InvalidOperationException {
        return getValue().doubleValue();
    }

    public BigInteger getValue() throws InvalidOperationException {
        ensureDefined();
        return val;
    }

    // logical
    
    public Value or       (final Value v) throws InvalidOperationException {
        return valueOf(getValue().or(valueOf(v).getValue()));
    }
    
    public Value and      (final Value v) throws InvalidOperationException {
        return valueOf(getValue().and(valueOf(v).getValue()));
    }
    
    public Value not      ()              throws InvalidOperationException {
        return valueOf(getValue().not());
    }

    public Value xor      (final Value v) throws InvalidOperationException {
        return valueOf(getValue().xor(valueOf(v).getValue()));
    }
    
    // arithmetic

    public Value add      (final Value v) throws InvalidOperationException {
        if (v.getType().equals(FloatValue.TYPE))
            return v.add(this);
        else
            return valueOf(getValue().add(valueOf(v).getValue()));
    }

    public Value subtract (final Value v) throws InvalidOperationException {
        if (v.getType().equals(FloatValue.TYPE))
            return FloatValue.valueOf(this).subtract(v);
        else
            return valueOf(getValue().subtract(valueOf(v).getValue()));
    }

    public Value multiply (final Value v) throws InvalidOperationException {
        if (v.getType().equals(FloatValue.TYPE))
            return v.multiply(this);
        else
            return valueOf(getValue().multiply(valueOf(v).getValue()));
    }

    public Value divide   (final Value v) throws InvalidOperationException {
        if (v.getType().equals(FloatValue.TYPE))
            return FloatValue.valueOf(this).divide(v);
        else
            return valueOf(getValue().divide(valueOf(v).getValue()));
    }

    public Value mod      (final Value v) throws InvalidOperationException {
        try {
            return valueOf(getValue().mod(valueOf(v).getValue()));
        } catch (final ArithmeticException e) {
            throw new InvalidOperationException("can't mod by non-positive: "
                    + valueOf(v).getValue(), e);
        }
    }

    public Value pow      (final Value v) throws InvalidOperationException {
        try {
            return valueOf(getValue().pow(valueOf(v).getValue().intValue()));
        } catch (final ArithmeticException e) {
            throw new InvalidOperationException("can't exponentiate by " +
                    "negative: " + valueOf(v).getValue(), e);
        }
    }

    public Value unaryPlus()              throws InvalidOperationException {
        return valueOf(getValue());
    }

    public Value negate   ()              throws InvalidOperationException {
        return valueOf(getValue().negate());
    }


    // relational, total order

    public Value lt       (final Value v) throws InvalidOperationException {
        return BoolValue.valueOf(
                getValue().compareTo(valueOf(v).getValue()) <  0);
    }
    public Value le       (final Value v) throws InvalidOperationException {
        return BoolValue.valueOf(
                getValue().compareTo(valueOf(v).getValue()) <= 0);
    }
    public Value eq       (final Value v) throws InvalidOperationException {
        return BoolValue.valueOf(
                getValue().compareTo(valueOf(v).getValue()) == 0);
    }
    public Value ne       (final Value v) throws InvalidOperationException {
        return BoolValue.valueOf(
                getValue().compareTo(valueOf(v).getValue()) != 0);
    }
    public Value ge       (final Value v) throws InvalidOperationException {
        return BoolValue.valueOf(
                getValue().compareTo(valueOf(v).getValue()) >= 0);
    }
    public Value gt       (final Value v) throws InvalidOperationException {
        return BoolValue.valueOf(
                getValue().compareTo(valueOf(v).getValue()) >  0);
    }

    public Value assign(final Value v, final CellImpl cell)
        throws InvalidOperationException {
        if (isDefined())
            throw new InvalidOperationException("value already assigned");

        // this is ok because BigIntegers are immutable
        // throw InvalidOperationException if v not defined
        val = valueOf(v).getValue();
        setDefined();

        return this;
    }

    
    public static final Type TYPE = new ClassType("int");

    public Type getType() {
        return TYPE;
    }

    public String toString() {
        return "IntValue(" + (isDefined() ? String.valueOf(val) : "?") + ")";
    }

    /**
     * Two IntValues are equal if they are both defined, 
     * and have the same value.
     **/
    public boolean equals(final Object o) {
        if (!(o instanceof IntValue))
            return false;
        else
            return equals((IntValue) o);
    }

    /**
     * Two IntValues are equal if they are both defined, 
     * and have the same value.
     **/
    public boolean equals(final IntValue iv) {
        if (!isDefined())
            return false;
        else if (!iv.isDefined())
            return false;
        else
            return val.equals(iv.val);
    }

    public int hashCode() {
        if (val == null)
            return 0;
        else
            return val.hashCode();
    }

    //
    // implements IntegerMetaParam
    //

    public BigInteger toBigInteger() {
        return val;
    }

    //
    // implements MetaParamValueInterface
    //

    public MetaParamTypeInterface toMetaParam() {
        return this;
    }
}
