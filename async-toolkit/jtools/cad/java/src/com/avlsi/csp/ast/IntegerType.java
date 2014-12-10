/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.csp.ast;

import java.math.BigInteger;

import com.avlsi.csp.util.CspUtils;
import com.avlsi.csp.util.Interval;

/**
 * Class for CSP Integer type
 *
 * @author David Hilvert
 * @version $Revision$ $Date$
 **/
public class IntegerType extends Type {
    /**
     * Is this type constant?
     **/
    private boolean is_const;

    /**
     * Is this type constant?
     **/
    private boolean is_signed;

    /**
     * User declared bit width.
     **/
    private final ExpressionInterface declaredWidth;

    /**
     * Interval containing all possible values of this type.
     **/
    private Interval interval;

    public IntegerType () {
        this(false);
    }

    public IntegerType (boolean is_const) {
        this(is_const, true, null);
    }

    public IntegerType (boolean is_const, ExpressionInterface declaredWidth) {
        this(is_const, false, declaredWidth);
    }

    public IntegerType (boolean is_const, boolean is_signed,
                        ExpressionInterface declaredWidth) {
        this.is_const = is_const;
        this.is_signed = is_signed;
        this.declaredWidth = declaredWidth;
        final BigInteger bw = CspUtils.getIntegerConstant(declaredWidth);
        if (bw == null) {
            this.interval = null;
        } else {
            if (bw.signum() != 1 || bw.bitLength() > 31) {
                final String loc = declaredWidth.getParseRange() == null ?
                    "" : " at " + declaredWidth.getParseRange().fullString();
                throw new IllegalArgumentException(
                        "Integer bit width invalid or too big: " + bw + loc);
            } else {
                this.interval = new Interval(is_signed, bw.intValue());
            }
        }
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitIntegerType(this);
    }

    /**
     * Indicates dimension of the type.  Scalars have dimension 0, arrays
     * N, where N > 0.
     **/
    public int dimension() {

        // An Integer is a scalar CSP type, so its dimension is 0.

        return 0;
    }

    /**
     * Indicates whether this integer type is a constant integer type.
     **/
    public boolean isConst() {
        return is_const;
    }

    /**
     * Indicates whether this integer type is signed.
     **/
    public boolean isSigned() {
        return is_signed;
    }

    public String toString() {
        return "int";
    }

    /**
     * Returns the declared bit width.
     **/
    public ExpressionInterface getDeclaredWidth() {
        return declaredWidth;
    }

    /**
     * Sets the interval containing all possible values of this type.  The
     * interval can only be set if this type does not have a declared bit
     * width.
     **/
    public void setInterval(final Interval interval) {
        if (declaredWidth == null) this.interval = interval;
    }

    /**
     * Returns the interval containing all possible values of this type.
     **/
    public Interval getInterval() {
        return interval;
    }
}
