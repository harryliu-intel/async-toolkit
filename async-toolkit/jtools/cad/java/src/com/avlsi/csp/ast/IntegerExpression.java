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

/**
 * Integer literals. Ie <code>10</code>, <code>0xFF</code>,
 * or <code>8_77</code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class IntegerExpression
    extends AbstractASTNode
    implements ExpressionInterface {

    /** String representation of integer.  Not null. **/
    private final String val;

    /**
     * Base used to interpret value string.  Must be
     * <code>2 &lt;= radix &amp;&amp; radix &lt;= 36</code>.
     **/
    private final int radix;

    /**
     * Class constructor.  
     *
     * @param val String representation of number starting with
     *     optional minus sign.  May not be null.
     * @param radix Base for string between 2 and 36 inclusive.
     **/
    public IntegerExpression(final String val, final int radix) {
        this.val = val;
        this.radix = radix;
    }

    /**
     * Class constructor.  
     *
     * @param val int representation of number 
     **/
    public IntegerExpression(final int val) {
        this.val = String.valueOf(val);
        this.radix = 10;
    }

    /**
     * Returns string representation of integer.
     *
     * @return string representation, not null
     **/
    public String getValue() {
        return val;
    }

    /**
     * Returns radix for interpretation of value string.
     *
     * @return radix used to interpret value string
     **/
    public int getRadix() {
        return radix;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitIntegerExpression(this);
    }

    public String toString() {
        switch (radix) {
          case 2: return "0b" + val;
          case 10: return val;
          case 16: return "0x" + val;
          default: return radix + "_" + val;
        }
    }
}
