/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.sigscan ;

/**
 * Class for boolean attributes in Signalscan
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class BooleanAttribute extends StringAttribute {

    /** The primitive data **/
    private boolean b;
    
    /**
     * Constructor, use when you don't want to bind the attribute
     * immediately.  Otherwise, not recommended.
     * @param name The name of the attribute
     * @param sigscan The database where the attribute resides
     * @param opts The debugging options for this attribute
     **/
    public BooleanAttribute(String name,Sigscan sigscan, DebugOpts opts) {
        super(name, sigscan, opts);
    }

    /**Old Constructor, the sigscan value is ignored.
     * @param name The name of the attribute
     * @param sigscan ignored
     * @param trantype The TransactionType to bind this attribute to.
     * @param opts The debugging options for this attribute
     **/
    public BooleanAttribute(String name,Sigscan sigscan,
                            TransactionType trantype, DebugOpts opts) {
        super(name, trantype, opts);
    }

    /** Recommended Constructor, will build the attribute and bind it to the
     * TransactionType <code>trantype</code>.
     * @param name The name of the attribute
     * @param trantype The TransactionType to bind this attribute to.
     * @param opts The debugging options for this attribute
     **/
    public BooleanAttribute(String name,
                            TransactionType trantype, DebugOpts opts) {
        super(name, trantype, opts);
    }

    /** Sets the internal primitive data variable.  Will log
     * if sigscan valid and loggingTrans turned on
     * @param b The new value of the primitive
     **/
    public void set(boolean b) {
        this.b = b;
        if ((sigscan != null) &&
            (opts.loggingTrans())) {
            if (b) sigscan.setStringAttribute(this, "true");
            else   sigscan.setStringAttribute(this, "false");
        }
    }

    /** Sets the internal primitive data variable.  Will log
     * if sigscan valid and loggingTrans turned on
     * @param b The new value of the primitive
     * @param time The time to set it at
     **/
    public void set(boolean b, long time) { set(new AttributeValue(b), time); }

    /** Sets the internal primitive data variable.  Will log
     * if sigscan valid and loggingTrans turned on
     * @param v The new value of the primitive
     **/
    public void set(AttributeValue v) { set(v.getBoolean()); }

    /** @return The primitive data variable wrapped in a AttributeValue
     * class
     */
    public AttributeValue getValue() {
        return new AttributeValue(b);
    }

    /** @return The boolean value of this attribute **/
    public boolean getBoolean() { return b; }

    public String get() {
        if (b) return "true";
        return "false";
    }
    
}

