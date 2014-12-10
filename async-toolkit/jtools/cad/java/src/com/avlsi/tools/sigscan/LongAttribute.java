/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.sigscan ;

/**
 * Class for integer attributes in Signalscan
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class LongAttribute extends Attribute {

    /** The primitive data **/
    private long l;
    
    /**
     * Constructor, use when you don't want to bind the attribute
     * immediately.  Otherwise, not recommended.
     * @param name The name of the attribute
     * @param sigscan The database where the attribute resides
     * @param opts The debugging options for this attribute
     **/
    public LongAttribute(String name,Sigscan sigscan, DebugOpts opts) {
        super(name, LONG, sigscan, opts);
    }
    
    /**Old Constructor, the sigscan value is ignored.
     * @param name The name of the attribute
     * @param sigscan ignored
     * @param trantype The TransactionType to bind this attribute to.
     * @param opts The debugging options for this attribute
     **/
    public LongAttribute(String name, Sigscan sigscan,
                         TransactionType trantype, DebugOpts opts) {
        super(name, LONG, trantype, opts);
    }

    /** Recommended Constructor, will build the attribute and bind it to the
     * TransactionType <code>trantype</code>.
     * @param name The name of the attribute
     * @param trantype The TransactionType to bind this attribute to.
     * @param opts The debugging options for this attribute
     **/
    public LongAttribute(String name,
                         TransactionType trantype,DebugOpts opts) {
        super(name, LONG, trantype,opts);
    }
    
    /** Sets the internal primitive data variable.  Will log
     * if sigscan valid and loggingTrans turned on
     * @param b The new value of the primitive
     **/
    public void set(long l) {
        this.l = l;
        if ((sigscan != null) &&
            (opts.loggingTrans())) sigscan.setLongAttribute(this, l); }

    /** Set this attribute value. Will log
     * if sigscan valid and loggingTrans turned on
     * @param v The AttributeValue to set this to
     * @see AttributeValue
     **/
    public void set(AttributeValue v) { set(v.getLong()); }

    /** Sets the internal primitive data variable.  Will log
     * if sigscan valid and loggingTrans turned on
     * @param l The new value of the primitive
     * @param time The time to set it at
     **/
    public void set(long l, long time) { set(new AttributeValue(l), time); }

    /** @return The primitive data variable wrapped in a AttributeValue
     * class
     */
    public AttributeValue getValue() {
        return new AttributeValue(l);
    }

    /** @return The long value of this attribute **/
    public long get() { return l; }

    
}

