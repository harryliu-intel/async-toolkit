/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.sigscan ;

/**
 * Class for integer attributes in SST
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class IntAttribute extends Attribute {

    /** The primitive data **/
    private int i;
    
    /**
     * Constructor, use when you don't want to bind the attribute
     * immediately.  Otherwise, not recommended.
     * @param name The name of the attribute
     * @param sigscan The database where the attribute resides
     * @param opts The debugging options for this attribute
     **/
    public IntAttribute(String name,Sigscan sigscan, DebugOpts opts) {
        super(name, INT, sigscan,opts);
    }

     /**Old Constructor, the sigscan value is ignored.
     * @param name The name of the attribute
     * @param sigscan ignored
     * @param trantype The TransactionType to bind this attribute to.
     * @param opts The debugging options for this attribute
     **/
    public IntAttribute(String name, Sigscan sigscan,
                        TransactionType trantype, DebugOpts opts) {
        super(name, INT, trantype, opts);
    }

     /** Recommended Constructor, will build the attribute and bind it to the
     * TransactionType <code>trantype</code>.
     * @param name The name of the attribute
     * @param trantype The TransactionType to bind this attribute to.
     * @param opts The debugging options for this attribute
     **/
    public IntAttribute(String name, TransactionType ttype, DebugOpts opts) {
        super(name, INT,ttype, opts);
    }

    /** Sets the internal primitive data variable.  Will log
     * if sigscan valid and loggingTrans turned on
     * @param b The new value of the primitive
     **/
    public void set(int i) {
        this.i = i;
        if ((sigscan != null) &&
            (opts.loggingTrans())) sigscan.setIntAttribute(this, i); }

    /** Set this attribute value. Will log
     * if sigscan valid and loggingTrans turned on
     * @param v The AttributeValue to set this to
     * @see AttributeValue
     **/
    public void set(AttributeValue v) { set(v.getInt()); }

    /** Set this attribute value. Will log
     * if sigscan valid and loggingTrans turned on
     * @param i The AttributeValue to set this to
     * @param time The time to set it at
     * @see AttributeValue
     **/
    public void set(int i,long time) { set(new AttributeValue(i),time); }
    
    /** @return The primitive data variable wrapped in a AttributeValue
     * class
     */
    public AttributeValue getValue() {
        return new AttributeValue(i);
    }

    /** @return The int value of this attribute **/
    public int get() { return i; }

}

