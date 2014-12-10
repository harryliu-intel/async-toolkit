/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.sigscan;

public abstract class Attribute {

    //
    //The different types of formatting to be displayed
    //
    public static final String HEX          = "'x";
    public static final String OCT          = "'o";
    public static final String DEC          = "'d";
    public static final String UNSIGNED_DEC = "'s";
    public static final String ASC          = "'a";
    public static final String BIN          = "'b";
    public static final String DEFAULT      =  HEX;

    //
    //The different types of attributes, damn primitives
    //
    
    protected static final int BOOLEAN  = 2; 
    protected static final int STRING   = 4; 
    protected static final int INT      = 5; 
    protected static final int LONG     = 6; 

    /* The database of the attribute */
    protected final Sigscan sigscan;

    /* The transaction type enclosing this attribute */
    protected TransactionType trantype=null;
    /* The native address to the attribute */
    protected NativeHandle handle;
    
    /* The name of the attribute */
    protected final String name;
    
    /* The type of attribute */
    protected final int type;

    /* The debugging options for this attribute */
    protected final DebugOpts opts;
    
    /* The display format for this attribute */
    protected final String formatString;

    /* The lsb is the lower bounds of an arrayed attribute */
    protected final int lsb;
    
    /* The msb is the lower bounds of an arrayed attribute */
    protected final int msb;
    
    /**
     * Constructor for an attribute.
     * This constructor is not recommended, use one that
     * sets the sigscan database.
     * @param name The name of the attribute
     * @param type The type of the attribute
     **/
    public Attribute(String name,int type) {
        this.name = name;
        this.type = type;
        this.sigscan = null;
        this.opts = new DebugOpts();
        this.lsb = 0;
        this.msb = 0;
        this.formatString = DEFAULT;
        //this(name, type, null, new DebugOpts());
    }

    /**
     * Constructor, use when you don't want to bind the attribute
     * immediately.  Otherwise, not recommended.
     * @param name The name of the attribute
     * @param type The type of the attribute
     * @param sigscan The database where the attribute resides
     * @param opts The debugging options for this attribute
     **/
    public Attribute(String name, int type, Sigscan sigscan, DebugOpts opts) {
        this.name = name;
        this.type = type;
        this.sigscan = sigscan;
        this.opts = opts;
        this.lsb = 0;
        this.msb = 0;
        this.formatString = DEFAULT;
    }

    /** Recommended Constructor, will build the attribute and bind it to the
     * TransactionType <code>trantype</code>.
     * @param name The name of the attribute
     * @param type The type of the attribute
     * @param trantype The TransactionType to bind this attribute to.
     * @param opts The debugging options for this attribute
     **/
    public Attribute(String name, int type,
                     TransactionType trantype, DebugOpts opts) {
        this(name, type, trantype, opts, DEFAULT, 0, 0);
    }
    
    /** Recommended Constructor, will build the attribute and bind it to the
     * TransactionType <code>trantype</code>.
     * @param name The name of the attribute
     * @param type The type of the attribute
     * @param trantype The TransactionType to bind this attribute to.
     * @param opts The debugging options for this attribute
     * @param lsb The least sig digit (for arrayed attributes)
     * @param msb The most sig digit (for arrayed attributes)
     **/
    public Attribute(String name, int type,
                     TransactionType trantype, DebugOpts opts,
                     int lsb, int msb) {
        this(name, type, trantype, opts, DEFAULT, lsb, msb);
    }
    
    /** Recommended Constructor, will build the attribute and bind it to the
     * TransactionType <code>trantype</code>.  Also, can specify the
     * formatting for this attribute by passing a formatString, such as
     * HEX, DEC, BIN, ASC, etc
     * @param name The name of the attribute
     * @param type The type of the attribute
     * @param trantype The TransactionType to bind this attribute to.
     * @param opts The debugging options for this attribute
     * @param format The format for displaying this attribute
     * @param lsb The least sig digit (for arrayed attributes)
     * @param msb The most sig digit (for arrayed attributes)
     **/
    public Attribute(String name, int type,
                     TransactionType trantype, DebugOpts opts,
                     String formatString, int lsb, int msb) {
        this.name = name;
        this.type = type;
        this.sigscan = trantype.getSigscan();
        this.opts = opts;
        this.formatString = formatString;
        this.lsb = lsb;
        this.msb = msb;
        attach(trantype);
        this.trantype = trantype;
    }

    /** Attaches the attribute to the target TransactionType
     * @param trantype The target TransactionType
     **/
    public void attach(TransactionType trantype) {
        handle = (sigscan != null)?
                 sigscan.attachAttribute(this, trantype):
                 null;
    }
    
    /** Set this attribute value
     * @param v The AttributeValue to set this to
     * @see AttributeValue
     **/
    public abstract void set(AttributeValue v);

    /** Will set the attribute or schedule it depending on the sim
     * time compared to <code>time</code>
     * @param value The value to set it to
     * @param time The time to set it at
     **/
    public void set(AttributeValue value, long time) {
        if (sigscan != null) { sigscan.setAttribute(this, value, time); }
    }
    
    /**
     * @return The value held in the attribute, use getType() to find
     * out what type of primitive it is
     **/
    public abstract AttributeValue getValue();
    
    /** @return the integer type value */
    public int getType() { return type; }

    /** @return the transaction type for the attribute */
    public TransactionType getTranType() { return trantype; }

    /** @return The Attribute name **/
    public String getName() { return name; }

    /** @return The native address, should not be called by the user **/
    public long getHandle() { return handle.getHandle(); }

    /** @return The attribute's format, default is HEX **/
    public String getFormat() { return formatString; }

    /** @return The attribute's lsb, used for attributes that are arrays **/
    public int getLeastSigDigit() { return lsb; }
    
    /** @return The attribute's msb, used for attributes that are arrays **/
    public int getMostSigDigit() { return msb; }
    
}

