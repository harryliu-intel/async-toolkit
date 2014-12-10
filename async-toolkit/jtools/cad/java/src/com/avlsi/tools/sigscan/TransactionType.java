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

package com.avlsi.tools.sigscan;

/**
 * Class for wrapping up SST TransactionTypes
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class TransactionType extends NativeHandle{

    /*This kind has a definite begin and end */
    public static final int KIND_BEGINEND = Sigscan.KIND_BEGINEND;
    /*This kind shows up as an error, and is instant */
    public static final int KIND_EVENT = Sigscan.KIND_EVENT;
    /*Another kind of instant transaction, signifying an event */
    public static final int KIND_ERROR = Sigscan.KIND_ERROR;

    protected final Sigscan sigscan;
    
    protected final TransactionFiber fiber;
    
    protected final int kind;
    
    protected final String name;
    
    /**
     * Constructor.
     * @param name The name of this TransactionType
     * @param trankind The kind of this type, like BEGINEND or ERROR
     * @param fiber The fiber this type resides on
     **/
    public TransactionType(String name, int trankind,
                           TransactionFiber fiber) {
        super((fiber.getSigscan() != null)?
              fiber.getSigscan().
              PRIVATEnewTransactionType(name, trankind, fiber):
              0);
        this.fiber = fiber;
        this.sigscan = fiber.getSigscan();
        this.kind = trankind;
        this.name = name;
    }

    /** Returns the native address of this TransactionType object.
     * Probably shouldn't be called ever by the user.
     **/
    public long getHandle() { return handle; }

    /**Adds an attribute to this type.
     * @param attr The attribue to add to this type
     **/
    public void addAttribute(Attribute attr) {
        attr.attach(this);
    }

    /** @return The sigscan that holds this type */
    public Sigscan getSigscan() { return sigscan; }

    /** @return The fiber that this tran type is on **/
    public TransactionFiber getFiber() { return fiber; }

    /** @return the type's kind **/
    public int getKind() { return kind; }

    public String toString() {
        String skind = "UNKNOWN";
        switch (kind) {
          case KIND_BEGINEND:   skind = "BEGINEND";
                                break;
          case KIND_ERROR:   skind = "ERROR";
                                break;
          case KIND_EVENT:   skind = "EVENT";                          
                                break;
        }
        return name+" ("+skind+")";
    }
}

