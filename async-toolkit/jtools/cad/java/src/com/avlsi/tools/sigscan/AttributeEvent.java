/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.sigscan;

/**
 * Class for adding attribute events to the scheduler
 *
 * @author Dan Daly
 * @version $Date$
 **/

class AttributeEvent extends LogEvent {

    protected final Attribute attr;

    protected final AttributeValue value;
    
    /**
     * Constructor.
     **/
    AttributeEvent(Attribute attr, AttributeValue value, long time) {
        super(time);
        this.attr = attr;
        this.value = value;
    }

    protected void actionPerformed() { attr.set(value); }
    
    public String toString() { 
        return "AttributeEvent: "+attr+" set to "+value+" @ "+time; }

}

