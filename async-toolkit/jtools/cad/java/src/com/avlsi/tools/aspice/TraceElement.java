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

package com.avlsi.tools.aspice;

/**
 * Class for tracing different parameters within the nodes
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class TraceElement {

    private int type;
    private Object o;
    /**
     * Constructor for TraceElement
     **/
    public TraceElement(final int type, final Object o) {
        this.type = type;
        this.o = o;
    }

    public double getData() {
        if ((type == Node.VOLTAGE) &&
            (o instanceof Node)) return ((Node) o).getVoltage();
            
        if (!(o instanceof AbstractDevice)) return -1;
        
        return ((AbstractDevice) o).getCurrent(type);
    }

    public Object getObject() { return o; }
    
    public int getType() { return type; }

    public String getName() {
        if (type == Node.VOLTAGE) {
            return ((Node) o).getName().toString();
        } else {
            AbstractDevice ad = (AbstractDevice) o;
            return ad.getName() + "_" + AbstractDevice.names[type];
        }
    }
    
    public String printElement() {
        String sname = "Unknown object type";
        String stype = "Unknown type";
        if ( getObject() instanceof Node) {
            sname = ((Node) o).getName().toString();
            stype = "V";//Node.names[getType()];
            return sname;
        } else if (getObject() instanceof AbstractDevice) {
            sname = ((AbstractDevice) o).getName().toString();
            stype = AbstractDevice.names[getType()];
            return sname+"_"+stype;
        }
        return sname;
    }
}

