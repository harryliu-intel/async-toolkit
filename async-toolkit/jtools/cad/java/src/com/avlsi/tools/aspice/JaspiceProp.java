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

package com.avlsi.tools.aspice ;

/**
 * Class for ...
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class JaspiceProp {

    private Object value;
    private final String units;
    private final String info;
    /**
     * Constructor.
     **/
    public JaspiceProp(Object value, String units,String info) {
        this.value = value;
        this.units = units;
        this.info = info;
    }

    public JaspiceProp(Object value) {
        this.value = value;
        this.units = "";
        this.info = "";
    }

    public Object getValue() { return value; }

    public String getUnits() { return units; }

    public void setValue(Object o) {
        this.value = o;
    }
    
    public String printProp(String key) {
        return " "+key+" : "+value+" ["+units+"]\n\t"+info;
    }
}

