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

package com.avlsi.circuit;

import com.avlsi.file.common.HierName;

/**
 * Class for describing sources specified in spice
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class Source implements SourceInterface{
    /** Name of the source **/
    private HierName name;
    /** Positive terminal node name **/
    private HierName pname;
    /** Negative terminal node name **/
    private HierName nname;
    /** Source type **/
    private String type;
    /** Type specific arguments **/
    private String[] args;
    
    /**
     * Constructor.
     **/
    public Source(HierName name, String type, HierName pname, HierName nname, 
           String[] args) {
        this.name = name;
        this.type = type;
        this.pname = pname;
        this.nname = nname;
        this.args = args;
    }
    /** Type of the source, pulse, sin, etc **/
    public String getType() { return type; }
    /** (optional) Name of the Source, returns null if unassigned **/
    public HierName getName() { return name; }
    /** Positive Terminal node name **/
    public HierName getPositiveTerminal(){ return pname; }
    /** Negative Terminal node name **/
    public HierName getNegativeTerminal() { return nname; }
    /** Type specific arguments to the source  **/
    public String[] getArguments() { return args; }
}

