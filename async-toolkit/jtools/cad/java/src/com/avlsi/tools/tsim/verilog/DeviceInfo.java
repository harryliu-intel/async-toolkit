/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim.verilog;

import java.util.*;
import com.avlsi.tools.tsim.SharedBus;

/**
 * Class for ...
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class DeviceInfo {

    private final String scopename;
    private final String classname;
    
    private final Map params = new LinkedHashMap();
    private final ArrayList ports = new ArrayList();
    
    /**
     * Constructor.
     **/
    public DeviceInfo(String classname, String scopename) {
        this.classname = classname;
        this.scopename = scopename;
    }

    public void addParameter(String name, Object o) {
        params.put(name, o);
    }

    public void addPort(SharedBus bus) {
        ports.add(bus);
    }

    public boolean hasPorts() { return ports.size() > 0; }
    
    public boolean hasParameters() { return params.size() > 0; }

    public Collection getPorts() { return ports; }

    public Map getParameters() { return params; }

    public String getScopename() { return scopename; }

    public String getClassname() { return classname; }
    
}

