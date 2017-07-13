/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.util;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

import com.avlsi.fast.metaparameters.MetaParamDefinition;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.cell.CellUtils;

/**
 * Contains information about the cell that the csp-&gt;java process
 * needs for creation and compilation of the java.  Includes all ports
 * which the csp connects to, but not necessarily all ports of the
 * total cell.
 *
 * Immutable.
 **/
public class CSPCellInfo {
    /**
     * The fully qualified type of the cell. 
     **/
    private final String type;

    /** Contains {@link com.avlsi.fast.metaparameters.MetaParamDefinition}s. **/
    private final Collection<MetaParamDefinition> metaParamDefinitions;

    /** Contains {@link com.avlsi.fast.ports.PortDefinition}s. **/
    private final Collection<PortDefinition> portDefinitions;

    public CSPCellInfo(final String type,
                       final Collection<MetaParamDefinition>
                           metaParamDefinitions,
                       final Collection<PortDefinition>
                           portDefinitions) {
        this.type = type;
        this.metaParamDefinitions = metaParamDefinitions;
        this.portDefinitions = portDefinitions;
    }

    public String getType() {
        return type;
    }

    public String getAbbreviatedType() {
        return CellUtils.hashMetaParameters(getType(), 10000);
    }
    
    public Iterator<MetaParamDefinition> getMetaParamDefinitions() {
        return Collections.unmodifiableCollection(metaParamDefinitions).iterator();
    }

    public Iterator<PortDefinition> getPortDefinitions() {
        return Collections.unmodifiableCollection(portDefinitions).iterator();
    }
}
