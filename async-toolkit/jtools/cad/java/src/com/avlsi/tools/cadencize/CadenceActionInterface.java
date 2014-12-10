/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.cadencize;

import java.util.Iterator;

/**
 * A class implementing this interface provides actions to be
 * executed as a CadenceInfoInterface is traversed.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public interface CadenceActionInterface {

    /**
     * Action to be executed on the name of the type of the cell.
     **/
    void doType(String typeName);

    /**
     * Action to be executed once for each canonical name in
     * the port list.
     **/
    void doPort(String portName);

    /**
     * Action to be executed once for each canonical name in
     * the local list.
     **/
    void doNet(String portName);

    /**
     * Action to be executed once for each subcell instantiation.
     **/
    void doSubcell(String subcellName, String subcellType);

    /**
     * Action to be executed once for each connection between a
     * net and a port of this cell.
     **/
    void doPortConnection(String netName, String portName);

    /**
     * Action to be executed once for each connection between a
     * net and a port on a subcell.
     **/
    void doSubcellConnection(String netName,
            String subcellName, String subcellPortName,
            String subcellNetName);

    /**
     * Action to be executed once for each netName.
     *
     * @param netName  the name of the net
     * @param netName  an unmodifiable Iterator of Strings of the
     *    aliases of the net
     **/
    void doAliases(String netName, Iterator aliases);

}
