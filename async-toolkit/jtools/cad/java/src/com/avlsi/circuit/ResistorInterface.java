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
 * Interface specifying required methods of a resistor passed to
 * AbstractCircuit
 *
 * @author Dan Daly
 * @version $Date$
 **/

public interface ResistorInterface {
    
    /** Source Node **/
    HierName getSource();
    
    /** Drain Node **/
    HierName getDrain();
    
    /** (optional) name of Resistor, null if not specified **/
    HierName getName();

    /** Conductance or inverse resitance of resistor in mho, or inverse ohm **/
    double getConductance();

}

