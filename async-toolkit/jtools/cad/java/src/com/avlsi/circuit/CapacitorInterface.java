// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

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
 * Interface specifying required methods of a capacitor passed to
 * AbstractCircuit
 *
 * @author Dan Daly
 * @version $Date$
 **/

public interface CapacitorInterface{
        
    /** Source Node **/
    HierName getSource();
    
    /** Drain Node **/
    HierName getDrain();
    
    /** (optional) name of Capacitor, null if not specified **/
    HierName getName();

    /** Get the Capacitance of the device, in Farads**/
    double getCapacitance();

}

