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
 * Class for ...
 *
 * @author Dan Daly
 * @version $Date$
 **/

public interface DiodeInterface {
    /** Get Diode Type **/
    int getType();
    /** Get the width of the transistor **/
    double getWidth();
    /** Get the length of the transistor **/
    double getLength();
    /** (optional) name of Diode, null if not specified **/
    HierName getName();

     /** Source Node **/
    HierName getSource();
    /** Drain Node **/
    HierName getDrain();

    /** Area of Diode **/
    double getArea();
    /** Perimeter of Diode **/
    double getPerimeter();


}

