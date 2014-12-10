/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: //depot/sw/cad/java/main/src/com/avlsi/cell/RefinementException.java#2 $
 * $DateTime: 2002/02/23 11:33:05 $
 * $Author: chrisb $
 */

package com.avlsi.cell;

public class BadCSPPortException extends Exception {
    public BadCSPPortException(String cellName, String portName) {
        super("Couldn't make port " + portName +
              " accessible to the csp block in cell " + cellName);
    }
}
