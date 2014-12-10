/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.layout;

public class PinGlobal {

    public float pitch;
    public float width;
    public float spacing;
    
    public PinGlobal() {
    }

    public PinGlobal(float pitch, float width, float spacing) {
        this.pitch = pitch;
        this.width = width;
        this.spacing = spacing;
    }
    
    public String toSkillString() {
        return "( defvar PinGlobalWirePitch " + pitch + " )\n"
             + "( defvar PinGlobalWireWidth " + width + " )\n"
             + "( defvar PinGlobalWireSpacing " + spacing + " )\n";
    }

}
