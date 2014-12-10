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

package com.avlsi.tools.sigscan;

/**
 * Wrapper class for different attribute values
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class AttributeValue {

    //
    //Supported Types
    //

    private final Object voidstar;
    private final int type;
    /**
     * Constructor.
     **/
    public AttributeValue(String str) {
        voidstar = str;
        type = Attribute.STRING;
    }

    public AttributeValue(long l) {
        voidstar = new long[] { l };
        type = Attribute.LONG;
    }

    public AttributeValue(int i) {
        voidstar = new int[] { i };
        type = Attribute.INT;
    }

    public AttributeValue(boolean b) {
        voidstar = new boolean[] { b };
        type = Attribute.BOOLEAN;
    }

    public long getLong() {
        if (type != Attribute.LONG) {
            System.out.println("AttributeValue is not of type long, "+
                               " returning -999");
            return -999;
        }
        return ((long[]) voidstar)[0];
    }
    
    public int getInt() {
        if (type != Attribute.INT) {
            System.out.println("AttributeValue is not of type int, "+
                               " returning -999");
            return -999;
        }
        return ((int[]) voidstar)[0];
    }
    
    public String getString() {
        if (type != Attribute.STRING) {
            System.out.println("AttributeValue is not of type string, "+
                               " returning the empty string \"\"");
            return "";
        }
        return (String) voidstar;
    }

    public boolean getBoolean() {
        if (type != Attribute.BOOLEAN) {
            System.out.println("AttributeValue is not of type string, "+
                               " returning false");
            return false;
        }
        return ((boolean[]) voidstar)[0];
    }
}

