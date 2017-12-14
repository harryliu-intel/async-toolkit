/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.fulcrummicro.hw.verification.util.common;

/**
 * Utility class for formating strings/numbers/etc
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class Formatter {

    public static String formatInt(int i,int radix, int padTo) {
        String numstr = null;
        if (radix == 2)
            numstr = Integer.toBinaryString(i);
        else if (radix == 16)
            numstr = Integer.toHexString(i);
        else numstr = Integer.toString(i,radix);//Watch out, leaves signs
        while (numstr.length() < padTo)
            numstr = "0" + numstr;
        if (numstr.length() > padTo)
            numstr = numstr.substring(numstr.length()-padTo);
        return numstr;
    }
}

