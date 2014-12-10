/*
 * Copyright 2012 Intel.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.lvs;

public class Staticizer {
    /** staticizer_type constants from standard/process.cast **/
    static final int NO_STATICIZER = -1;
    static final int WEAK_STATICIZER = 0;
    static final int FULL_STATICIZER = 1;
    static final int COMB_STATICIZER = 2;
    static final int FULL_COMB_STATICIZER = 3;
    static final int AUTO_STATICIZER = 4;
    static final int FULL_AUTO_STATICIZER = 5;

    /** Test if staticizer_type is legal **/
    static boolean isBadStaticizerType(int st) {
        return st<NO_STATICIZER || st>FULL_AUTO_STATICIZER;
    }

    /** Test if staticizer_type has weak interfering feedback **/
    static boolean isWeakFeedback(int st) {
        return st==WEAK_STATICIZER || st==FULL_STATICIZER;
    }

    /** Test if staticizer_type has combinational feedback **/
    static boolean isCombinationalFeedback(int st) {
        return st==COMB_STATICIZER || st==FULL_COMB_STATICIZER;
    }

    /** Test if staticizer_type selects automatically **/
    static boolean isAutomaticFeedback(int st) {
        return st==AUTO_STATICIZER || st==FULL_AUTO_STATICIZER;
    }

    /** Test if explict inverter should be added **/
    static boolean addSmallInverter(int st) {
        return st==FULL_STATICIZER || st==FULL_COMB_STATICIZER || st==FULL_AUTO_STATICIZER;
    }
}
