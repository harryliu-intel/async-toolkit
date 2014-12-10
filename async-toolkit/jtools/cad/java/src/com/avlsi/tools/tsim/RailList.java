/*
 * Copyright 2000, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import java.math.BigInteger;
import com.avlsi.util.debug.Debug;

/**
 * @todo Undocumented.
 *
 * @author Dan Daly
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 23 July 2002.
 **/

public class RailList {
    
    private int[] r;
    private BigInteger[] BigN;
    private BigInteger[] BigNpowM;
    private boolean NpowMset = false;
    
    private RailList() { }
   
    public void build(int length) {
        r = new int[length];
        BigN = new BigInteger[length];
        BigNpowM = new BigInteger[length];
    }
    
    public void set(int index, int N) {
        this.r[index] = N;
        this.BigN[index] = BigInteger.valueOf(N);
        //this.BigNpowM[index] = BigN[index].pow(M);
    }

    public void setNpowM(int M) {
        for(int loop=0;loop<getLength();loop++) {
           this.BigNpowM[loop] = BigN[loop].pow(M);
        }
        NpowMset = true;
    }
 
    public RailList(int r1) {
        build(1);
        set(0,r1);
    }
    
    public RailList(int r1,int r2) {
        build(2);
        set(0,r1);
        set(1,r2);
    }

    public /*@ pure @*/ int getLength() { return r.length; }
    
    public /*@ pure @*/ int get(int index) { 
        Debug.assertTrue(index < getLength() && index >=0);
        return r[index];
    }

    public /*@ pure @*/ BigInteger getBigN(int index) { 
        Debug.assertTrue(index < getLength()&& index >=0);
        return BigN[index];
    }

    public /*@ pure @*/ BigInteger getBigNpowM(int index) {
        Debug.assertTrue(index < getLength()&& index >=0);
        if (NpowMset)
            return BigNpowM[index];
        return BigInteger.ZERO;
    }

} // end of class RailList

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
