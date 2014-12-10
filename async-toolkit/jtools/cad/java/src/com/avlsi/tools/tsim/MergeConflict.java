/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import java.math.BigInteger;

/**
 * @todo Undocumented.
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class MergeConflict {

    // Attributes

    BigInteger value1;
    BigInteger value2;
    long time1;
    long time2;

    // Constructors

    /** 
     * @todo Undocumented. 
     *
     * @param m1v
     * @param m1t
     * @param m2v
     * @param m2t
     **/

    public MergeConflict(BigInteger m1v, long m1t,
			 BigInteger m2v, long m2t){
	
	this.value1 = m1v; 
	this.value2 = m2v;
	this.time1 = m1t; 
	this.time2 = m2t;
    }

    // Public Methods

    /**
     * Get conflict string.
     **/

    public String toString(){
	String str = value1.toString()+"@"+time1
	    +", " +value2.toString()+"@"+time2;
	return str;
    }

} // end of class MergeConflict

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
