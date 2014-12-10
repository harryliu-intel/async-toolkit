/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.csp.csp2java.runtime;

import java.math.BigInteger;
import com.avlsi.fast.metaparameters.ArrayMetaParam;
import com.avlsi.fast.metaparameters.BooleanMetaParam;
import com.avlsi.fast.metaparameters.IntegerMetaParam;
import com.avlsi.fast.metaparameters.MetaParamTypeInterface;

/* A class for runtime array support. */

public class CspMetaParamArray1{
    ArrayMetaParam a;

    public CspMetaParamArray1( ArrayMetaParam a ){
        this.a = a;
    }

    public CspInteger get( final CspInteger index ){
        MetaParamTypeInterface m = a.get( index.intValue() );
        
        if( m instanceof IntegerMetaParam ){
            return new CspInteger(((IntegerMetaParam) m).toBigInteger());
        }else if( m instanceof BooleanMetaParam ){
            boolean b = ((BooleanMetaParam) m).toBoolean();
            return (b ? new CspInteger(BigInteger.ONE)
                      : new CspInteger(BigInteger.ZERO));
        }else{
            throw new /* Assertion */ Error( "Invalid Meta-parameter type." );
        }
    }
}

