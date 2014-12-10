/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.fast.metaparameters;
import java.math.BigInteger;

/**
 * Interface representing boolean metaparameter types.
 *
 * @author David Hilvert
 * @version $Revision$ $Date$
 **/
public interface BooleanMetaParam extends MetaParamTypeInterface {
    String toString();
    boolean toBoolean();
}
