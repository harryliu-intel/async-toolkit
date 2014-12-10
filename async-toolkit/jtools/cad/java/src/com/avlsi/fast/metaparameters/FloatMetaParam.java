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

/**
 * Interface representing floating point metaparameter types.
 *
 * @author David Hilvert
 * @version $Revision$ $Date$
 **/
public interface FloatMetaParam extends MetaParamTypeInterface {
    String toString();
    float toFloat();
}
