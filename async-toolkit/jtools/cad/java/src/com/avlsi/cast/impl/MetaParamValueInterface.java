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

package com.avlsi.cast.impl;

import com.avlsi.fast.metaparameters.MetaParamTypeInterface;

/**
 * Tagging interface for values that are meta params.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public interface MetaParamValueInterface {
    MetaParamTypeInterface toMetaParam();
}
