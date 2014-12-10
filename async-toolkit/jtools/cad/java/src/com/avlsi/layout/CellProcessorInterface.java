/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.layout;

import java.io.IOException;
import java.io.Writer;
import com.avlsi.cast2.directive.UnknownDirectiveException;

/**
 * Command line program to process some CellInterfaces
 **/

public interface CellProcessorInterface {
    void process( final Writer writer ) throws IOException,
                                               UnknownDirectiveException,
                                               CellProcessorException;
}
