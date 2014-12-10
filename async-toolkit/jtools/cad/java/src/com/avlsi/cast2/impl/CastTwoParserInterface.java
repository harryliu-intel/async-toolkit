/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.impl;

import com.avlsi.cast.impl.CastParserInterface;

/**
 * An interface which all subparsers implement.
 **/
public interface CastTwoParserInterface extends CastParserInterface {
    void setVerbosity(boolean verbose);
}
