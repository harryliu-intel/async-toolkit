// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.impl;

import antlr.RecognitionException;
import antlr.TokenStreamException;

/**
 * An interface which implements a tree parser callback.
 **/
public interface CastTwoParserCallback extends CastTwoParserInterface {
    void goalCallback(CastTwoUtil.ParserCallback cb)
        throws RecognitionException, TokenStreamException;
}
