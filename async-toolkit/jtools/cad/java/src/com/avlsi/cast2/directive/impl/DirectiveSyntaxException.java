// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: $
 * $DateTime: $
 * $Author: $
 */

package com.avlsi.cast2.directive.impl;

import antlr.SemanticException;

public class DirectiveSyntaxException extends SemanticException {
    public DirectiveSyntaxException(String s, String fileName, int line,
                                    int column) {
        super(s, fileName, line, column);
    }
}
