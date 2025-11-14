// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.util;

import java.io.IOException;

import com.avlsi.fast.DirectiveBlock;
import com.avlsi.fast.BlockInterface;

public interface DirectiveActionInterface {
    
  
    void doUnParameterizedDirective(BlockInterface block,
                                         DirectiveBlock db,
                                         String directive,
                                         Object value,
                                         String valueType ) throws IOException ;
    void doParameterizedDirectiveValue(BlockInterface block,
                                       DirectiveBlock db,
                                       String directive,
                                       Object parameter,
                                       Object value,
                                       String parameterType,
                                       String valueType) throws IOException ;
    void doParameterizedDirectiveType(BlockInterface block,
                                      DirectiveBlock db,
                                      String directive,
                                      String parameterType,
                                      String valueType) throws IOException;
    void doBlockInterface(BlockInterface block) throws IOException ;
}
