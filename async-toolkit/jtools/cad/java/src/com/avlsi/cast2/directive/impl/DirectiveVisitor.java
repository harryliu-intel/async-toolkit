// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.directive.impl;

import com.avlsi.cast.impl.Value;
import com.avlsi.cast2.directive.impl.DirectiveStatement;
import com.avlsi.cast2.directive.impl.TokenInfo;

public interface DirectiveVisitor {
    void conditional(String guard, DirectiveStatement[] body, TokenInfo info);

    void loop(String var, String from, String to, DirectiveStatement[] body,
              TokenInfo info);

    void definition(String key, String val, TokenInfo info);

    void definition(String key, String parameter, String val, TokenInfo info);
}
