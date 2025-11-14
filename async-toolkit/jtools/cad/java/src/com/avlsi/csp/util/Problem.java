// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.csp.util;

import com.avlsi.csp.grammar.ParseRange;
import com.avlsi.io.Printable;

public interface Problem {
    ParseRange getParseRange();
    String getCode();
    String getMessage();
    void printMessage(Printable pw);
    Object[] getArguments();
}
