// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.csp.csp2java.runtime;

import com.avlsi.csp.csp2java.runtime.Fold.BinaryFunction; 

public interface CspValue {
    void setValue(CspValue value);
    void setValue(CspValue value, BinaryFunction modifier);
}
