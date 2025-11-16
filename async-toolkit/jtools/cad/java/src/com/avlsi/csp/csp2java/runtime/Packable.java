// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.csp.csp2java.runtime;

public interface Packable {
    int pack(CspInteger packed, int start);
    int unpack(CspInteger packed, int start);
}
