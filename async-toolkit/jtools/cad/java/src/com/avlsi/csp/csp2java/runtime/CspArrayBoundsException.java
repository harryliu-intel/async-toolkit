// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.csp.csp2java.runtime;

public class CspArrayBoundsException extends RuntimeException {
    final String filename;
    final int line, column;
    public CspArrayBoundsException(final CspInteger index,
                                   final int min,
                                   final int max,
                                   final String filename,
                                   final int line,
                                   final int column) {
        super("index " + index + " out of bounds " + min + ".." + max);
        this.filename = filename;
        this.line = line;
        this.column = column;
    }
}
