// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.file.liberty.parser;

public class LibertyParserException extends RuntimeException {
    public final int errorCode;

    public LibertyParserException(final int errorCode, final String message) {
        super(message);
        this.errorCode = errorCode;
    }
}
