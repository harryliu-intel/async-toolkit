// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.fulcrummicro.util.properties;

public class BadPropertyException extends RuntimeException {

    private static final long serialVersionUID = -7578252303588634641L;

    public BadPropertyException(String msg) {
        super(msg);
    }

}

