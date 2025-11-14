// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.fulcrummicro.util.properties;

public class Property {
    
    public final boolean concat;
    public final char concat_op;
    public final String value;
    public final String type;
    
    public Property(String type, String value) {
        this(type, value, false, ',');
    }

    public Property(String type, String value, boolean concat, char concat_op) {
        this.type = type;
        this.value = value;
        this.concat = concat;
        this.concat_op = concat_op;
    }
}
