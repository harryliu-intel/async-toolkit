// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.csp.util;

import java.util.ResourceBundle;
import java.util.Enumeration;

public class NullResourceBundle extends ResourceBundle {
    public Enumeration getKeys() { return null; }
    protected Object handleGetObject(String key) { return null; }
}
