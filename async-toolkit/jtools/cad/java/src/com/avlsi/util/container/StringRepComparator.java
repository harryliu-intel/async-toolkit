// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.util.container;

import java.util.Comparator;

public class StringRepComparator implements Comparator {
    public int compare(Object a, Object b) {
        return a.toString().compareTo(b.toString());
    }
}
