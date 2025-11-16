// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

public class LineAndSection {
    public final PrsLine line;
    public final Section section;

    public LineAndSection(PrsLine line, Section section) {
        this.line = line;
        this.section = section;
    }
}
