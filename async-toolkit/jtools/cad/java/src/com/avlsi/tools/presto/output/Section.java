// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

public interface Section extends Comparable {
    /**
     * Returns true if a blank line should appear before this section,
     * when it is preceded by prevSection.
     */
    boolean blankLineBefore(Section prevSection);

    /**
     * Returns a comment which should appear before this section,
     * when it is preceded by prevSection.  null means no comment
     */
    String commentBefore(Section prevSection);

    /**
     * Returns a comment which should appear on the first line of this section,
     * when it is preceded by prevSection.  null means no comment
     */
    String commentWith(Section prevSection);
}
