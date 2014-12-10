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
