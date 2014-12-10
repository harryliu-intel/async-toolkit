/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

import java.math.BigDecimal;

public class SectionImpl implements Section {
    private final BigDecimal sectID;
    private final BigDecimal blankLineBeforeID;
    private final BigDecimal commentBeforeID;
    private final String commentBefore;
    private final BigDecimal commentWithID;
    private final String commentWith;

    public SectionImpl(BigDecimal sectID, BigDecimal blankLineBeforeID,
                       BigDecimal commentBeforeID, String commentBefore,
                       BigDecimal commentWithID, String commentWith) {
        this.sectID = sectID;
        this.blankLineBeforeID = blankLineBeforeID;
        this.commentBeforeID = commentBeforeID;
        this.commentBefore = commentBefore;
        this.commentWithID = commentWithID;
        this.commentWith = commentWith;
    }

    public int compareTo(Object o) {
        SectionImpl s = (SectionImpl) o;
        return sectID.compareTo(s.sectID);
    }

    public boolean blankLineBefore(Section prevSection_) {
        SectionImpl prevSection = (SectionImpl) prevSection_;
        return (blankLineBeforeID != null &&
                prevSection.sectID.compareTo(blankLineBeforeID) < 0);
    }

    public String commentBefore(Section prevSection_) {
        SectionImpl prevSection = (SectionImpl) prevSection_;
        if (commentBeforeID != null &&
            prevSection.sectID.compareTo(commentBeforeID) < 0)
            return commentBefore;
        return null;
    }

    public String commentWith(Section prevSection_) {
        SectionImpl prevSection = (SectionImpl) prevSection_;
        if (commentWithID != null &&
            prevSection.sectID.compareTo(commentWithID) < 0)
            return commentWith;
        return null;
    }
}
