/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cdl2skill;


public class CDL2SkillException extends Exception {
    public CDL2SkillException( final String errorStr, final Exception cause ) {
        super( errorStr, cause );
    }

    public CDL2SkillException( final String errorStr ) {
        super( errorStr );
    }
}
