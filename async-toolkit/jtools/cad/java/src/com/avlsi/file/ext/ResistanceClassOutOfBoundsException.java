/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.file.ext;

/**
 * This is a description of the class
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class ResistanceClassOutOfBoundsException
    extends IndexOutOfBoundsException
{
    public ResistanceClassOutOfBoundsException() {
        super();
    }

    public ResistanceClassOutOfBoundsException(final int resistanceClass,
                                           final int numResistanceClasses)
    {
        this(resistanceClass, numResistanceClasses, null);
    }

    public ResistanceClassOutOfBoundsException(final int resistanceClass,
                                           final int numResistanceClasses,
                                           final Throwable cause)
    {
        super("resistance class out of range: " + resistanceClass
                + "; number of valid classes: " + numResistanceClasses);
        initCause(cause);
    }
}
