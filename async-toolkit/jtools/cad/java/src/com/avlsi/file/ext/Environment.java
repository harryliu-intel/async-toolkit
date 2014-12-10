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

import java.util.Date;

/**
 * Immutable wrapper class encapsulating environment information from the
 * .ext file such as:
 * <ul>
 *     <li> The name of the technology
 *     <li> The time the cell was last modified
 *     <li> The version of the ext format
 *     <li> The style that the cell has been extracted with
 *     <li> The resistance per square for each of the resistance classes
 *              defined in the tech file.
 * </ul>
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class Environment {
    private final String technology;
    private final Date timestamp;
    private final String version;
    private final String style;
    private double[] resistanceClasses;

    public Environment(final String technology,
                       final Date timestamp,
                       final String version,
                       final String style,
                       final double[] resistanceClasses)
    {
        this.technology = technology;
        this.timestamp = timestamp;
        this.version = version;
        this.style = style;
        this.resistanceClasses = resistanceClasses;
    }

    public String getTechnology() {
        return technology;
    }

    public Date getTimestamp() {
        return timestamp;
    }

    public String getVersion() {
        return version;
    }

    public String getStyle() {
        return style;
    }

    /**
     * Returns the reistance per square (in milliohms) for the specified
     * resistance class.
     * @param n  the number of the resistance class
     * @return resistance per square (in milliohms) for resistance class n
     * 
     * @throws ResistanceClassOutOfBoundsException
     **/
    public double getResistanceForClass(int n) {
        try {
            return resistanceClasses[n];
        } catch (ArrayIndexOutOfBoundsException e) {
            throw new ResistanceClassOutOfBoundsException(n,
                    getNumResistClasses(), e);
        }
    }

    public int getNumResistClasses() {
        return resistanceClasses.length;
    }

    public String toString() {
        final StringBuffer sb = new StringBuffer();

        sb.append("Environment {");
        sb.append("\n Technology: " + technology);
        sb.append("\n, Timestamp: " + timestamp);
        sb.append("\n, Version: " + version);
        sb.append("\n, Style: " + version);
        sb.append("\n, Resistance Classes:");
        for (int i = 0; i < resistanceClasses.length; ++i)
            sb.append(" " + resistanceClasses[i]);
        sb.append("\n}");

        return sb.toString();
    }
}
