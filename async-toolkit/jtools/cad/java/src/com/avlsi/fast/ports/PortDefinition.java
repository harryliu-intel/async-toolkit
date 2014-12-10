/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.ports;

/**
 * A port variable definition, consisting of its name, type, and direction.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public final class PortDefinition {

    /**
     * Direction specifier used for nodes or channels whose direction is 
     * unspecified.
     * **/
    public static final int NONE = -1;

    /** Direction specifier used for IN nodes or channels. **/
    public static final int IN = 0;
    public static final String INSTRING = "-";

    /** Direction specifier used for OUT nodes or channels. **/
    public static final int OUT = 1;
    public static final String OUTSTRING = "+";

    /** Direction specifier used for INOUT nodes or channels. **/
    public static final int INOUT = 2;
    public static final String INOUTSTRING = "";

    /** 
     * Synonyms for IN/OUT/INOUT which may be more intuitive when sub-channels
     * are involved.
     **/

    /** Direction specifier used for REVERSE sub-nodes or sub-channels. **/
    public static final int REVERSE = IN;

    /** Direction specifier used for FORWARD sub-nodes or sub-channels. **/
    public static final int FORWARD = OUT;

    /** Direction specifier used for BIDIRECTIONAL sub-nodes or sub-channels.**/
    public static final int BIDIRECTIONAL = INOUT;

    /** Port name, not null. **/
    private final String name;

    /** Port type, not null. **/
    private final PortTypeInterface type;

    /** Port direction, either IN, OUT, or INOUT. **/
    private final int direction;

    /**
     * Class constructor
     *
     * @param name  port name, not null
     * @param type  port type, not null
     * @param direction  port direction, IN, OUT, or INOUT
     **/
    public PortDefinition(final String name,
            final PortTypeInterface type,
            final int direction) {
        this.name = name;
        this.type = type;
        this.direction = direction;
    }

    /**
     * Returns the port variable name.
     *
     * @return port name, not null
     **/
    public String getName() {
        return name;
    }

    /**
     * Returns the port type.
     *
     * @return port type, not null
     **/
    public PortTypeInterface getType() {
        return type;
    }

    /**
     * Returns the port direciton, either IN, OUT, or INOUT.
     *
     * @return port direction
     **/
    public int getDirection() {
        return direction;
    }

    /**
     * Returns the reverse of the port direction, either IN, OUT, or INOUT.
     *
     * @return reverse of the port direction
     **/
    public int getReverseDirection() {
        switch (direction) {
          case IN:
            return OUT;
          case OUT:
            return IN;
          case INOUT:
            return INOUT;
          default:
            return NONE;
        }
    }


    private String directionString() {
        switch (direction) {
            case IN:
                return "in";
            case OUT:
                return "out";
            case INOUT:
                return "inout";
            default:
                return "<bad direction>";
        }
    }

    private String subPortDirectionString() {
        switch (direction) {
            case REVERSE:
                return "reverse";
            case FORWARD:
                return "forward";
            case BIDIRECTIONAL:
                return "bidirectional";
            default:
                return "<bad direction>";
        }
    }


    /**
     * Updates the port direction (in, out, etc) with the specified
     * direction modifier (forward, reverse, bidirectional).
     **/
    public static int updateDirection(final int originalDirection,
                                      final int directionModifier) {
        switch (directionModifier) {
            case PortDefinition.FORWARD:
                return originalDirection;

            case PortDefinition.REVERSE:
                switch (originalDirection) {
                    case PortDefinition.IN:
                        return PortDefinition.OUT;
                    case PortDefinition.OUT:
                        return PortDefinition.IN;
                    case PortDefinition.INOUT:
                    case PortDefinition.NONE:
                        return PortDefinition.INOUT;
                }
                throw new AssertionError("Unknown port direction");

            case PortDefinition.BIDIRECTIONAL:
                return PortDefinition.INOUT;
        }

        throw new AssertionError("Unknown port direction");
    }


    public String toString() {
        return name + ": " + directionString() + ' ' + type.toString();
    }

    public String toSubPortString() {
        return name + ": " + directionString() + ' ' + type.toString();
    }
}
