/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim.spec;

/**
 * Class to represent a cosimulation.
 * <p>
 * Represents the top level rule <code>cosimulate</code> rule in the
 * <a href="http://internal.avlsi.com/tree/sw/cad/doc/specs/cast/cosim.html">Cosimulation UI Specification</a>.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class CoSim {
    /**
     * Type of cell to be simulated.  May not be null.
     **/
    private final String cellType;

    /**
     * Specification of the cell's simulation methods.  May not be null.
     **/
    private final CoSimSpecList coSimSpecList;

    /**
     * Named environment in which the cell should be simulated.  
     * May not be null.
     **/
    private final String envName;

    /**
     * Specification of the environment's simulation method.  May not be null.
     **/
    private final CoSimSpec envSpec;

    /**
     * Class constructor.
     * 
     * @param cellType  Type of cell to be simulated.  May not be null.
     * @param coSimSpecList  Specification of the cell's simulation methods.
     *   May not be null.
     * @param envName  Named environment in which the cell should be
     *   simulated.  May not be null.
     * @param envSpec  pecification of the environment's simulation method.
     *   May not be null.
     **/
    public CoSim(
            final String cellType,
            final CoSimSpecList coSimSpecList,
            final String envName,
            final CoSimSpec envSpec) {
        this.cellType = cellType;
        this.coSimSpecList = coSimSpecList;
        this.envName = envName;
        this.envSpec = envSpec;
    }

    /**
     * Returns the cell type to be simulated.
     * @return cell type to be simulated, not null.
     **/
    public String getCellType() {
        return cellType;
    }

    /**
     * Returns a specification of the cell's simulation methods.
     * @return specification of the cell's simulation methods, not null
     **/
    public CoSimSpecList getCoSimSpecList() {
        return coSimSpecList;
    }

    /**
     * Returns the named environment in which the cell should be simulated.
     * @return named environment in which the cell should be simulated,
     *   not null.
     **/
    public String getEnvName() {
        return envName;
    }

    /**
     * Returns a specification of the environment's simulation method.
     * @return specification of the environment's simulation methods not null
     **/
    public CoSimSpec getEnvSpec() {
        return envSpec;
    }

    public String toString() {
        return cellType + coSimSpecList +
            ':' + envName + '{' + envSpec + '}';
    }

    /**
     * Returns a cosimulation object parsed from the given spec.
     * @param spec A cosimulation specification
     * @return cosimulation object corresponding to <var>spec</var>
     * @throws IllegalArgumentException if <var>spec</var> cannot be parsed
     **/
    public static CoSim getCoSim(final String spec)
        throws IllegalArgumentException {
        return getCoSim(spec, false);
    }

    public static CoSim getCoSim(final String spec, final boolean envOptional)
        throws IllegalArgumentException {
        return getCoSim(spec, envOptional, null, null);
    }

    public static CoSim getCoSim(final String spec, final boolean envOptional,
                                 final CoSimSpecList topCoSimSpecList,
                                 final CoSimSpec envCoSimSpec)
        throws IllegalArgumentException {
        final CoSimLexer lexer = new CoSimLexer(new java.io.StringReader(spec));
        final CoSimParser parser = new CoSimParser(lexer);

        if (topCoSimSpecList != null)
            parser.setTopCoSimSpecList(topCoSimSpecList);
        if (envCoSimSpec != null) parser.setEnvCoSimSpec(envCoSimSpec);

        try {
            if (envOptional) return parser.cosimEnvOptional();
            else return parser.cosim();
        } catch (antlr.ANTLRException e) {
            throw (IllegalArgumentException)
                new IllegalArgumentException("Cannot parse cosimulation specification: " + spec).initCause(e);
        }
    }
}
