/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast;

import java.util.ArrayList;
import java.util.List;

import com.avlsi.fast.BlockCommon;
import com.avlsi.tools.cosim.CSPCoSimInfo;
import com.avlsi.csp.ast.CSPProgram;

/**
 * Interface to a CSP block.
 **/
public class CspBlock extends BlockCommon {
    /**
     * The cosimulation info from the csp block
     **/
    private final CSPCoSimInfo cspCosimInfo;

    /**
     * The names (Strings) of the ports for the csp block.  If all
     * ports are to be passed in to the csp, is empty.
     **/
    private final List cspPorts;

    /**
     * CSP parse tree.
     **/
    private CSPProgram csp;

    /**
     * The compiled class for CSP without coverage probes;
     **/
    private Class cspClass;

    /**
     * The compiled class for CSP with coverage probes;
     **/
    private Class cspClassWithProbes;

    /**
     * Does any refinement parents have sequential CSP statements?
     **/
    private boolean hasSequential;

    public CspBlock() {
        this.cspCosimInfo = new CSPCoSimInfo();
        this.cspPorts = new ArrayList();
        this.csp = null;
        this.cspClass = null;
        this.cspClassWithProbes = null;
        this.hasSequential = false;
    }

    public String getType() {
        return BlockInterface.CSP;
    }

    /* CSP cosim information is derived and, due to the restrictions on
     * port-changing by the refinement process, will always be derived
     * properly.  Statement overrides, functions add.
     */
    public void refineFrom(final BlockInterface o) {
        super.refineFrom(o);

        final CspBlock parent = (CspBlock) o;
        this.hasSequential |= parent.hasSequentialStatement();

        final CSPProgram parentCSP = parent.getCSPProgram();
        if (parentCSP != null) {
            if (getCSPProgram() == null) {
                setCSPProgram(new CSPProgram());
            }
            getCSPProgram().refineFrom(parentCSP);
        }
    }

    public BlockInterface merge(BlockInterface o) {
        throw new AssertionError(
                "CspBlock doesn't support all BlockInterface functionality");
    }

    public BlockInterface replace(BlockInterface o) {
        throw new AssertionError(
                "CspBlock doesn't support all BlockInterface functionality");
    }

    public CSPCoSimInfo getCSPCoSimInfo() {
        return cspCosimInfo;
    }

    public List getCSPPorts() {
        return cspPorts;
    }
    
    public Class getCSPClass(boolean probes) {
        return probes ? cspClassWithProbes : cspClass;
    }

    public void setCSPClass(final Class cspClass, boolean probes) {
        if (probes) {
            this.cspClassWithProbes = cspClass;
        } else {
            this.cspClass = cspClass;
        }
    }

    public CSPProgram getCSPProgram() {
        return csp;
    }

    public void setCSPProgram(final CSPProgram csp) {
        this.csp = csp;
    }

    public boolean hasSequentialStatement() {
        return hasSequential || (csp != null && csp.getStatement() != null);
    }

    public String toString() {
        return "CspBlock cspCosimInfo=" + cspCosimInfo +
               " cspPorts=" + cspPorts +
               " cspClass=" + cspClass +
               " cspClassWithProbes=" + cspClassWithProbes +
               " csp=" + csp;
    }
}
