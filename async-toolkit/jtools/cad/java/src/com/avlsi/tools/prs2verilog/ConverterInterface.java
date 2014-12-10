/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog;

import java.util.Map;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.util.cmdlineargs.CommandLineArgs;

/**
 * An interface to different style converters.
 **/
public interface ConverterInterface {
    /**
     * Returns a <code>VerilogObject</code> representation.
     *
     * @param theArgs command line arguments that the program was invoked with
     * @param moduleName name of the module, or <code>null</code> if the CAST
     * name should be used.
     * @param toplevel whether the cell being converted is the top level cell
     * of the hierarchy
     * @param topEnv <code>true</code> if conversion is in the context of the
     * top level cell that hooks up the cell and the environment, and
     * <code>false</code> otherwise.
     *
     * @return the <code>VerilogObject</code> representation
     **/
    VerilogObject convert(final CommandLineArgs theArgs,
                          final VerilogObject moduleName,
                          final boolean toplevel,
                          final boolean topEnv);

    /**
     * Returns file dependencies.  Should only be called after
     * <code>convert</code> to get sensical result.
     **/
    Map /*<CellInterface,String[]>*/ getDependencies();
}
