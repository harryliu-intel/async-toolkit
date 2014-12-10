/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.dsim;

/**
 * Thrown to indicate that a cell type could not be found.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class NoSuchCellException extends Exception {

    private final String cellType;

    /**
     * Constructs a <code>NoSuchCellException</code> indicating
     * that the specified cell could not be found.
     **/
    public NoSuchCellException(final String cellType) {
        this.cellType = cellType;
    }

    /**
     * Returns the type of cell that couldn't be found.
     **/
    public String getCellType() {
        return cellType;
    }
}
