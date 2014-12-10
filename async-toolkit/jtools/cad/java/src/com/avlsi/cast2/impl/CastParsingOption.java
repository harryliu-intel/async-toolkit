/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.impl;

import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.Symbol;
import com.avlsi.cast.impl.Value;
import com.avlsi.cell.CellInterface;

/**
 * A collection of methods called by the CAST parser to get information on how
 * parsing should proceed.
 **/
public interface CastParsingOption {
    /**
     * Should requests to inline a cell contain subcells be processed?
     *
     * @return <code>true</code> if inlining should be respected, and
     * <code>false</code> otherwise.
     **/
    boolean processInline(CellInterface cell);

    /**
     * Get the value that has been override at runtime for variable
     * <code>var</code> in the cell <code>cell</code>.
     *
     * @return the overriden value, or <code>null</code> to indicate the
     * variable has not been overriden.
     **/
    Value getOverrideValue(CellInterface cell, Symbol var, Value init,
                           Environment env);

    /**
     * Signal the start of CellImpl creation.  At this point, only the name of
     * the <code>cell</code> is defined.
     **/
    void beginCellConstruction(CellInterface cell);

    /**
     * Signal the end of of CellImpl creation.  At this point, the
     * <code>cell</code> should be complete.
     **/
    void endCellConstruction(CellInterface cell);

    /**
     * Tells the parser if it should be compatible with CAST source before the
     * fix for bug 3771.  So far, the only cell that is found to be
     * incompatible is chip.x6.core.CORE, but no one is willing to modernize
     * the CAST.
     **/
    boolean bug3771HackEnabled();

    /**
     * Tells the parser if it should be compatible with CAST source before the
     * fix for bug 7068.
     **/
    boolean bug7068HackEnabled();

    /**
     * Tells the parser if it should be compatible with CAST source before the
     * fix for bug 16459.
     **/
    boolean bug16459HackEnabled();

    /**
     * The default choice.
     **/
    class DefaultParsingOption implements CastParsingOption {
        public boolean processInline(final CellInterface cell) { return true; }
        public Value getOverrideValue(CellInterface cell, Symbol val,
                                      Value init, Environment env) {
            return null;
        }
        public void beginCellConstruction(final CellInterface cell) { }
        public void endCellConstruction(final CellInterface cell) { }
        public boolean bug3771HackEnabled() { return false; }
        public boolean bug7068HackEnabled() { return false; }
        public boolean bug16459HackEnabled() { return false; }
    }

    CastParsingOption DEFAULT = new DefaultParsingOption();

    /**
     * Simulators should use this option.  Currently, it turns inlining off, so
     * that CSP definitions inside inlined subcells can be found properly.
     **/
    CastParsingOption SIMULATOR = new DefaultParsingOption() {
        public boolean processInline(final CellInterface cell) { return false; }
    };
}
