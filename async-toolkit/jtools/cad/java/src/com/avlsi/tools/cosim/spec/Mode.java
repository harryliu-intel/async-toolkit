/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim.spec;

import java.util.Optional;
import java.util.function.Predicate;

import com.avlsi.cell.CellInterface;
import com.avlsi.util.container.ObjectUtils;

/**
 * Represents the mode in which a cell should be simulated.
 * <p>
 * Represents the rule <code>mode</code>
 * in the
 * <a href="http://internal.avlsi.com/tree/sw/cad/doc/specs/cast/cosim.html">Cosimulation UI Specification</a>.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class Mode implements AcceptorInterface {
    protected Mode() { }

    public void accept(VisitorInterface v) {
        v.visitMode(this);
    }

    /**
     * Java mode.
     **/
    public static final Mode JAVA = new JavaMode();

    /**
     * Csp mode.
     **/
    public static final Mode CSP = new CspMode();

    /**
     * Subcells mode.
     **/
    public static final Mode SUBCELLS = new SubcellsMode();

    /**
     * PRS mode.
     **/
    public static final Mode PRS = new PrsMode();

    /**
     * Spice mode.
     **/
    public static final Mode SPICE = new SpiceMode();

    private static final class JavaMode extends Mode {
        public String toString() { return "java"; }
    }

    private static final class CspMode extends Mode {
        public String toString() { return "csp"; }
    }

    public static final class SubcellsMode extends Mode {
        private final boolean narrow;
        private final Predicate<CellInterface> descend;
        SubcellsMode() {
            this(false);
        }
        SubcellsMode(final boolean narrow) {
            this(narrow, x -> true);
        }
        public SubcellsMode(final boolean narrow,
                            final Predicate<CellInterface> descend) {
            this.narrow = narrow;
            this.descend = descend;
        }
        public String toString() { return "subcells"; }
        public SubcellsMode makeNarrow() {
            return isNarrow() ? this
                              : new SubcellsMode(true, getDescendPredicate());
        }
        public boolean isNarrow() {
            return narrow;
        }
        public boolean descendInto(final CellInterface x) {
            return descend.test(x);
        }
        public Predicate<CellInterface> getDescendPredicate() {
            return descend;
        }
    }

    private static final class PrsMode extends Mode {
        public String toString() { return "prs"; }
    }

    private static final class SpiceMode extends Mode {
        public String toString() { return "spice"; }
    }

    public static final class VerilogMode extends Mode {
        private final String level;

        public VerilogMode(final String level) {
            this.level = level;
        }

        public String getLevel() {
            return level;
        }

        public boolean equals(final Object o) {
            return o instanceof VerilogMode &&
                   ObjectUtils.equals(((VerilogMode) o).level, level);
        }

        public int hashCode() {
            return level == null ? 0 : level.hashCode();
        }

        public String toString() {
            return "verilog" + (level != null ? '.' + level : "");
        }
    }
}
