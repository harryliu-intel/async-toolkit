/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim;

import java.util.Hashtable;
import java.util.Optional;
import java.util.function.Predicate;

import com.avlsi.cell.CellInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.tools.tsim.Arbiter;
import com.avlsi.util.text.StringUtil;

public class CoSimParameters {
    private Hashtable table;

    public static final int UNSPEC = 1;
    public static final int DIGITAL = 2;
    public static final int JAVA = 4;
    public static final int CSP = 8;
    public static final int VERILOG = 16;
    public static final int NULL = 32;
    public static final int BEH_ALL = 64;
    public CoSimParameters() { table = new Hashtable(); }

    /**
     * Find an existing parameter in the table with the given name.
     *
     * @param name name of the parameter
     * @return a parameter corresponding to the name or <code>null</code> if a
     * prameter by that name does not exist
     **/
    private Parameter getParam(String name) {
        if (name.charAt(0) != '.') name = "." + name;

        return (Parameter) table.get(name);
    }

    /**
     * Find an existing parameter in the table with the given name or if it
     * does not exist, create a parameter with the given behavior and
     * abitration mode and added it to the table.
     *
     * @param name name of the parameter
     * @param behavior behavior for a newly created parameter
     * @param arbitrationMode arbitration mode for a newly created parameter
     * @return a parameter corresponding to the name
     **/
    private Parameter getParam(String name, int behavior, int arbitrationMode) {
        if (name.charAt(0) != '.') name = "." + name;

        Parameter p = (Parameter) table.get(name);
        if (p == null) {
            p = new Parameter(behavior, arbitrationMode);
            table.put(name, p);
        }
        return p;
    }

    private static final String COSIM_SUFFIX = "_cosim";

    public static String stripCoSimDigitalSuffix(final HierName name) {
        return StringUtil.replaceSubstring(name.getAsString('.'), "."+COSIM_SUFFIX, "");
    }

    public static HierName addCoSimDigitalSuffix(final HierName prefix) {
         HierName rv = null;
         try {
           rv = HierName.makeHierName(prefix, COSIM_SUFFIX);
         } catch (InvalidHierNameException e) {
           System.out.println("Ieee!  InvalidHierNameException!");
           System.exit(1);
         }
         return rv;
    }

    /**
     * Set bits of the parameter bitfield (to true).  This will not set the
     * UNSPEC bit.
     *
     * @param name Name of the instance to parameterize
     * @param value Bits set to true
     **/
    private Parameter setSimpleBehavior(final String name, int value) {
        // do not modify the UNSPEC bit
        value = value & (~UNSPEC);

        final Parameter p = getParam(name, value, 0);
        value = value | p.getBehavior();

        if ((value & CSP) != 0 && (value & JAVA) != 0) {
            System.out.println("Error: cannot cosimulate CSP and JAVA."
                             + "  Using CSP only.");
            value = (value & ~JAVA);
        }

        p.setBehavior(value);

        return p;
    }

    public void setBehavior(final String name, final int value) {
        assert value != VERILOG;
        setSimpleBehavior(name, value);
    }

    public void setBehavior(final String name, final int value,
                            final String level) {
        assert level == null || value == VERILOG;
        final Parameter p = setSimpleBehavior(name, value);
        if (level != null) p.setVerilogLevel(level);
    }

    public void setBehavior(final String name, final int value, 
                            final boolean narrow,
                            final Predicate<CellInterface> descend) {
        assert value == DIGITAL;
        final Parameter p = setSimpleBehavior(name, value);
        p.setNarrowSubcell(narrow);
        p.setDescendPredicate(descend);
    }

    /**
     * Clear bits of the parameter bitfield (to false).  This will not clear
     * the UNSPEC bit if no bits have been set yet.
     *
     * @param name Name of the instance to parameterize
     * @param value Bits set to false
     **/ 
    public void clearBehavior(final String name, final int value) {
        final Parameter p = getParam(name);
        if (p != null)
            p.setBehavior(~value & p.getBehavior());
    }

    public int lookupBehavior(final String name) {
        final Parameter p = getParam(name);
        return p == null ? UNSPEC : p.getBehavior();
     }

    public void setArbitrationMode(final String name,
                                   final int arbitrationMode) {
        getParam(name, UNSPEC, -1).setArbitrationMode(arbitrationMode);
    }

    public int lookupArbitrationMode(final String name) {
        final Parameter p = getParam(name);
        return p == null ? Arbiter.NON_LINKED : p.getArbitrationMode();
    }

    public String lookupVerilogLevel(final String name) {
        final Parameter p = getParam(name);
        return p == null ? null : p.getVerilogLevel();
    }

    public boolean isNarrowSubcell(final String name) {
        final Parameter p = getParam(name);
        return p == null ? false : p.isNarrowSubcell();
    }

    public Optional<Predicate<CellInterface>> getDescendPredicate(final String name) {
        final Parameter p = getParam(name);
        return p == null ? Optional.empty() : p.getDescendPredicate();
    }

    private static final class Parameter {
        private int behavior;
        private int arbitrationMode;
        private String verilogLevel;
        private boolean narrowSubcell;
        private Optional<Predicate<CellInterface>> descendPredicate;

        Parameter(final int behavior, final int arbitrationMode) {
            this.behavior = behavior;
            this.arbitrationMode = arbitrationMode;
            this.verilogLevel = null;
            this.narrowSubcell = false;
            this.descendPredicate = Optional.empty();
        }

        void setBehavior(final int behavior) {
            this.behavior = behavior;
        }

        int getBehavior() {
            return behavior;
        }

        void setArbitrationMode(final int arbitrationMode) {
            this.arbitrationMode = arbitrationMode;
        }

        int getArbitrationMode() {
            return arbitrationMode;
        }

        void setVerilogLevel(final String verilogLevel) {
            assert this.verilogLevel == null;
            this.verilogLevel = verilogLevel;
        }

        String getVerilogLevel() {
            return verilogLevel;
        }

        void setNarrowSubcell(final boolean narrowSubcell) {
            this.narrowSubcell = narrowSubcell;
        }

        boolean isNarrowSubcell() {
            return narrowSubcell;
        }

        void setDescendPredicate(final Predicate<CellInterface> p) {
            descendPredicate = Optional.of(p);
        }

        Optional<Predicate<CellInterface>> getDescendPredicate() {
            return descendPredicate;
        }
    }
}
