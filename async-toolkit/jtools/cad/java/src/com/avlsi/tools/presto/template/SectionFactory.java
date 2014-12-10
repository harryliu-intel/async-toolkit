/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.template;

import java.math.BigDecimal;
import com.avlsi.tools.presto.output.Direction;
import com.avlsi.tools.presto.output.Section;
import com.avlsi.tools.presto.output.SectionImpl;

public class SectionFactory {
    private static final BigDecimal INPUT = new BigDecimal("1");
    private static final BigDecimal GO = new BigDecimal("2");
    private static final BigDecimal LOGIC = new BigDecimal("3");
    private static final BigDecimal OUTPUT_INVERTERS = new BigDecimal("4");
    private static final BigDecimal OUTPUT_TREE = new BigDecimal("5");
    private static final BigDecimal CONDACK = new BigDecimal("6");
    private static final BigDecimal UNCONDACK = new BigDecimal("7");
    private static final BigDecimal POSTACK = new BigDecimal("8");
    private static final BigDecimal ENABLE = new BigDecimal("9");

    private static final int INT_DIGITS = 10;

    private static BigDecimal field(int value, int pos) {
        return new BigDecimal(Integer.toString(value))
                       .movePointLeft(pos * INT_DIGITS);
    }

    public static Section mkAncestorSection() {
        /* We want to return a section which is less than any other section.
         * (This is required by PrsPrinter.emit())  Of course, you can
         * always come up with a smaller BigDecimal, but negative
         * one googol is (much) more than sufficient for our needs. */
        BigDecimal id = new BigDecimal("-1").movePointRight(100);
        return new SectionImpl(id, null, null, null, null, null); 
    }

    public static Section mkInputSection(int tier) {
        BigDecimal id = INPUT.add(field(tier, 1));
        return new SectionImpl(id, null, INPUT, "input", id, "tier " + tier);
    }

    public static Section mkGoSection(int channel) {
        BigDecimal id = GO.add(field(channel, 1));
        return new SectionImpl(id, id, GO, "go", null, null);
    }

    public static Section mkLogicSection(int channel, int rail,
                                         Direction dir) {
        BigDecimal ch = LOGIC.add(field(channel, 1));
        BigDecimal id = ch.add(field((dir == Direction.DOWN ? 0 : 1), 2))
                          .add(field(rail, 3));
        return new SectionImpl(id, ch, LOGIC, "logic", null, null);
    }

    public static Section mkOutputInvertersSection(int channel, int rail) {
        BigDecimal id = OUTPUT_INVERTERS.add(field(channel, 1))
                                        .add(field(rail, 2));
        return new SectionImpl(id, null, OUTPUT_INVERTERS,
                               "output", null, null);
    }

    public static Section mkOutputTreeSection(int tier) {
        BigDecimal id = OUTPUT_TREE.add(field(tier, 1));
        return new SectionImpl(id, OUTPUT_TREE, OUTPUT_INVERTERS,
                               "output", id, "tier " + tier);
    }

    public static Section mkCondAckSection(int channel, int line) {
        BigDecimal ch = CONDACK.add(field(channel, 1));
        BigDecimal id = ch.add(field(line, 2));
        return new SectionImpl(id, ch, CONDACK, "ack", null, null);
    }

    public static Section mkAckSection(int tier) {
        BigDecimal id = UNCONDACK.add(field(tier, 1));
        return new SectionImpl(id, UNCONDACK, CONDACK, "ack", id,
                               "tier " + tier);
    }

    public static Section mkPostAckSection() {
        return new SectionImpl(POSTACK, null, CONDACK, "ack", null, null);
    }

    public static Section mkEnableSection(int tier) {
        BigDecimal id = ENABLE.add(field(tier, 1));
        return new SectionImpl(id, null, ENABLE, "enable", id, "tier " + tier);
    }
}
