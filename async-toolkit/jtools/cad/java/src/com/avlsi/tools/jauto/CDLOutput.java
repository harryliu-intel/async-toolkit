/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import java.io.IOException;
import java.io.Writer;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import com.avlsi.cast.impl.InvalidOperationException;
import com.avlsi.cast.impl.FloatValue;
import com.avlsi.cast.impl.Symbol;
import com.avlsi.cell.CellInterface;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.CellType;
import com.avlsi.fast.NetlistAdapter;
import com.avlsi.fast.NetlistBlock;
import com.avlsi.file.common.HierName;
import com.avlsi.file.cdl.parser.CDLFactoryEmitter;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.Visitor;
import com.avlsi.netlist.impl.CDLEmitter;
import com.avlsi.netlist.impl.DeviceProcessor;
import com.avlsi.netlist.impl.FoldTransistor;
import com.avlsi.netlist.impl.NetlistFormatter;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.util.debug.Debug;

/**
 * Class to output CDL.
 *
 * @author Harry Liu
 * @version $Revision$ $Date$
 **/
public final class CDLOutput {

    /**
     * This class cannot be instantiated.
     **/
    private CDLOutput() {
        throw new AssertionError();
    }

    /**
     * Convert a Map of Symbol to FloatValue to a Map of String to Token.
     **/
    private static Map getParameters(final Map params) {
        final TreeMap m = new TreeMap();
        for (Iterator i = params.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final Symbol s = (Symbol) entry.getKey();
            final FloatValue v = (FloatValue) entry.getValue();
            double vs = 0;
            try {
                vs = v.getValue();
            } catch (InvalidOperationException e) {
                Debug.assertTrue(false, "Should not happen!");
            }
            m.put(s.getString(), new CDLLexer.SimpleToken(new Double(vs)));
        }
        return m;
    }

    /**
     * Generate an appropriate subcircuit call for a given cell.
     **/
    static void templateHeader(final CellInterface ci,
                               final Map params,
                               final CDLFactoryInterface factory,
                               final Cadencize cadencizer) {
        Set port = NetlistAdapter.getParameterList(ci, cadencizer);
        String ports[] = new String[port.size()];
        int j = 0;
        for (Iterator i = port.iterator(); i.hasNext(); j++) {
            final HierName h = (HierName) i.next();
            ports[j] = h.getCadenceString();
        }
        final Map parameters = getParameters(params);
        factory.beginSubcircuit(ci.getFullyQualifiedType(), new String[0],
                                ports, parameters, null);
    }

    /**
     * Write out a CDL template to a writer.
     **/
    public static void writeCDL(final CellInterface ci,
                                final Cadencize cadencizer,
                                final Writer w) {
        writeCDL(ci, cadencizer, new CDLFactoryEmitter(w));
    }

    /**
     * Write out a CDL template to a CDLFactoryInterface.
     **/
    public static void writeCDL(final CellInterface ci,
                                final Cadencize cadencizer,
                                final CDLFactoryInterface factory) {
        final NetlistBlock block = (NetlistBlock) ci.getBlockInterface().iterator(BlockInterface.NETLIST).next();
        templateHeader(ci, block.getParams(), factory, cadencizer);
        if (!block.isEmpty()) block.getCDLTemplate().execute(factory);
        factory.endSubcircuit(ci.getFullyQualifiedType(), null);
    }

    static CDLEmitter getCDLEmitter(final Writer w, String lengthUnit) {
        if (lengthUnit.length() > 1) lengthUnit = lengthUnit.substring(0, 1);
        return new CDLEmitter(w, lengthUnit, 6);
    }

    /**
     * Write out a NetGraph.
     **/
    public static void writeCDL(final CellType c,
                                final DeviceProcessor proc,
                                String lengthUnit,
                                double cutoff,
                                double cutoffmin,
                                boolean outputRC,
                                final Map stackMap,
                                final HierName GND,
                                final HierName Vdd,
                                final float top,
                                final float minWidth,
                                final Writer w) {
        final CDLEmitter emitter = getCDLEmitter(w, lengthUnit);
        writeCDL(c, proc, cutoff, cutoffmin, outputRC, stackMap, GND, Vdd, top,
                 minWidth, emitter, emitter);
    }

    public static void writeCDL(final CellType c,
                                final DeviceProcessor proc,
                                double cutoff,
                                double cutoffmin,
                                boolean outputRC,
                                final Map stackMap,
                                final HierName GND,
                                final HierName Vdd,
                                final float top,
                                final float minWidth,
                                final NetlistFormatter emitter,
                                final Visitor visitor) {
        final AbstractNetlist netlist = new FoldTransistor(new NetlistAdapter(c, outputRC, stackMap, GND, Vdd, top, minWidth), cutoff, cutoffmin);
        proc.setNetlist(netlist);
        emitter.format(proc, visitor, false);
    }
}
