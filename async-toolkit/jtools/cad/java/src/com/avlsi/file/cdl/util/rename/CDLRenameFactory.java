/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.util.rename;


import java.util.Map;

import com.avlsi.cast.impl.Environment;

import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;

import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;

import com.avlsi.file.cdl.util.rename.CDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.CDLRenameException;
import com.avlsi.file.cdl.util.rename.CDLNameInterface;

/**
 * An CDLFactoryInterface that can be used to rename nodes and names of circuit
 * components to another format
 **/
public final class CDLRenameFactory implements CDLFactoryInterface {
    private final CDLFactoryInterface out;
    private CDLNameInterface ni;
    private final CDLNameInterfaceFactory nif;
    private String subckt;

    public static class RenameException extends RuntimeException {
        RenameException(final String message, final String cell,
                        final CDLRenameException e) {
            super(message + (cell == null ? "" : " in cell " + cell));
            initCause(e);
        }
    }

    public CDLRenameFactory(final CDLFactoryInterface out,
                            final CDLNameInterfaceFactory nif) {
        this.out = out;
        this.nif = nif;
        this.ni = null;
    }

    private String convert(final HierName name) {
        return name.getAsString('.');
    }

    private HierName convert(final String name) {
        try {
            return HierName.makeHierName(name, '.');
        } catch (InvalidHierNameException e) {
            throw new RuntimeException(e.getMessage());
        }
    }

    private HierName renameNode(final HierName node) {
        return convert(renameNode(convert(node)));
    }

    private String renameNode(final String node) {
        try {
            return ni.renameNode(node);
        } catch (CDLRenameException e) {
            throw new RenameException("Cannot rename node: " + node, subckt, e);
        }
    }

    private HierName renameDevice(final HierName device) {
        try {
            return convert(ni.renameDevice(convert(device)));
        } catch (CDLRenameException e) {
            throw new RenameException(
                "Cannot rename device: " + device, subckt, e);
        }
    }

    private HierName renameSubCellInstance(final HierName instance) {
        try {
            return convert(ni.renameSubCellInstance(convert(instance)));
        } catch (CDLRenameException e) {
            throw new RenameException(
                "Cannot rename instance: " + instance, subckt, e);
        }
    }

    private String renameTransistorModel(final String model) {
        try {
            return ni.renameTransistorModel(model);
        } catch (CDLRenameException e) {
            throw new RenameException(
                "Cannot rename transistor model: " + model, subckt, e);
        }
    }

    private String renameCell(final String cell) {
        try {
            return ni.renameCell(cell);
        } catch (CDLRenameException e) {
            throw new RenameException("Cannot rename cell: " + cell, subckt, e);
        }
    }
    
    public void makeResistor(final HierName name, final HierName n1,
                             final HierName n2, final CDLLexer.InfoToken val,
                             final Map parameters, final Environment env) {
        out.makeResistor(renameDevice(name), renameNode(n1), renameNode(n2),
                         val, parameters, env);
    }

    public void makeCapacitor(final HierName name, final HierName npos,
                              final HierName nneg, final CDLLexer.InfoToken val,
                              final Map parameters, final Environment env) {
        out.makeCapacitor(renameDevice(name), renameNode(npos),
                          renameNode(nneg), val, parameters, env);
    }

    public void makeTransistor(final HierName name, final String type,
                               final HierName ns, final HierName nd,
                               final HierName ng, final HierName nb,
                               final CDLLexer.InfoToken width,
                               final CDLLexer.InfoToken length,
                               final Map parameters, final Environment env) {
        out.makeTransistor(renameDevice(name), renameTransistorModel(type),
                           renameNode(ns), renameNode(nd), renameNode(ng),
                           renameNode(nb), width, length, parameters, env);
    }

    public void makeDiode(final HierName name, final String type,
                          final HierName npos, final HierName nneg,
                          final CDLLexer.InfoToken val,
                          final Map parameters, final Environment env) {
        out.makeDiode(renameDevice(name), type, renameNode(npos),
                      renameNode(nneg), val, parameters, env);
    }

    public void makeInductor(final HierName name, final HierName npos,
                             final HierName nneg, final CDLLexer.InfoToken val,
                             final Map parameters, final Environment env) {
        out.makeInductor(renameDevice(name), renameNode(npos), renameNode(nneg),
                         val, parameters, env);
    }

    public void makeBipolar(final HierName name, final String type,
                            final HierName nc, final HierName nb,
                            final HierName ne, final CDLLexer.InfoToken val,
                            final Map parameters, final Environment env) {
        out.makeBipolar(renameDevice(name), type, renameNode(nc),
                        renameNode(nb), renameNode(ne), val, parameters, env);
    }

    public void makeCall(final HierName name, final String subName,
                         final HierName[] args, final Map parameters,
                         final Environment env) {
        final HierName[] newargs = new HierName[args.length];
        for (int i = 0; i < args.length; ++i) {
            newargs[i] = renameNode(args[i]);
        }
        out.makeCall(renameSubCellInstance(name), renameCell(subName),
                     newargs, parameters, env);
    }

    public void beginSubcircuit(final String subName, final String[] in,
                                final String[] out, final Map parameters,
                                final Environment env) {

        subckt = subName;

        try {
            ni = nif.getNameInterface( subName );
        }
        catch ( CDLRenameException e ) {
            throw new RenameException(
                "Cannot get name interface for cell: " + subName, null, e);
        }

        final String[] newin = new String[in.length];
        for (int i = 0; i < in.length; ++i) {
            newin[i] = renameNode(in[i]);
        }
        
        final String[] newout = new String[out.length];
        for (int i = 0; i < out.length; ++i) {
            newout[i] = renameNode(out[i]);
        }

        this.out.beginSubcircuit(renameCell(subName), newin, newout, parameters,
                                 env);
    }

    public void endSubcircuit(final String subName, final Environment env) {
        out.endSubcircuit(renameCell(subName), env);
        ni = null;
        subckt = null;
    }
}
