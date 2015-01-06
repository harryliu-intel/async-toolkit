/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.tools.prs2verilog.verilog;

import java.util.Map;

import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

/**
 * Based on Chris' RenamingVerilogFactory which is part of genIP.
 **/
public class SimpleRenamingVerilogFactory extends VerilogFactoryImpl {
    private static class RenamingIdent extends Ident {
        
        private boolean renamed;

        public RenamingIdent( final String ident, boolean escape ) {
            super( ident, escape );
            renamed = false;
        }

        public void rename( final String newIdent ) {
            if ( ! ( renamed ) ) {
                ident = newIdent;
                renamed = true;
            }
        }
        
        public String getStr() {
            return ident;
        }
    }

    private abstract class IdentRenameVisitor extends TrivialVisitor {
        private final VerilogObject mObj;

        public IdentRenameVisitor(VerilogObject objToVisit ) {
            mObj = objToVisit;
            mObj.accept(this);
        }

        public void ident( final String ident, final boolean escape ) {
            final RenamingIdent obj = ( RenamingIdent ) mObj;
            String newName = null;
            try {
                newName = renameIdent( ident );
            } catch (CDLRenameException e) {
                throw new RuntimeException(e);
            }

            obj.rename( newName );
        }

        public abstract String renameIdent(final String ident)
            throws CDLRenameException;
    }

    private class CellNameVisitor extends IdentRenameVisitor {
        public CellNameVisitor(final VerilogObject objToVisit ) {
            super(objToVisit);
        }

        public String renameIdent(final String ident) throws CDLRenameException
        {
            return renamer.renameCell(ident);
        }
    }

    private class NodeNameVisitor extends IdentRenameVisitor {
        public NodeNameVisitor(VerilogObject objToVisit ) {
            super(objToVisit);
        }

        public String renameIdent(final String ident) throws CDLRenameException
        {
            return renamer.renameNode(ident);
        }
    }

    private class InstanceNameVisitor extends IdentRenameVisitor {
        public InstanceNameVisitor(VerilogObject objToVisit) {
            super(objToVisit);
        }

        public String renameIdent(final String ident) throws CDLRenameException
        {
            return renamer.renameSubCellInstance(ident);
        }
    }


    private class RenameVisitor extends TrivialVisitor {
        public RenameVisitor() { }

        public void arrayAccess( final VerilogObject array,
                                 final VerilogObject index ) {
            array.accept( this );
        }

        public void moduleInst( final VerilogObject ident,
                                final VerilogObject module,
                                final VerilogObject[] parameters,
                                final VerilogObject[] ports ) {
            new InstanceNameVisitor(ident);
            new CellNameVisitor(module);
            module.accept( this );  // if module is actually a macro
            
            int i;
            for ( i = 0 ; i < ports.length ; ++i ) {
                new NodeNameVisitor(ports[i]);
                ports[i].accept( this ); // if port is actually a namedPort
            }
        }

        public void netDecl( final String type,
                             final VerilogObject ident,
                             final VerilogObject delay ) {
            new NodeNameVisitor(ident);
        }

        public void primitive( final String type,
                               final VerilogObject delay,
                               final VerilogObject ident,
                               final VerilogObject[] terminals ) {
            if ( ident != null ) {
                new InstanceNameVisitor(ident);
            }
            int i;
            for ( i = 0 ; i < terminals.length ; ++i ) {
                new NodeNameVisitor(terminals[i]);
            }
        }

        public void parameterDecl(final VerilogObject ident,
                                  final String type ) {
            new NodeNameVisitor(ident);
        }

        public void concatOp(final VerilogObject[] elements) {
            for (int i = 0; i < elements.length; ++i) {
                new NodeNameVisitor(elements[i]);
            }
        }

        public void namedPort(final VerilogObject portName,
                              final VerilogObject port) {
            new NodeNameVisitor(portName);
            if (port != null) new NodeNameVisitor(port);
        }

        public void macroUse(final VerilogObject ident,
                             final VerilogObject[] args) {
            new CellNameVisitor(ident);
            if (args != null) {
                for (int i = 0; i < args.length; ++i) args[i].accept(this);
            }
        }
    }
    
    private final CDLNameInterface renamer;

    public SimpleRenamingVerilogFactory(final CDLNameInterface renamer) {
        this.renamer = renamer;
    }

    public VerilogObject ident(final String ident, boolean escape) {
        return new RenamingIdent(ident, escape);
    }

    public VerilogObject module(final VerilogObject ident,
                                final VerilogObject[] ports,
                                final VerilogObject[] items) {
        new CellNameVisitor(ident);
        for (int i = 0; i < ports.length; ++i) {
            new NodeNameVisitor(ports[i]);
        }
        
        final RenameVisitor visitor = new RenameVisitor();
        for (int i = 0; i < items.length; ++i ) {
            items[i].accept(visitor);
        }
        return super.module(ident, ports, items);
    }
}
