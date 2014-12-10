/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.tools.ipgen;

import java.util.Map;
import java.util.regex.Pattern;

import com.avlsi.tools.prs2verilog.verilog.ContinuousAssign;
import com.avlsi.tools.prs2verilog.verilog.Delay;
import com.avlsi.tools.prs2verilog.verilog.Expr;
import com.avlsi.tools.prs2verilog.verilog.Ident;
import com.avlsi.tools.prs2verilog.verilog.ModuleInst;
import com.avlsi.tools.prs2verilog.verilog.Module;
import com.avlsi.tools.prs2verilog.verilog.NetDecl;
import com.avlsi.tools.prs2verilog.verilog.ParameterDecl;
import com.avlsi.tools.prs2verilog.verilog.Primitive;
import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogFactoryInterface;
import com.avlsi.tools.prs2verilog.verilog.VerilogFactoryImpl;
import com.avlsi.tools.prs2verilog.verilog.TrivialVisitor;
import com.avlsi.tools.prs2verilog.NetgraphGateConverter;

import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

import com.avlsi.tools.ipgen.HashMapTableEmitter;
import com.avlsi.tools.ipgen.HashMapTableEmitterFactory;

public class RenamingVerilogFactory extends VerilogFactoryImpl {


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

    private abstract static class IdentRenameVisitor extends TrivialVisitor {
        protected final HashMapTableEmitter mTable;
        protected final CDLNameInterface mNamer;
        private final VerilogObject mObj;

        public IdentRenameVisitor( HashMapTableEmitter table, CDLNameInterface namer, VerilogObject objToVisit ) {
            mTable = table;
            mNamer = namer;
            mObj = objToVisit;
            mObj.accept( this );
        }

        public void ident( final String ident, final boolean escape ) {
            final RenamingIdent obj = ( RenamingIdent ) mObj;
            String newName = renameIdent( ident );
            
            if ( newName == null ) {
                final StringBuffer newNameBuffer = new StringBuffer();
                int i;
                for ( i = 0 ; i < ident.length() ; ++i) {
                    final char c = ident.charAt( i );
                    if ( ! ( Character.isLetterOrDigit( c ) ) ) {
                        if ( c == '\\' ) {
                            newNameBuffer.append( '\\' );
                        }
                        else if ( c == '`' ) {
                            newNameBuffer.append( '`' );
                        }
                        else {
                            newNameBuffer.append( '_' );
                        }
                    }
                    else {
                        newNameBuffer.append( c );
                    }
                }

                newName = newNameBuffer.toString();
            }
            obj.rename( newName );
        }

        public abstract String renameIdent( final String ident );
    }

    private static class CellNameVisitor extends IdentRenameVisitor {
    
        public CellNameVisitor( HashMapTableEmitter table, CDLNameInterface namer, VerilogObject objToVisit ) {
            super( table, namer, objToVisit );
        }

        public String renameIdent( final String ident ) {
            final String s = mTable.getGDSIICellNameForCastCellName( ident );
            try {
                if (s == null && mNamer != null)
                    return mNamer.renameCell( ident );
                else return s;
            } catch (CDLRenameException e) {
                return null;
            }
        }
    }

    private static class NodeNameVisitor extends IdentRenameVisitor {
        public NodeNameVisitor( HashMapTableEmitter table, CDLNameInterface namer, VerilogObject objToVisit ) {
            super( table, namer, objToVisit );
        }

        public String renameIdent( final String ident ) {
            final String s = mTable.getGDSIINodeNameForCastNodeName( ident );
            try {
                if (s == null && mNamer != null &&
                    !ident.equals(NetgraphGateConverter.CLK))
                    return mNamer.renameNode( ident );
                else return s;
            } catch (CDLRenameException e) {
                return null;
            }
        }
    }

    private static class InstanceNameVisitor extends IdentRenameVisitor {
        public InstanceNameVisitor( HashMapTableEmitter table, CDLNameInterface namer, VerilogObject objToVisit ) {
            super( table, namer, objToVisit );
        }

        public String renameIdent( final String ident ) {
            final String s = mTable.getGDSIIInstanceNameForCastInstanceName( ident );
            try {
                if (s == null && mNamer != null)
                    return mNamer.renameSubCellInstance( ident );
                else return s;
            } catch (CDLRenameException e) {
                return null;
            }
        }
    }


    private static class RenameVisitor extends TrivialVisitor {
        private final HashMapTableEmitter mCellMap;
        private final CDLNameInterface mNamer;
        private static final Pattern VERILOG_INSTANCE =
            Pattern.compile(".*\\$\\d+$");

        public RenameVisitor( final HashMapTableEmitter cellMap, final CDLNameInterface namer ) {
            mCellMap = cellMap;
            mNamer = namer;
        }

        public void moduleInst( final VerilogObject ident,
                                final VerilogObject module,
                                final VerilogObject[] parameters,
                                final VerilogObject[] ports ) {
            // leave the cell name untouched if the instance came from a
            // verilog block; this, sadly, depends on the naming convention
            if (!(ident instanceof RenamingIdent) ||
                !VERILOG_INSTANCE.matcher(((RenamingIdent) ident).getStr())
                                 .matches()) {
                new CellNameVisitor( mCellMap, mNamer, module );
                module.accept( this );  // if module is actually a macro
            }

            new InstanceNameVisitor( mCellMap, mNamer, ident );
            
            int i;
            for ( i = 0 ; i < ports.length ; ++i ) {
                new NodeNameVisitor( mCellMap, mNamer, ports[i] );
            }
        }

        public void netDecl( final String type,
                             final VerilogObject ident,
                             final VerilogObject delay ) {
            new NodeNameVisitor( mCellMap, mNamer, ident );
        }

        public void primitive( final String type,
                               final VerilogObject delay,
                               final VerilogObject ident,
                               final VerilogObject[] terminals ) {
            if ( ident != null ) {
                new InstanceNameVisitor( mCellMap, mNamer, ident );
            }
            int i;
            for ( i = 0 ; i < terminals.length ; ++i ) {
                new NodeNameVisitor( mCellMap, mNamer, terminals[i] );
            }
        }

        public void parameterDecl( final VerilogObject ident, final String type ) {
            new NodeNameVisitor( mCellMap, mNamer, ident );
        }

        public void concatOp(final VerilogObject[] elements) {
            for (int i = 0; i < elements.length; ++i) {
                new NodeNameVisitor(mCellMap, mNamer, elements[i]);
            }
        }

        public void namedPort(final VerilogObject portName, final VerilogObject port) {
            new NodeNameVisitor(mCellMap, mNamer, port);
        }

        public void macroUse(final VerilogObject ident,
                             final VerilogObject[] args) {
            new CellNameVisitor( mCellMap, mNamer, ident );
            if (args != null) {
                for (int i = 0; i < args.length; ++i) args[i].accept(this);
            }
        }
    }
    
    private final HashMapTableEmitterFactory mCellMaps; 
    private final CDLNameInterfaceFactory mCdlFactory;

    public RenamingVerilogFactory( final HashMapTableEmitterFactory cellMaps,
                                   final CDLNameInterfaceFactory cdlFactory ) {
        mCellMaps = cellMaps;
        mCdlFactory = cdlFactory;
    }

    public VerilogObject ident( final String ident, boolean escape ) {
        return new RenamingIdent( ident, escape );
    }

    public VerilogObject module( final VerilogObject ident,
                                 final VerilogObject[] ports,
                                 final VerilogObject[] items ) {
        final VerilogObject ret = super.module( ident,
                                                ports,
                                                items );
        
        final String cellName = ( ( RenamingIdent ) ident ).getStr();
        final HashMapTableEmitter cellMap = mCellMaps.getEmitterForCell( cellName );
        final CDLNameInterface namer;
        try {
            namer =
                mCdlFactory == null ? null 
                                    : mCdlFactory.getNameInterface( cellName );
        } catch (CDLRenameException e) {
            throw new RuntimeException("Cannot get name interface for " +
                                       cellName, e);
        }
        if ( cellMap != null ) {
            int i;
            new CellNameVisitor( cellMap, namer, ident );
            for ( i = 0 ; i < ports.length ; ++i ) {
                new NodeNameVisitor( cellMap, namer, ports[i] );
            }
            
            final RenameVisitor visitor = new RenameVisitor( cellMap, namer );
            for ( i = 0 ; i < items.length ; ++i ) {
                items[i].accept( visitor );
            }
        }
        return ret;
    }
    
}
