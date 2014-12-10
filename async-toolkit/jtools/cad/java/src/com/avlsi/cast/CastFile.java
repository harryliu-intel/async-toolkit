/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.cast;

import java.util.Map.Entry;
import java.util.NoSuchElementException;

import antlr.ASTFactory;
import antlr.RecognitionException;
import antlr.collections.AST;

import com.avlsi.cast.impl.CastParserEnvironment;
import com.avlsi.cast.impl.AmbiguousLookupException;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.EnvironmentEntry;
import com.avlsi.cast.impl.Symbol;
import com.avlsi.cast.impl.UserDefinedValue;
import com.avlsi.cast.impl.Value;
import com.avlsi.cast.impl.InstanceValue;
import com.avlsi.cast.impl.SemanticWrapperException;

import com.avlsi.cell.CellImpl;
import com.avlsi.cell.CellInterface;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.file.common.HierName;

import com.avlsi.cast.impl.CellInterfaceCollectionIterator;
import com.avlsi.cast.impl.EnvironmentIterator;

/**
 * Class to represent a parsed cast file.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class CastFile {
    private final Environment env;

    /**
     * For the "no explicit instantiation" hack, the file needs to
     * have an environment.
     **/
    private final CastParserEnvironment cpe;

    /**
       CellImpl that contains instantiations from the root environment of the cast file.
     */
    private final CellImpl envCell;

    CastFile(final CastParserEnvironment cpe, final Environment env, final CellImpl envCell ) {
        
        this.cpe = cpe;
        this.env = env;
        this.envCell = envCell;

    }

    /**
     * Get the cell interface for a user-defined cell instantiated in
     * the file.  
     *
     * [This is being added as a hack so that the regression test suite without
     *  env blocks will work with a corresponding CSP2Java hack.  --dhilvert]
     **/
    public CellInterface getCellForInstanceName(final String instanceName) {
        try {
            Value v;
            InstanceValue iv;
            CellInterface cell;

            v = env.lookup(Symbol.create(instanceName));
            assert v != null : "no such symbol: " + instanceName;

            if (!(v instanceof InstanceValue)) {
                throw new AssertionFailure("Not an instance name.");
            }

            iv = (InstanceValue) v;
            cell = iv.getCell();

            return cell;

        } catch (AmbiguousLookupException e) {
            // XXX: should throw a real exception
            throw (AssertionFailure)
                new AssertionFailure("more than one such symbol")
                    .initCause(e);
        }
    }

    /**
     * Get the cell interface for a cell defined in the file.
     **/
    public CellInterface getCell(final String cellName)
        throws CastSemanticException {
        return getCell(cellName, null, null);
    }

    /**
     * Get the cell interface for a cell defined in the file.
     **/
    public CellInterface getCell(final String cellName, CellImpl parent, 
                                 final HierName inst)
        throws CastSemanticException {
        try {
            return cpe.getCell(env, cellName, parent, inst);
        } catch (SemanticWrapperException e) {
            throw new CastSemanticException(e.getCause(),
                    e.getFilename(), e.getLine(), e.getColumn());
        }
    }

    public CellInterface getEnvironmentCell( ) {
        return envCell;
    }

    public final CellInterfaceCollectionIterator getAllCellsPossible( ) {

        
        
        return new CellInterfaceCollectionIterator() {
                private final EnvironmentIterator m_EnvIter = env.iterator();

                private UserDefinedValue m_nextUDV = null;

                private final void updateNextUDV() {
                    try {
                        while ( ( m_nextUDV == null ) && ( m_EnvIter.hasNext() ) ) {
                            final EnvironmentEntry currValue = m_EnvIter.next();
                            if ( currValue.getValue() instanceof UserDefinedValue ) {
                                UserDefinedValue currUDV = ( UserDefinedValue ) currValue.getValue();
                               
                                if ( ! currUDV.hasMetaParams() ) {
                                    m_nextUDV = ( UserDefinedValue ) currValue.getValue();
                                }
                            }
                        }
                    }
                    catch ( NoSuchElementException e ) {
                        throw (AssertionFailure) 
                            new AssertionFailure
                            ( "Any iterator's next method failed even though"+
                              " hasNext returned true." ).initCause(e);
                    }
                }

                public final boolean hasNext() {
                    updateNextUDV();
                    return m_nextUDV != null;
                }

                public CellInterface next() throws CastSemanticException {
                    
                    updateNextUDV();
                    if ( m_nextUDV != null ) {
                        final CellInterface ret =
                            getCell('"' + m_nextUDV.getCellTypeName() + '"');
                        m_nextUDV = null;
                        return ret;
                    }
                    else {
                        throw new NoSuchElementException();
                    }
                }
                
            };

    }
}
