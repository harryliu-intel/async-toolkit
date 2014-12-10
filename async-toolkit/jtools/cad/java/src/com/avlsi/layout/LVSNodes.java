/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.layout;

import java.lang.String;
import java.lang.Exception;

import java.util.SortedSet;
import java.util.List;

import com.avlsi.file.common.HierName;


/**
   Interface used to get sets of lvs nodes for cells.
 */
public interface LVSNodes {

    class LVSNodesException extends Exception {
        public LVSNodesException( final Exception e ) {
            super( e );
        }
        
        public LVSNodesException( final String str ) {
            super( str );
        }

        public LVSNodesException( final String str,
                                  final Exception e ) {
            super( str, e );
        }
    }

    interface Info {
        SortedSet getLVSNodes();
        SortedSet getTopLevelLVSNodes();
        List getInstanceLVSNodesConnections( final HierName instanceName ); 
    }

    Info getLVSNodesSetsForCell( final String fullyQualifiedCellName ) throws LVSNodesException;

    SortedSet getLVSNodesForCell( final String fullyQualifiedCellName ) throws LVSNodesException;

    SortedSet getTopLevelLVSNodesForCell( final String fullyQualifiedCellName ) throws LVSNodesException;
}
