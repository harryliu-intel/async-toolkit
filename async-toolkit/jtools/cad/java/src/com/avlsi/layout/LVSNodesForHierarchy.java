/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.layout;

import java.lang.String;
import java.lang.Exception;

import java.util.Map;
import java.util.SortedSet;
import java.util.HashMap;
import java.util.TreeSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Collections;
import java.util.ArrayList;
import java.util.List;

import com.avlsi.util.container.Pair;

import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;

import com.avlsi.cell.CellInterface;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSyntaxException;
import com.avlsi.cast.CastSemanticException;

import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;

import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;

import com.avlsi.layout.LVSNodes;


/**
   Class used to get sets of lvs nodes for cells.
 */
public class LVSNodesForHierarchy implements LVSNodes {

    /**
       Implements a stack frame for the stack
       used in getLVSNodesSetsForCell.
       
     */
    private static class Frame {
        private final String mCellName;
        private final HierName mInstanceName;
        private final CadenceInfo mCellInfo;
        private final Iterator mSubCellIter;
        private final SortedSet mLVSNodes;
        private final Map mInstanceLVSNodesConnections;

        public Frame( final String cellName,
                      final HierName instanceName,
                      final CadenceInfo cellInfo ) {
            mCellName = cellName;
            mInstanceName = instanceName;
            mCellInfo = cellInfo;
            mSubCellIter = cellInfo.getSubcellPairIterator();
            mLVSNodes = new TreeSet();
            mInstanceLVSNodesConnections = new HashMap();
        }

        public String getCellName() {
            return mCellName;
        }

        public CadenceInfo getCellInfo() {
            return mCellInfo;
        }

        public Iterator getSubcellPairIterator() {
            return mSubCellIter;
        }

        public SortedSet getLVSNodes() {
            return mLVSNodes;
        }

        public HierName getInstanceName() {
            return mInstanceName;
        }

        public Map getInstanceLVSNodesConnections() {
            return mInstanceLVSNodesConnections;
        }

    }

    private static class InfoImpl implements LVSNodes.Info {
        private final SortedSet mLVSNodes;
        private final SortedSet mTopLevelLVSNodes;
        private final Map mInstanceLVSNodesConnections;

        public InfoImpl( final SortedSet lvsNodes,
                         final SortedSet topLevelLVSNodes,
                         final Map instanceLVSNodesConnections ) {
            mLVSNodes = lvsNodes;
            mTopLevelLVSNodes = topLevelLVSNodes;
            mInstanceLVSNodesConnections = instanceLVSNodesConnections;
        }

        public SortedSet getLVSNodes() {
            return mLVSNodes;
        }

        public SortedSet getTopLevelLVSNodes() {
            return mTopLevelLVSNodes;
        }

        public List getInstanceLVSNodesConnections( final HierName instanceName ) {
            return ( List ) mInstanceLVSNodesConnections.get( instanceName );
        }
         
    }


    private final Map mLVSNodesMap;
    private final CastFileParser mCastParser;
    private final Cadencize mCadencizer;

    private final SortedSet mEmptySortedSet;
    private final Info mEmptyInfo;


    public LVSNodesForHierarchy( final CastFileParser castParser,
                                 final Cadencize cadencizer ) {
        mCastParser = castParser;
        mCadencizer = cadencizer;
        mLVSNodesMap = new HashMap();
        mEmptySortedSet = new TreeSet();
        mEmptyInfo = new InfoImpl( mEmptySortedSet, mEmptySortedSet, Collections.EMPTY_MAP );
    }

    public Info getLVSNodesSetsForCell( final String fullyQualifiedCellName ) 
        throws LVSNodes.LVSNodesException {
        final Info existingLVSNodesInfo = 
            ( Info ) mLVSNodesMap.get( fullyQualifiedCellName );
        
        if ( existingLVSNodesInfo == null ) {
            
            try {
            
                final CellInterface cell = 
                    mCastParser.getFullyQualifiedCell( fullyQualifiedCellName );
                final CadenceInfo cellInfo = mCadencizer.convert( cell );
                final LinkedList stack = new LinkedList();
                final Frame topFrame = new Frame( fullyQualifiedCellName,
                                                  null,
                                                  cellInfo );
                stack.addFirst( topFrame );
                
                while ( stack.size() > 0 ) {
                    final Frame currFrame = ( Frame ) stack.getFirst();
                    final CadenceInfo currInfo = currFrame.getCellInfo();
                    final Iterator subcellPairIter = currFrame.getSubcellPairIterator();
                    final SortedSet lvsNodes = currFrame.getLVSNodes();
                    final Map instanceLVSNodesConnections =
                        currFrame.getInstanceLVSNodesConnections();

                    if ( subcellPairIter.hasNext() ) {
                        final Pair subcellPair = ( Pair ) subcellPairIter.next();
                        final HierName instanceName = ( HierName ) subcellPair.getFirst();
                        final CadenceInfo masterInfo = ( CadenceInfo ) subcellPair.getSecond();
                        
                        final String masterCellName = masterInfo.getType();
                        
                        final Info existingLVSNodesInfoForMaster =
                            ( Info ) mLVSNodesMap.get( masterCellName );


                        
                        if ( existingLVSNodesInfoForMaster == null ) {
                            final Frame newFrame = new Frame( masterCellName,
                                                              instanceName,
                                                              masterInfo );
                            stack.addFirst( newFrame );
                        }
                        else {
                            final SortedSet lvsNodesForMaster = 
                                existingLVSNodesInfoForMaster.getLVSNodes();
                            if ( lvsNodesForMaster.size() > 0 ) {
                                final Iterator subCellLVSNodes = lvsNodesForMaster.iterator();
                                final List lvsNodesConnectionPairs = 
                                    new ArrayList( lvsNodesForMaster.size() );
                                while ( subCellLVSNodes.hasNext() ) {
                                    final HierName subCellLVSNode = 
                                        ( HierName ) subCellLVSNodes.next();
                                    final HierName lvsNode = 
                                        HierName.append( instanceName, subCellLVSNode );
                                    final Pair lvsNodeConnectionPair =
                                        new Pair( lvsNode, subCellLVSNode );
                                    lvsNodesConnectionPairs.add( lvsNodeConnectionPair );
                                }
                                instanceLVSNodesConnections.put( instanceName,
                                                                 lvsNodesConnectionPairs );
                            }
                        }
                    }
                    else {
                        final String currCellName = 
                            currFrame.getCellName();
                        final CellInterface currCell = 
                            mCastParser.getFullyQualifiedCell( currCellName );
                        final List lvsNodesFromDirective = ( List ) 
                            DirectiveUtils.getTopLevelDirective( currCell, 
                                                                 DirectiveConstants.LVS_NODES );
                        
                        final SortedSet topLevelLVSNodes;
                        if ( lvsNodesFromDirective != null ) {
                            topLevelLVSNodes = new TreeSet();
                            for ( int i = 0 ; i < lvsNodesFromDirective.size() ; ++i ) {
                                final String lvsNodeString = 
                                    ( String ) lvsNodesFromDirective.get(i);
                                
                                final HierName lvsNode = 
                                    HierName.makeHierName( lvsNodeString, '.' );
                                
                                final Boolean isInPortList = 
                                    ( Boolean ) currInfo.getPortNodes().getValue( lvsNode );
                                
                                if ( isInPortList == null ) {
                                    topLevelLVSNodes.add( lvsNode );
                                    lvsNodes.add( lvsNode );
                                }
                                else {
                                    final String errorString = "\"" +
                                        lvsNodeString +
                                        "\" is in the port list of \"" +
                                        currCellName +
                                        "\", thus it can not be in the \"" + 
                                        DirectiveConstants.LVS_NODES +
                                        "\" directive.";
                                    throw new LVSNodes.LVSNodesException( errorString );
                                }
                                
                            }
                        }
                        else {
                            topLevelLVSNodes = mEmptySortedSet; 
                        }
                        if ( ( lvsNodes.size() == 0 ) &&
                             ( instanceLVSNodesConnections.size() == 0 ) ) {
                            mLVSNodesMap.put( currCellName, mEmptyInfo );
                        }
                        else {
                            final SortedSet myLVSNodes;
                            if ( lvsNodes.size() == 0 ) {
                                myLVSNodes = mEmptySortedSet;
                            }
                            else {
                                myLVSNodes = lvsNodes;
                            }

                            final Info newInfo;
                            if ( instanceLVSNodesConnections.size() > 0 ) {
                                newInfo = 
                                    new InfoImpl( myLVSNodes,
                                                  topLevelLVSNodes,
                                                  instanceLVSNodesConnections );
                                
                            }
                            else {
                                newInfo = 
                                    new InfoImpl( myLVSNodes,
                                                  topLevelLVSNodes,
                                                  Collections.EMPTY_MAP );
                            }
                            mLVSNodesMap.put( currCellName,
                                              newInfo );
                        }
                        
                        final HierName currInstanceName = currFrame.getInstanceName();
                        stack.removeFirst();

                    }
                }
                final Info ret = ( Info ) mLVSNodesMap.get( fullyQualifiedCellName );
                assert ret != null;
                return ret;
            }
            catch ( InvalidHierNameException e ) {
                throw new LVSNodes.LVSNodesException( e );
            }
            catch ( CastSemanticException e ) {
                throw new LVSNodes.LVSNodesException( e );
            }
        }
        else {
            return existingLVSNodesInfo;
        }
    }

    public SortedSet getLVSNodesForCell( final String fullyQualifiedCellName ) 
        throws LVSNodes.LVSNodesException {
        return ( SortedSet ) getLVSNodesSetsForCell( fullyQualifiedCellName ).getLVSNodes();
    }

    public SortedSet getTopLevelLVSNodesForCell( final String fullyQualifiedCellName ) 
        throws LVSNodes.LVSNodesException {
        return ( SortedSet ) getLVSNodesSetsForCell( fullyQualifiedCellName ).getTopLevelLVSNodes();
    }
}
