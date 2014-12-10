/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.parser;

import java.lang.String;
import java.lang.System;

import java.util.Set;
import java.util.SortedSet;
import java.util.Map;
import java.util.Iterator;
import java.util.HashSet;
import java.util.List;

import com.avlsi.util.container.Pair;

import com.avlsi.cast.impl.Environment;

import com.avlsi.cast2.directive.DirectiveConstants;

import com.avlsi.file.common.HierName;

import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.LVSNodesHandler;

import com.avlsi.layout.LVSNodes;

/**
 * CDLFactory that adds LVSNodes to cells
 * - adds LVS nodes to the port lists of cells
 * - adds LVS node connections to the instantiations of subcells
 * - checks to see that the LVS nodes are used in the cell, and 
 *   throws if this is not the case
 * - forwards to LVSNodesHandler:
 *     startCell - called on beginSubCircuit
 *     lvsNodesForInstance - called on makeCall
 *     endCell - called on endSubCircuit if there were LVS nodes
 *     abortCell - called on endSubCircuit if there weren't LVS nodes
 **/

public class LVSNodesCDLFactory implements CDLFactoryInterface {
    private final LVSNodes mLVSNodes;
    private final CDLFactoryInterface mTarget;
    private final LVSNodesHandler mHandler;
    private Set mCurrNodes;
    private LVSNodes.Info mCurrCellLVSNodesInfo;
    

    public LVSNodesCDLFactory( final LVSNodes lvsNodes,
                               final CDLFactoryInterface target,
                               final LVSNodesHandler handler ) {
        mLVSNodes = lvsNodes;
        mTarget = target;
        mHandler = handler;
        mCurrNodes = null;
        mCurrCellLVSNodesInfo = null;
    }
    public void makeTransistor( HierName name, String type, HierName ns,
                                HierName nd, HierName ng, HierName nb,
                                CDLLexer.InfoToken w, CDLLexer.InfoToken l,
                                Map parameters, Environment env) {
        assert mCurrNodes != null;
        mTarget.makeTransistor( name, type, ns, nd, ng, nb, w, l, parameters, env );
        mCurrNodes.add( ns );
        mCurrNodes.add( nd );
        mCurrNodes.add( ng );
        mCurrNodes.add( nb );
    }
    public void makeDiode(HierName name, String type, HierName npos,
                          HierName nneg, CDLLexer.InfoToken val,
                          Map parameters, Environment env) {
        assert mCurrNodes != null;
        mTarget.makeDiode( name, type, npos, nneg, val, parameters, env );
        mCurrNodes.add( npos );
        mCurrNodes.add( nneg );
    }
    public void makeResistor(HierName name, HierName n1, HierName n2,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) { 
        assert mCurrNodes != null;
        mTarget.makeResistor( name, n1, n2, val, parameters, env );
        mCurrNodes.add( n1 );
        mCurrNodes.add( n2 );
    }
    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              CDLLexer.InfoToken val, Map parameters,
                              Environment env) {
        assert mCurrNodes != null;
        mTarget.makeCapacitor( name, npos, nneg, val, parameters, env );
        mCurrNodes.add( npos );

        mCurrNodes.add( nneg );
    }
    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) {
        assert mCurrNodes != null;
        mTarget.makeInductor( name, npos, nneg, val, parameters, env );
        mCurrNodes.add( npos );
        mCurrNodes.add( nneg );
    }
    public void makeBipolar(HierName name, String type, HierName nc,
                            HierName nb, HierName ne, CDLLexer.InfoToken val,
                            Map parameters, Environment env) {
        assert mCurrNodes != null;
        mTarget.makeBipolar( name, type, nc, nb, ne, val, parameters, env );
        mCurrNodes.add( nc );
        mCurrNodes.add( nb );
        mCurrNodes.add( ne );
    }
    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters, Environment env) {

        assert mCurrNodes != null;

        final List lvsNodesConnections = 
            mCurrCellLVSNodesInfo.getInstanceLVSNodesConnections( name );

        if ( ( lvsNodesConnections != null ) && 
             ( lvsNodesConnections.size() > 0 ) ) {

            final HierName[] newArgs = 
                new HierName[ args.length + lvsNodesConnections.size() ];

            int destIndex = 0;

            for ( int i = 0 ; i < args.length ; ++i ) {
                newArgs[destIndex] = args[i];
                ++destIndex;
                mCurrNodes.add( args[i].getCadenceString() );
            }
            final Iterator lvsNodesConnectionsIter = 
                lvsNodesConnections.iterator();
            
            while ( lvsNodesConnectionsIter.hasNext() ) {
                final Pair lvsNodeConnection = 
                    ( Pair ) lvsNodesConnectionsIter.next();
                final HierName lvsNode = ( HierName ) lvsNodeConnection.getFirst();
                newArgs[destIndex] = lvsNode;
                ++destIndex;
            }
            mHandler.lvsNodesForInstance( name, lvsNodesConnections );
            mTarget.makeCall( name, subName, newArgs, parameters, env );
        }
        else {
            mTarget.makeCall( name, subName, args, parameters, env );
        }
        for ( int i = 0 ; i < args.length ; ++i ) {
            mCurrNodes.add( args[i] );
        }
    }

    public void beginSubcircuit(String subName, String[] in, String[] out,
                                Map parameters, Environment env) {
        mCurrNodes = new HashSet();

        try {

            mCurrCellLVSNodesInfo = mLVSNodes.getLVSNodesSetsForCell( subName );
            //The startCell method of the handler must be called before
            //the beginSubcircuit method of the target factory.
            //Handler implementations are allowed to rely on this fact.
            mHandler.startCell( subName );
            
            final SortedSet lvsNodes = mCurrCellLVSNodesInfo.getLVSNodes();

            //The LVS nodes appear as inputs to the cell
            //iff there are not outputs
            final boolean addLVSNodesToInputs = out.length == 0;
            
            final String[] newInputs; 
            final String[] newOutputs;
            final String[] arrayToAddLVSNodesTo;
            int destIndex;

            if ( addLVSNodesToInputs ) {
                newInputs = new String[ lvsNodes.size() + in.length ];
                System.arraycopy( in, 0, newInputs, 0, in.length );
                arrayToAddLVSNodesTo = newInputs;
                destIndex = in.length;
                newOutputs = out;
            }
            else {
                newOutputs = new String[ lvsNodes.size() + out.length ];
                System.arraycopy( out, 0, newOutputs, 0, out.length );
                arrayToAddLVSNodesTo = newOutputs;
                destIndex = out.length;
                newInputs = in;
            }
            
            final Iterator lvsNodesIter = lvsNodes.iterator();
            
            
            while ( lvsNodesIter.hasNext() ) {
                final HierName lvsNode = ( HierName ) lvsNodesIter.next();
                arrayToAddLVSNodesTo[destIndex] = lvsNode.getCadenceString();
                ++destIndex;
            }
            
            mTarget.beginSubcircuit( subName,
                                     newInputs,
                                     newOutputs,
                                     parameters,
                                     env );
        }
        catch ( LVSNodes.LVSNodesException e ) {
            throw new RuntimeException( "Unable to get LVSNodes.Info for \"" +
                                        subName +
                                        "\".",
                                        e );
        }


    }
    public void endSubcircuit(String subName, Environment endEnv ) { 
        assert mCurrCellLVSNodesInfo != null;
        assert mCurrNodes != null;
        

        final SortedSet topLevelLVSNodes = 
            mCurrCellLVSNodesInfo.getTopLevelLVSNodes();

        assert topLevelLVSNodes != null;
        
        if ( topLevelLVSNodes.size() > 0 ) {
            
            final Iterator topLevelLVSNodesIter = topLevelLVSNodes.iterator();
            
            while ( topLevelLVSNodesIter.hasNext() ) {
                final HierName lvsNode = ( HierName ) topLevelLVSNodesIter.next();
                
                if ( ! (  mCurrNodes.contains( lvsNode ) ) ) {
                    throw new RuntimeException( "\"" + 
                                                lvsNode.getCadenceString() +
                                                "\" is not a node in \"" +
                                                subName +
                                                "\", so it can not be in the " +
                                                DirectiveConstants.LVS_NODES +
                                                " directive." );
                }
                
            }
        }
        
        mTarget.endSubcircuit( subName, endEnv );
        
        final SortedSet lvsNodes = mCurrCellLVSNodesInfo.getLVSNodes();
        
        /* It is critical that we call the abort or endCell method
           of the handler AFTER we have executed all the statements of the
           accumulated template.  Handler implementations are allowed to
           assume that abort or endCell is called after the call to
           endSubcircuit. */
        if ( lvsNodes.size() == 0 ) {
            mHandler.abortCell( subName );
        }
        else {
            mHandler.endCell( subName, lvsNodes );
        }
        
        mCurrNodes = null;
        mCurrCellLVSNodesInfo = null;
    }
}
