/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.file.cdl.parser;

import java.lang.String;
import java.lang.System;
import java.lang.RuntimeException;

import java.util.Map;
import java.util.List;
import java.util.ListIterator;
import java.util.SortedSet;
import java.util.HashMap;
import java.util.LinkedList;

import com.avlsi.util.container.Pair;

import com.avlsi.file.common.HierName;

import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;

import com.avlsi.cast.impl.Environment;


import com.avlsi.layout.LVSNodes;

public class RemoveLVSNodesCDLFactory implements CDLFactoryInterface {

    private final LVSNodes mLVSNodes;
    private final CDLFactoryInterface mTarget;
    private final Map mLVSNodesPorts;
    private String mCurrCellName;
    private LVSNodes.Info mCurrCellLVSNodesInfo;

    public static class RemoveException extends RuntimeException {
        public RemoveException( final String str ) {
            super( str );
        }

        public RemoveException( final String str,
                                final Exception e ) {
            super( str, e );
        }

        public RemoveException( final Exception e ) {
            super( e );
        }
    }

    public RemoveLVSNodesCDLFactory( final LVSNodes lvsNodes,
                                     final CDLFactoryInterface target ) {
        mLVSNodes = lvsNodes;
        mTarget = target;
        mLVSNodesPorts = new HashMap();
        mCurrCellName = null;
        mCurrCellLVSNodesInfo = null;
    }
    
    
    public void makeTransistor( HierName name,
                                String type,
                                HierName ns,
                                HierName nd,
                                HierName ng,
                                HierName nb,
                                CDLLexer.InfoToken w,
                                CDLLexer.InfoToken l,
                                Map parameters,
                                Environment env ) {
        mTarget.makeTransistor( name, type, ns, nd, ng, nb, w, l, parameters, env );
    }
    public void makeDiode( HierName name,
                           String type,
                           HierName npos,
                           HierName nneg,
                           CDLLexer.InfoToken val,
                           Map parameters,
                           Environment env) {
        mTarget.makeDiode( name, type, npos, nneg, val, parameters, env );
    }
    public void makeResistor( HierName name,
                              HierName n1,
                              HierName n2,
                              CDLLexer.InfoToken val,
                              Map parameters,
                              Environment env) { 
        mTarget.makeResistor( name, n1, n2, val, parameters, env );
    }
    public void makeCapacitor( HierName name,
                               HierName npos,
                               HierName nneg,
                               CDLLexer.InfoToken val, 
                               Map parameters,
                               Environment env) {
        mTarget.makeCapacitor( name, npos, nneg, val, parameters, env );
    }
    public void makeInductor( HierName name,
                              HierName npos,
                              HierName nneg,
                              CDLLexer.InfoToken val,
                              Map parameters,
                              Environment env ) {
        mTarget.makeInductor( name, npos, nneg, val, parameters, env );
    }
    public void makeBipolar( HierName name,
                             String type,
                             HierName nc,
                             HierName nb,
                             HierName ne,
                             CDLLexer.InfoToken val,
                             Map parameters,
                             Environment env) {
        mTarget.makeBipolar( name, type, nc, nb, ne, val, parameters, env );
    }
    public void makeCall( HierName name,
                          String subName,
                          HierName[] args,
                          Map parameters, 
                          Environment env ) {

        final List lvsNodesConnections = mCurrCellLVSNodesInfo.getInstanceLVSNodesConnections( name );

        if ( ( lvsNodesConnections != null ) && 
             ( lvsNodesConnections.size() > 0 ) ) {

            final HierName[] lvsNodesPortsFromMasterDef =
                ( HierName [] ) mLVSNodesPorts.get( subName );

            if ( lvsNodesPortsFromMasterDef != null ) {

                final int numLVSNodes = lvsNodesConnections.size();

                assert args.length > numLVSNodes;

                final int numRealArgs = args.length - numLVSNodes;
                
                final ListIterator lvsNodesConnectionsIter = 
                    lvsNodesConnections.listIterator( numLVSNodes );
                int argIndex = ( args.length - 1 );
                int masterLVSPortIndex = lvsNodesPortsFromMasterDef.length - 1;
                boolean foundProblem = false;

                while ( ( argIndex >= numRealArgs ) &&
                        ( lvsNodesConnectionsIter.hasPrevious() ) &&
                        ( masterLVSPortIndex >= 0 ) &&
                        ( ! foundProblem ) ) {
                    final Pair lvsNodeConnectionPair =
                        ( Pair ) lvsNodesConnectionsIter.previous();
                    
                    final HierName castNodeName = 
                        ( HierName ) lvsNodeConnectionPair.getFirst();
                    
                    final HierName castNodeNameInSubcell =
                        ( HierName ) lvsNodeConnectionPair.getSecond();
                    
                    foundProblem = 
                        ( ( ! castNodeName.equals( args[ argIndex ] ) ) ||
                          ( ! castNodeNameInSubcell.equals( lvsNodesPortsFromMasterDef[ masterLVSPortIndex ] ) ) );
                    
                    --argIndex;
                    --masterLVSPortIndex;

                }

                if ( foundProblem ) {
                    ++argIndex;
                    ++masterLVSPortIndex;
                    final Pair lvsNodeConnectionPair =
                        ( Pair ) lvsNodesConnectionsIter.next();
                    final HierName castNodeName = 
                        ( HierName ) lvsNodeConnectionPair.getFirst();
                    
                    final HierName castNodeNameInSubcell =
                        ( HierName ) lvsNodeConnectionPair.getSecond();
                    
                    final String errorStr;
                    assert mCurrCellName != null;
                    if ( ! castNodeName.equals( args[ argIndex ] ) ) {
                        errorStr =
                            "In \"" +
                            mCurrCellName +
                            "\", on instance \"" +
                            name.getCadenceString() +
                            "\", the node \"" +
                            args[argIndex].getCadenceString() +
                            "\" should be \"" +
                            castNodeName.getCadenceString() +
                            "\" which should then be connected to \"" +
                            castNodeNameInSubcell.getCadenceString() +
                            "\" in the instantiated sub-cell.";
                    }
                    else if ( ! castNodeNameInSubcell.equals( lvsNodesPortsFromMasterDef[ masterLVSPortIndex ] ) ) {
                        errorStr =
                            "In \"" +
                            mCurrCellName +
                            "\", on instance \"" +
                            name.getCadenceString() +
                            "\", the node \"" +
                            args[argIndex].getCadenceString() +
                            "\" is connected to \"" +
                            lvsNodesPortsFromMasterDef[ masterLVSPortIndex].getCadenceString() +
                            "\" in the instantiated subcell, but it should connect to \"" +
                            castNodeNameInSubcell.getCadenceString() +
                            "\" instead.";
                    }
                    else {
                        errorStr = "Unknown LVSNode problem.";
                    }

                    throw new RemoveException( errorStr );
                }

                final HierName[] newArgs = new HierName[ numRealArgs ];
                
                System.arraycopy( args, 0, newArgs, 0, numRealArgs );
                
                mTarget.makeCall( name, subName, newArgs, parameters, env );
                
            }
            else {
                throw new RemoveException( "No LVSNode ports for \"" +
                                           subName +
                                           "\" which is instantiated as \"" +
                                           name.getCadenceString() +
                                           "\" in \"" +
                                           mCurrCellName +
                                           "\"." );
            }
                
        }
        else {
            mTarget.makeCall( name, subName, args, parameters, env );
        }
    }

    public void beginSubcircuit( String subName,
                                 String[] in,
                                 String[] out,
                                 Map parameters,
                                 Environment env ) {
        
        mCurrCellName = subName;
        try {

            mCurrCellLVSNodesInfo = mLVSNodes.getLVSNodesSetsForCell( subName );
            
            final SortedSet lvsNodes = mCurrCellLVSNodesInfo.getLVSNodes();

            final ListIterator lvsNodesIter = new LinkedList( lvsNodes ).listIterator( lvsNodes.size() );

            int i = out.length - 1;

            boolean foundProblem = false;

            String lvsNodeString = null;

            while ( ( ! foundProblem ) &&
                    ( i >= 0 ) &&
                    ( lvsNodesIter.hasPrevious() ) ) {
                final HierName lvsNode = ( HierName ) lvsNodesIter.previous();

                lvsNodeString = lvsNode.getCadenceString();
                
                foundProblem = ( ! lvsNodeString.equals( out[i] ) );
                --i;
            }

            if ( foundProblem ) {
                ++i;
                assert ( ! lvsNodeString.equals( out[i] ) );
                
                final String errorStr =
                    "In the port list of \"" +
                    subName +
                    "\", \"" +
                    out[i] +
                    "\" should be \"" +
                    lvsNodeString +
                    "\".";
                throw new RemoveException( errorStr );
            }

            final int outputLen = i + 1;
            
            i = in.length - 1;
            while ( ( ! foundProblem ) &&
                    ( i >= 0 ) &&
                    ( lvsNodesIter.hasPrevious() ) ) {
                final HierName lvsNode = ( HierName ) lvsNodesIter.previous();

                lvsNodeString = lvsNode.getCadenceString();
                
                foundProblem = ( ! lvsNodeString.equals( in[i] ) );
                --i;
            }

            foundProblem = foundProblem || 
                lvsNodesIter.hasPrevious() ||
                ( i < 0 );
            
            if ( foundProblem ) {
                final String errorStr;
                if ( i < 0 ) {
                    errorStr =
                        "\"" +
                        subName +
                        "\" only had lvs nodes in its port list.";
                }
                else if ( lvsNodesIter.hasPrevious() ) {
                    final HierName lvsNode = ( HierName ) lvsNodesIter.previous();
                    errorStr =
                        "\"" +
                        lvsNode.getCadenceString() +
                        "\" should be in the port list of \"" +
                        subName +
                        "\".";
                } else if ( ! lvsNodeString.equals( in[ i + 1 ] ) ) {
                    errorStr =
                        "In the port list of \"" +
                        subName +
                        "\", \"" +
                        in[ i + 1 ] +
                        "\" should be \"" +
                        lvsNodeString +
                        "\".";
                }
                else {
                   errorStr = "Unknown LVSNode problem.";
                }
                throw new RemoveException( errorStr );
            }

            final int inputLen = i + 1;

            final String[] newInput;
            
            assert inputLen <= in.length;

            if ( inputLen == in.length ) {
                newInput = in;
            }
            else {
                newInput = new String[ inputLen ];
                System.arraycopy( in, 0, newInput, 0, inputLen );
            }
            
            assert outputLen <= out.length;

            final String[] newOutput;
            if ( outputLen == out.length ) {
                newOutput = out;
            }
            else {
                newOutput = new String[ outputLen ];
                System.arraycopy( out, 0, newOutput, 0, outputLen );
            }

            
            final HierName[] lvsNodesArray = ( HierName[] ) lvsNodes.toArray( new HierName[0] );
            mLVSNodesPorts.put( subName, lvsNodesArray );
            
            mTarget.beginSubcircuit( subName,
                                     newInput,
                                     newOutput,
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
    public void endSubcircuit( String subName, Environment endEnv ) { 
        assert mCurrCellLVSNodesInfo != null;
        assert mCurrCellName != null;
        
        mTarget.endSubcircuit( subName, endEnv );
        
        mCurrCellName = null;
        mCurrCellLVSNodesInfo = null;    
    }

}
