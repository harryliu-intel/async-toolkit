/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.netlist.util;

import com.avlsi.util.debug.Debug;

import java.util.List;
import java.util.LinkedList;
import java.util.Iterator;
import java.util.NoSuchElementException;

import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.AbstractDeviceIterator;
import com.avlsi.netlist.util.SubcellIterator;
import com.avlsi.netlist.util.CombiningNodeIterator;
import com.avlsi.netlist.AbstractNodeIterator;
import com.avlsi.netlist.AbstractNode;



public final class FlatteningIterator {
    
    

    public static final class FlatInstance {
        private final List mPathToInstance;
        private final AbstractNetlist mMasterNetlist;

        private FlatInstance( final List pathToInstance,
                              final AbstractNetlist masterNetlist ) {
            mPathToInstance = pathToInstance;
            mMasterNetlist = masterNetlist;
        }

        public final List getPathToInstance( ) {
            return mPathToInstance;
        }

        public final AbstractNetlist getInstanceMasterNetlist( ) {
            return mMasterNetlist;
        }

        public static String pathToString( final List path, final String seperator ) {
            final Iterator iter = path.iterator();
            final StringBuffer ret = new StringBuffer();
            boolean first = true;

            while ( iter.hasNext() ) {
                final String currComponent = ( String ) iter.next();
                if ( ! first ) {
                    ret.append( seperator );
                }
                first = false;
                ret.append( currComponent );
            }
            return ret.toString();
        }
        
        /*public final String getPathToInstanceAsString( final String seperator ) {
            return pathToString( getPathToInstance(), seperator );
        }

        public final String getPathToInstanceAsString( ) {
            return getPathToInstanceAsString( "." );
        }*/

        public final String getCanonicalNodeName( final String nodeName,
                                                  final String hierarchySeperator ) {
            final Iterator componentIter = mPathToInstance.iterator();
            
            String ret = nodeName;
            boolean done = false;

            if ( componentIter.hasNext() ) {
                
                PathComponent currComponent = null;
                while ( ( componentIter.hasNext() ) && ( !done ) ) { 
                    currComponent = ( PathComponent ) componentIter.next();
                    
                    final AbstractNetlist currNetlist = currComponent.getComponentNetlist();
                    
                    final AbstractNodeIterator portIter =
                        new CombiningNodeIterator( currNetlist.getInputNodes(),
                                                   currNetlist.getOutputNodes() );
                    int nodeIndexInPortList = 0;
                    boolean nodeFoundInPortList = false;
                    
                    while ( ( portIter.hasNext() ) && ( !nodeFoundInPortList ) ) {
                        final AbstractNode currPortNode = portIter.next();
                        final String currPortNodeName =
                            currPortNode.getCanonicalName().toString();
                        if ( currPortNodeName.equals( ret ) ) {
                            nodeFoundInPortList = true;
                        }
                        else {
                            ++nodeIndexInPortList;
                        }
                    }
                    
                    if ( nodeFoundInPortList ) {
                        if ( componentIter.hasNext() ) {
                            final AbstractNodeIterator connectedNodes =
                                currComponent.getConnectedNodes();
                            
                            int i;
                            
                            for ( i = 0 ; i < nodeIndexInPortList ; ++i ) { 
                                connectedNodes.next();
                            }
                            
                            final AbstractNode connectedNode = connectedNodes.next();
                            
                            ret = connectedNode.getCanonicalName().toString();
                            
                        }
                        else {
                            done = true;
                        }
                    }
                    else {
                        done = true;
                    }
                    
                } 
                {
                    final String currComponentName = currComponent.getComponentName();
                    if ( currComponentName != null ) {
                        ret = currComponentName + hierarchySeperator + ret;
                    }
                }
                
                while ( componentIter.hasNext() ) { 
                    final PathComponent curr = ( PathComponent ) componentIter.next();
                    final String currComponentName = curr.getComponentName();
                    if ( currComponentName != null ) {
                        ret = currComponentName + hierarchySeperator + ret;
                    }
                    else {
                        Debug.assertTrue( ! ( componentIter.hasNext() ) ) ;
                    }
                }
            }
            return ret;
        }

    }

    public interface PathComponent {
    
        String getComponentName();

        AbstractNetlist getComponentNetlist();

        AbstractNodeIterator getConnectedNodes();
    }

    private interface StackFrame extends PathComponent {
        SubcellIterator getIter();
    }

    private static class RootFrame implements StackFrame {
        
        private final AbstractNetlist mRootNetlist;
        private final SubcellIterator mIter;
        
        private RootFrame( final AbstractNetlist rootNetlist ) {
            mRootNetlist = rootNetlist;
            mIter = new SubcellIterator( getComponentNetlist().getDevices() );
        }

        public AbstractNetlist getComponentNetlist() {
            return mRootNetlist;
        }

        public String getComponentName() {
            return null;
        }

        public AbstractNodeIterator getConnectedNodes() {
            return new AbstractNodeIterator() {
                    public boolean hasNext() {
                        return false;
                    }

                    public AbstractNode next() {
                        throw new NoSuchElementException();
                    }
                };
        }
        
        public SubcellIterator getIter() {
            return mIter;
        }
    }

    private static class StackFrameImpl implements StackFrame {
        
        private final SubcellIterator mIter;

        private final SubcellIterator.SubcellInstance mComponentInstance;

        
        public String getComponentName() {
            return mComponentInstance.getName().toString();
        }

        public AbstractNetlist getComponentNetlist() {
            return mComponentInstance.getInstantiatedNetlist();
        }

        public AbstractNodeIterator getConnectedNodes() {
            return mComponentInstance.getConnectedNodes();
        }

        public StackFrameImpl( final SubcellIterator.SubcellInstance componentInstance ) {
            mComponentInstance = componentInstance;
            mIter = new SubcellIterator( getComponentNetlist().getDevices() );
        }
        
        public SubcellIterator getIter() {
            return mIter;
        }
    }
    
    private final LinkedList mStack;

    private AbstractNetlist mNextNetlist;

    private void push( final SubcellIterator.SubcellInstance componentInstance ) {
        final StackFrame newFrame = new StackFrameImpl( componentInstance );
        push( newFrame );
    }

    private void push( final StackFrame frameToPush ) {
        mStack.addFirst( frameToPush );
    }

    private SubcellIterator getTopIterator() {
        final StackFrame topFrame = ( StackFrame ) mStack.getFirst();
        return topFrame.getIter();
    }

    private AbstractNetlist getTopNetlist() {
        final StackFrame topFrame = ( StackFrame ) mStack.getFirst();
        return topFrame.getComponentNetlist();
    }
    
    private String getTopFrameName() {
       final StackFrame topFrame = ( StackFrame ) mStack.getFirst();
       return topFrame.getComponentName();
    }

    
    public FlatteningIterator( final AbstractNetlist top ) {
        mStack = new LinkedList();
        push( new RootFrame( top ) );
        mNextNetlist = null;
    }

    
    public boolean hasNext() {   
        while ( ( mNextNetlist == null ) && ( ! ( mStack.isEmpty() ) ) ) {
            final SubcellIterator currIter = getTopIterator();
 
            if ( currIter.hasNext() ) {
                final SubcellIterator.SubcellInstance instance = 
                    currIter.next();
                push( instance );
            }
            else {
                mNextNetlist = getTopNetlist();
            }
        }
        return mNextNetlist != null;
    }

    public FlatInstance next() {   
        if ( hasNext() ) {
            final FlatInstance ret = new FlatInstance( getCurrPath(),
                                                       mNextNetlist );
            mNextNetlist = null;
            mStack.removeFirst();
            return ret;
        }
        else {
            throw new NoSuchElementException();
        }
    }

    private List getCurrPath() {
        final LinkedList ret = new LinkedList();
        final Iterator iter = mStack.iterator();
        while ( iter.hasNext() ) {
            final StackFrame currFrame = ( StackFrame ) iter.next();
            ret.addLast( currFrame );
        }
        return ret;
    }
}
