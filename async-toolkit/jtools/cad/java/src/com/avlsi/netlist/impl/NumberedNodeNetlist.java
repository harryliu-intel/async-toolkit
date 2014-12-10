/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist.impl;

import com.avlsi.file.common.HierName;
import com.avlsi.netlist.AbstractDevice;
import com.avlsi.netlist.AbstractDeviceIterator;
import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.AbstractNetlistIterator;
import com.avlsi.netlist.AbstractNode;
import com.avlsi.netlist.AbstractNodeIterator;
import com.avlsi.netlist.Visitor;

import java.util.Map;
import java.util.HashMap;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class NumberedNodeNetlist implements AbstractNetlist {

    private final Map m_nameToNumber;

    private final AbstractNetlist m_Wrapped;

    private final class Device implements AbstractDevice {
        private final class DeviceVisitor implements Visitor {
         
            private final Map m_nameToNumber;
            private final Visitor m_Visitor;
            
            public DeviceVisitor( final Map nameToNumber, final Visitor visitor ) {
            
                m_nameToNumber = nameToNumber;
                m_Visitor = visitor;
                
            }

            public void genericTransistor(final HierName name,
                                   final AbstractNode drain,
                                   final AbstractNode gate,
                                   final AbstractNode source,
                                   final AbstractNode bulk,
                                   final double length,
                                   final double width,
                                   final String type,
                                   final Map parameters) {
                m_Visitor.genericTransistor( name,
                                             new Node( m_nameToNumber, drain ),
                                             new Node( m_nameToNumber, gate ),
                                             new Node( m_nameToNumber, source ),
                                             new Node( m_nameToNumber, bulk ),
                                             length,
                                             width,
                                             type,
                                             parameters );
                
            }

    
            public void genericDiode(final HierName name,
                              final AbstractNode positive,
                              final AbstractNode negative,
                              final double length,
                              final double width,
                              final double perimeter,
                              final double area,
                              final String type,
                              final Map parameters) {
                m_Visitor.genericDiode( name,
                                        new Node( m_nameToNumber, positive ),
                                        new Node( m_nameToNumber, negative ),
                                        length,
                                        width,
                                        perimeter,
                                        area,
                                        type,
                                        parameters );
            }

    
            public void genericResistor(final HierName name,
                                        final AbstractNode node1,
                                        final AbstractNode node2,
                                        final double conductance,
                                        final Map parameters) {
                m_Visitor.genericResistor( name,
                                            new Node( m_nameToNumber, node1 ),
                                            new Node( m_nameToNumber, node2 ),
                                            conductance,
                                            parameters );
            }

    
            public void genericCapacitor(final HierName name,
                                         final AbstractNode positive,
                                         final AbstractNode negative,
                                         final double capacitance,
                                         final Map parameters) {
                m_Visitor.genericCapacitor( name,
                                             new Node( m_nameToNumber, positive ),
                                             new Node( m_nameToNumber, negative ),
                                             capacitance,
                                             parameters );
            }

    
            public void genericInductor(final HierName name,
                                        final AbstractNode positive,
                                        final AbstractNode negative,
                                        final double inductance,
                                        final Map parameters) {
                m_Visitor.genericInductor( name,
                                           new Node( m_nameToNumber, positive ),
                                           new Node( m_nameToNumber, negative ),
                                           inductance,
                                           parameters );
            }

    
            public void subcircuitCall(final HierName name,
                                       final AbstractNetlist circuit,
                                       final AbstractNodeIterator nodes,
                                       final Map parameters) {
                final AbstractNodeIterator portIter = new NodeIterator( m_nameToNumber, nodes );
                m_Visitor.subcircuitCall( name,
                                          circuit,
                                          portIter,
                                          parameters );
            }
        }

        private final Map m_NameToNumber;
        private final AbstractDevice m_InnerDevice;

        public Device( final Map nameToNumber, AbstractDevice innerDevice ) {
            m_NameToNumber = nameToNumber;
            m_InnerDevice = innerDevice;
        }

        public void accept(final Visitor visitor) {
            m_InnerDevice.accept( new DeviceVisitor( m_NameToNumber, visitor ) );
        }
    }

    private final class Node implements AbstractNode {
        
        private final int m_NodeNumber;
        private final AbstractNode m_Wrapped;
        private final Map m_NameToNumber;
        
        
        public  Node( Map nameToNumber, AbstractNode wrapped ) {
            m_Wrapped = wrapped;

            m_NameToNumber = nameToNumber;

            final Integer nodeNumber = ( Integer ) nameToNumber.get( wrapped.getCanonicalName() );

            m_NodeNumber = nodeNumber.intValue();

            
            
        }
        
       
        public HierName getCanonicalName() {
            return HierName.makeHierName( String.valueOf( m_NodeNumber ) );
        }

        public Iterator getAliases() {
            return new Iterator() {
                
                    final Iterator m_innerIter = m_Wrapped.getAliases();
                    
                    boolean m_didNumberAlias = false;
                    
                    final HierName m_NumberAlias = getCanonicalName();

                    public boolean hasNext() {
                        return ( ( ! m_didNumberAlias ) || ( m_innerIter.hasNext() ) );
                    }

                    public Object next() {
                        if ( m_didNumberAlias ) {
                            return m_innerIter.next();
                        }
                        else {
                            m_didNumberAlias = true;
                            return m_NumberAlias;
                        }
                    }
                    
                    public void remove() throws UnsupportedOperationException {
                        throw new UnsupportedOperationException();
                    }
                    

                };
        }

        public AbstractDeviceIterator getDevices() {
            return new AbstractDeviceIterator() {
                    private final AbstractDeviceIterator m_InnerIter = m_Wrapped.getDevices();
                    private final Map nameToNumber = m_NameToNumber;
                    
                    public final boolean hasNext() {
                        return m_InnerIter.hasNext();
                    }

                    public AbstractDevice next() {
                        return new Device( nameToNumber, m_InnerIter.next() );
                    }
                    
                };
        }

    }

    private final class NodeIterator implements AbstractNodeIterator {
        
        private final AbstractNodeIterator m_Wrapped;
        private final Map m_NameMap;

        public NodeIterator( Map nameMap, AbstractNodeIterator wrapped ) {
            m_Wrapped = wrapped;
            m_NameMap = nameMap;
        }

        public boolean hasNext() {
            return m_Wrapped.hasNext();
        }

        public AbstractNode next() {
            return new Node( m_NameMap, m_Wrapped.next() );
        }

        

    }

    public NumberedNodeNetlist( AbstractNetlist wrapped ) {
        
        m_Wrapped = wrapped;
        
        m_nameToNumber = new HashMap();

        final AbstractNodeIterator nodeIter = m_Wrapped.getNodes();

        int counter = 0;

        while ( nodeIter.hasNext() ) {
        
            final AbstractNode currNode = nodeIter.next();

            final HierName currNodeName = currNode.getCanonicalName();

            m_nameToNumber.put( currNodeName, new Integer( counter ) );
            ++counter;
           
        }

    }
      
    public AbstractNodeIterator getInputNodes() {
        return new NodeIterator( m_nameToNumber, m_Wrapped.getInputNodes() );
    }
    
    
    public AbstractNodeIterator getOutputNodes() {
        return new NodeIterator( m_nameToNumber, m_Wrapped.getOutputNodes() );
    }
    
    
    public HierName getName() {
        return m_Wrapped.getName();
    }
    
    
    public AbstractNodeIterator getNodes() {
        return new NodeIterator( m_nameToNumber, m_Wrapped.getNodes() );
    }
    
    
    public AbstractDeviceIterator getDevices() {
        return new AbstractDeviceIterator() {
                private final Map nameToNumber = m_nameToNumber;
                private final AbstractDeviceIterator m_InnerIter = m_Wrapped.getDevices();

                public boolean hasNext() {
                    return m_InnerIter.hasNext();
                }

                public AbstractDevice next() {
                    return new Device( m_nameToNumber, m_InnerIter.next() );
                }
                
            };
    }
    
    public AbstractNetlistIterator getSubcircuits() {
        return new AbstractNetlistIterator() {
                private AbstractNetlistIterator m_InnerIter = m_Wrapped.getSubcircuits();

                public boolean hasNext() {
                    return m_InnerIter.hasNext();
                }

                public AbstractNetlist next() {
                    return new NumberedNodeNetlist( m_InnerIter.next() );
                }

            };
    }
}
