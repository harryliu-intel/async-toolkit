package com.avlsi.layout;

import java.util.List;
import java.util.LinkedList;
import java.util.Iterator;

import com.avlsi.util.container.MappingIterator;
import com.avlsi.util.functions.UnaryFunction;
import com.avlsi.file.common.HierName;
import com.avlsi.fast.ports.PortDefinition;

public class APInPlaceChannelDef extends APChannelDef {	 

    public APInPlaceChannelDef( final HierName portName,
                                final String portType, 
                                final List subPorts, 
                                final boolean bunched ) {
	super(portName, portType, subPorts, PortDefinition.NONE, bunched);
    }
			       
    public List getSubPorts() {
	final List inplacePorts = new LinkedList();
	Iterator i = new MappingIterator(super.getSubPorts().iterator(), new UnaryFunction() {
		public Object execute(Object arg) {
		    APPortDef port = (APPortDef) arg;
		    return port.getInPlacePortDef();
		}
	    } );

	for(;i.hasNext(); ) {
	    inplacePorts.add( i.next() );
	}
	
	return inplacePorts;
    }
}
