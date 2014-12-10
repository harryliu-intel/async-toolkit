package com.avlsi.layout;
import com.avlsi.file.common.HierName;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.LinkedList;
import java.util.Iterator;

public class APDefaultPortDefFactory extends APPortDefFactory {
    public APPortDef makePowerGridDef( final HierName netName, 
                                       final HierName portName, 
                                       final float width, 
                                       final float spacing ) {
	return  new APPowerGridDef(netName, portName, width, spacing);
    }

    public APPortDef makeNodeDef( final HierName netName,
                                  final HierName portName, 
                                  final int direction,
                                  final float width, 
                                  final float spacing ) { 
	//	System.out.println( netName + " " + portName + " " + direction );
	return new APPitchedNodeDef(netName, portName, direction, width, spacing);
    }
      
    public APPortDef makeHorizontalStrutDef( final HierName netName, 
                                             final HierName portName, 
                                             final float width, 
                                             final float spacing ) { 
	return new APHorizontalStrutDef(netName, portName, width, spacing);
    }

    public APPortDef makeChannelDef( final HierName portName,
                                     final String portType, 
                                     final List subPorts, 
				     final int direction,
 boolean bunched ) {
	boolean pitchedPorts = true;
	for(Iterator i = subPorts.iterator(); i.hasNext(); ) {
	    if( !( i.next() instanceof APPitchedNodeDef ) )
		pitchedPorts = false;
	}
	if( pitchedPorts && portType.matches( ".*1of[0-9]*.*" ) ) {	
	    APPitchedNodeDef enablePort = null;	  
	    List dataPorts = new LinkedList();
	    for(Iterator i = subPorts.iterator(); i.hasNext(); ) {
		APPitchedNodeDef node = (APPitchedNodeDef) i.next();
		 if( node.getPortName().getSuffixString().matches( ".*[ae]" ) ) {
		     enablePort = node;		    
		 }
		 else {
		     dataPorts.add( node );		   
		 }
	    }
	 
	    if(enablePort == null && dataPorts.size() != 0) {
		enablePort = (APPitchedNodeDef) dataPorts.remove( dataPorts.size() / 2 );	
	    }
	    
	    return new AP1ofNDef(portName, portType, enablePort, dataPorts, subPorts, direction, bunched);
	}
	else {
	    return new APChannelDef(portName, portType, subPorts, direction, bunched);
	}
    } 
}


