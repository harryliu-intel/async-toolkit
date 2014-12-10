package com.avlsi.layout;

import java.util.List;
import java.util.LinkedList;
import java.util.Iterator;
import java.text.MessageFormat;
import com.avlsi.file.common.HierName;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

/**
 * Definition of a Channel extending {@link APPortDef }. 
 */

public class APChannelDef extends APPortDef {	 
    
    boolean bunched;
    String portType;
    List subPorts;

    public APChannelDef( final HierName portName, 
                         final String portType, 
                         final List subPorts,
                         final int direction,
                         final boolean bunched ) {
	super(null, portName, direction);
	this.portType = portType;
	this.subPorts = subPorts;     
	this.bunched = bunched;
    }

    public List getSubPorts() {
	return subPorts;
    }

    public boolean isBunched() {
	return bunched;
    }

    public boolean isFlattened() {
	boolean flattened=true;
	for(Iterator i=getSubPorts().iterator(); i.hasNext(); ) {
	    final APPortDef port = (APPortDef) i.next();
	    if( !port.isLeaf() ) {
		flattened = false;
		break;
	    }
	}
	return flattened;
    }

    public String toSkillString( final PinGlobal global ) throws CDLRenameException {
	String str="";
	for(Iterator i=getSubPorts().iterator(); i.hasNext(); ) {
	    APPortDef port = (APPortDef) i.next();
	    str += port.toSkillString(global);
	}
	
	return str;
    }

    public boolean isNode() {
	return false;
    }

    public boolean isLeaf() {
	return isBunched();
    }

    public List getLeafPorts() {
	List leafPorts = new LinkedList();
	for(Iterator i=getSubPorts().iterator(); i.hasNext(); ) {
	    APPortDef port = (APPortDef) i.next();	 
	    if( port.isLeaf() )
		leafPorts.add( port );
	    else if( port instanceof APChannelDef )	 
		leafPorts.addAll( ((APChannelDef)port).getLeafPorts() );	
	}
	return leafPorts;
    }

    public APPortDef getInPlacePortDef() {
	return new APInPlaceChannelDef(portName, portType, subPorts, bunched);
    }
}
