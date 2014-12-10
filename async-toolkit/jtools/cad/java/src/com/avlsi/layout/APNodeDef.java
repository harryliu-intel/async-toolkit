package com.avlsi.layout;
import java.text.MessageFormat;
import com.avlsi.file.common.HierName;
import com.avlsi.fast.ports.PortDefinition;

/**
 * Definition of a Node extending {@link APPortDef }. Implements toSkillString() 
 */

public abstract class APNodeDef extends APPortDef {	 
    
    public APNodeDef(HierName netName, HierName portName, int direction) {	
	super(netName, portName, direction);	   	
    }		    
     
    public boolean isNode() {
	return true;
    }

    public boolean isLeaf() {
	return true;
    }
}
