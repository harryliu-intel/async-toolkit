/**
 * Definition of a Port for a pin placer.
 */


package com.avlsi.layout;
import java.text.MessageFormat;
import com.avlsi.file.common.HierName;
import com.avlsi.file.cdl.util.rename.CDLRenameException;
import com.avlsi.file.cdl.util.rename.CadenceNameInterface;

public abstract class APPortDef {
    /**
     * The canonical name of the net attached
     **/
    protected HierName netName;

    /**
     * The full name of the port
     */
    protected HierName portName;
     
    /**
     * The direction (locale) of the port
     */
    protected int direction;   
  
    public int getDirection() {
	return direction;
    }
    
    public HierName getNetName() { 
	return netName;
    } 
    
    public HierName getPortName() {
	return portName;
    }
   
    public APPortDef(HierName netName, HierName portName, int direction) {
	this.netName = netName;
	this.portName = portName;
	this.direction = direction;
    }

    private static CadenceNameInterface cni =  new CadenceNameInterface();

    public static String getCadenceNetName(String netName) throws CDLRenameException {
	return cni.renameNode( netName );
    }

    /**
     * Converts the Port to a SkillString - the code to create the port
     */
    public abstract String toSkillString(PinGlobal global) throws CDLRenameException ; 
    public abstract boolean isNode();
    public abstract boolean isLeaf();
    public abstract APPortDef getInPlacePortDef();   
}


