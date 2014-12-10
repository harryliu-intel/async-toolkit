package com.avlsi.layout;
import java.text.MessageFormat;
import com.avlsi.file.common.HierName;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.file.cdl.util.rename.CDLRenameException;
	
/**
 * Definition of an InPlace Node  - a Node that is adopted from a subcell
 */

public class APInPlaceNodeDef extends APNodeDef {		
    public APInPlaceNodeDef(HierName netName, HierName portName) {	
	super(netName, portName, PortDefinition.NONE);
    }		
    
    static final MessageFormat form = new MessageFormat( "( PinPlaceInPlace \"{0}\" )\n" );
    
    public String toSkillString(PinGlobal global) throws CDLRenameException   {     
	Object[] args = { getCadenceNetName( netName.toString() ) };	
	return form.format(args);	  	    
    }
      
    public APPortDef getInPlacePortDef() {
	return this;
    }
}	

	
