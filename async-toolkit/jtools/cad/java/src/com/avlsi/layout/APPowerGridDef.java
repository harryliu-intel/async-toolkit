package com.avlsi.layout;
import java.text.MessageFormat;
import com.avlsi.file.common.HierName;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

public class APPowerGridDef extends APPitchedNodeDef {

   
    private int gridOffset;
    private int gridSpacing;
    
    public int getGridOffset() {
	return gridOffset;
    }
  
    public int getGridSpacing() {
	return gridSpacing;
    }

    public void setGridSpacing(int gridSpacing) {
	this.gridSpacing = gridSpacing;
    }

    public void setGridOffset(int gridOffset) {
	this.gridOffset = gridOffset;
    }

 
    public APPowerGridDef(HierName netName, HierName portName, float width, float spacing) {
	super(netName, portName, PortDefinition.INOUT, width, spacing);
    }  

    static final MessageFormat form = new MessageFormat( "( PinPlacePowerGrid \"{0}\" {1} {2} {3} {4})\n" );

    public String toSkillString(PinGlobal global) throws CDLRenameException   {	     

        final Object[] args;
        if( width != global.width ) {
            if( spacing != global.spacing )
                args = new Object[] { getCadenceNetName(portName.toString()), 
                                      new Integer(gridOffset), 
                                      new Integer(gridSpacing), 
                                      ""+width, 
                                      ""+spacing };
            else
                args = new Object[] { getCadenceNetName(portName.toString()),
                                      new Integer(gridOffset),
                                      new Integer(gridSpacing),
                                      ""+width, "" };
        }
        else
            args = new Object[] { getCadenceNetName(portName.toString()),
                                  new Integer(gridOffset),
                                  new Integer(gridSpacing), "", ""};
    
        
  
        

      return form.format(args);
    } 

    public boolean isNode() {
	return false;
    }

    public boolean isLeaf() {
	return true;
    }

    public APPortDef getInPlacePortDef() {
        return this;
    }
}
