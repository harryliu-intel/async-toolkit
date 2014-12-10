
package com.avlsi.layout;
import java.text.MessageFormat;
import com.avlsi.file.common.HierName;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

/**
 * Definition of a Node with a pitch extending {@link APNodeDef }. Implements toSkillString() 
 */

public class APPitchedNodeDef extends APNodeDef {	 
  
    public APPitchedNodeDef(HierName netName, HierName portName, int direction, float width, float spacing) {
	super(netName, portName, direction);
	this.width = width;
        this.spacing = spacing;
    }
     
    /**
     * The pitch index of the port
     **/

    public static final int NONE = -1;

    protected int pitch = NONE;
 
    public void setPitch(int pitch) {
	this.pitch = pitch;	    
    }
    
    public int getPitch() {
	return pitch;
    }	  
    
    protected float width;
    protected float spacing;

    public float getWidth() {
	return width;
    }
    
    public float getSpacing() {
        return spacing;
    }
  

    static final MessageFormat formLeft = new MessageFormat( "( PinPlaceLeft \"{0}\" {1} {2} {3} )\n" );
    static final MessageFormat formRight = new MessageFormat( "( PinPlaceRight \"{0}\" {1} {2} {3} )\n" );
    static final MessageFormat formLeftRight = new MessageFormat( "( PinPlaceLeftRightPin \"{0}\" {1} {2} {3}  )\n" );
    
    public String toSkillString(PinGlobal global) throws CDLRenameException {	
        final Object[] args;
        if( width != global.width )            
            if( spacing != global.spacing )
                args = new Object[] { getCadenceNetName(netName.toString()), new Integer(pitch), ""+width, ""+spacing };
            else
                args = new Object[] { getCadenceNetName(netName.toString()), new Integer(pitch), ""+width, "" };
        else
            args = new Object[] { getCadenceNetName(netName.toString()), new Integer(pitch), "", ""};
        
	
	switch( direction ) {
	case PortDefinition.IN:	    
	    return formLeft.format(args);	   
	case PortDefinition.OUT: 
	    return formRight.format(args);
	case PortDefinition.INOUT:
	    return formLeftRight.format(args);
	default:
	    return "";
	}
    } 

    public APPortDef getInPlacePortDef() {
       return new APInPlaceNodeDef(netName, portName);
    }
}

