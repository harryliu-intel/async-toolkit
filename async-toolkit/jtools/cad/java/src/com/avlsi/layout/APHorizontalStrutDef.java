package com.avlsi.layout;
import java.text.MessageFormat;
import com.avlsi.file.common.HierName;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

public class APHorizontalStrutDef extends APPitchedNodeDef {
 
    public APHorizontalStrutDef(HierName netName, HierName portName, float width, float spacing) {
	super(netName, portName, PortDefinition.INOUT, width, spacing);      
    }

    static final MessageFormat form = new MessageFormat( "( PinPlaceHorizontalStrut \"{0}\" {1} {2} {3} )\n" );

    public String toSkillString(PinGlobal global) throws CDLRenameException   {	
        final Object[] args;
        if( width != global.width )            
            if( spacing != global.spacing )
                args = new Object[] { getCadenceNetName(netName.toString()), new Integer(pitch), ""+width, ""+spacing };
            else
                args = new Object[] { getCadenceNetName(netName.toString()), new Integer(pitch), ""+width, "" };
        else
            args = new Object[] { getCadenceNetName(netName.toString()), new Integer(pitch), "", ""};

      return form.format(args);
    }
}
