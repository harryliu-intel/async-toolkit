package com.avlsi.layout;
import com.avlsi.file.common.HierName;
import java.util.List;
import java.util.LinkedList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Collections;

public class APInPlacePortDefFactory extends APPortDefFactory {
    
    static final APDefaultPortDefFactory baseFactory = new APDefaultPortDefFactory();
  
    public APPortDef makePowerGridDef( final HierName netName,
                                       final HierName portName,
                                       final float width,
                                       final float spacing ) {
	return baseFactory.makePowerGridDef(netName, portName, width, spacing).getInPlacePortDef();
    }

    public APPortDef makeNodeDef( final HierName netName, 
                                  final HierName portName,
                                  final int direction,
                                  final float width,
                                  final float spacing ) { 
	return baseFactory.makeNodeDef(netName, portName, direction, width, spacing).getInPlacePortDef();
    }
      
    public APPortDef makeHorizontalStrutDef( final HierName netName, 
                                             final HierName portName, 
                                             final float width,
                                             final float spacing ) { 
	return baseFactory.makeHorizontalStrutDef(netName, portName, width, spacing).getInPlacePortDef();
    } 

    public APPortDef makeChannelDef( final HierName portName, 
                                     final String portType, 
                                     final List subPorts,
				     final int direction, 
                                     final boolean bunched ) {
	return baseFactory.makeChannelDef(portName, portType, subPorts, direction, bunched).getInPlacePortDef();
    }
 
}
