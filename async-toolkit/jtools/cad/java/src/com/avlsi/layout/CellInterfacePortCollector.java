package com.avlsi.layout;

import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;

import com.avlsi.util.container.Pair;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.file.common.HierName;
import com.avlsi.cell.CellInterface;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;

public class CellInterfacePortCollector implements CellInterfaceActionInterface {
    final protected List ports;
    final protected String powerRegEx;
    final protected String inOutRegEx;
    final protected float pgWidth;
    final protected APPortDefFactory factory;
    final protected boolean addFlattenedNodes;
    final protected boolean addFlattenedLeafs;    
    final protected boolean addNonLeafPorts;
    final protected Pair layer;

    public CellInterfacePortCollector(APPortDefFactory factory, 
                                      Pair layer, 
                                      String powerRegEx,
                                      String inOutRegEx,
                                      float pgWidth,
				      boolean addFlattenedNodes,
                                      boolean addFlattenedLeafs,
                                      boolean addNonLeafPorts ) {
	this.factory = factory;
	this.addFlattenedNodes = addFlattenedNodes;
	this.addFlattenedLeafs = addFlattenedLeafs;
	this.addNonLeafPorts = addNonLeafPorts;
	this.powerRegEx = powerRegEx;
	this.inOutRegEx = inOutRegEx;
        this.pgWidth = pgWidth;
        this.layer = layer;
	this.ports = new ArrayList();
    }
    
    public void doLocalSubCell(CellInterface cell, CadenceInfo cellInfo, HierName cellName) {  }      
    
    public void doChannel(HierarchicalDirectiveInterface hdi, 
                          CellInterface subCell,
                          CadenceInfo cellInfo, 
			  HierName portName,
                          int direction) {
        
        //get bunched directive
        final Boolean bunchedObj = (Boolean) hdi.getDirectiveHierarchical(DirectiveConstants.CHANNEL_BUNCHED, 
                                                                          DirectiveConstants.CHANNEL_TYPE,
                                                                          portName );
        boolean bunched;		    
        if( bunchedObj == null)
            bunched = false;
        else
            bunched = bunchedObj.booleanValue();

        //get type
	String portType = subCell.getType();

        //recurse
	CellInterfacePortCollector portCollector = 
	    new CellInterfacePortCollector(factory, layer, powerRegEx, inOutRegEx, pgWidth,
					   addFlattenedNodes, addFlattenedLeafs, addNonLeafPorts );
	CellProcessor.walk( portCollector, hdi, subCell, layer, cellInfo, portName, direction);
	List subPorts = portCollector.getPorts();
	APChannelDef channel = (APChannelDef) 
	    factory.makeChannelDef(portName, portType, subPorts, direction, bunched);
	if( addFlattenedNodes || addFlattenedLeafs )  {
	    for(Iterator i=subPorts.iterator(); i.hasNext(); ) {
		APPortDef port = (APPortDef) i.next();
		if( addFlattenedLeafs && port.isLeaf() )
		    ports.add( port );
		else if( addFlattenedNodes && port.isNode() )
		    ports.add( port );
	    }		  
	}
	if(channel.isLeaf() || addNonLeafPorts )       
	    ports.add(channel);      
    }	
  
    public void doPort(HierarchicalDirectiveInterface hdi, HierName netName, HierName portName, int direction) {
        //get directives
        float width = getNodeWireWidthHierarchical(hdi, layer, portName);                   
        float spacing = getNodeWireSpacingHierarchical(hdi, layer, portName);
	if( portName.toString().matches(powerRegEx) ) {
            // always place Vdd and GND first, because they cannot be moved
	    ports.add( 0,  factory.makePowerGridDef(netName, portName, pgWidth, spacing) );
        }
        else if( portName.toString().matches(inOutRegEx) ) {
            ports.add(  factory.makeHorizontalStrutDef(netName, portName, width, spacing) );
        }
        else if( netName.toString().matches(powerRegEx) ) {
	}
	else
	    ports.add(  factory.makeNodeDef(netName, portName, direction, width, spacing ) );	       
    }
    
    public List getPorts() {
	return ports;
    }




    

   public static float getNodeWireWidthHierarchical( HierarchicalDirectiveInterface hdi, Pair layer, HierName name ) {
        Float value = (Float) hdi.getDirectiveHierarchicalWithBackup( DirectiveConstants.WIREWIDTH, 
                                                                      DirectiveConstants.NODE_TYPE, 
                                                                      name,
                                                                      DirectiveConstants.LAYER_WIREWIDTH, 
                                                                      DirectiveConstants.LAYER_TYPE, 
                                                                      layer  );
        return value.floatValue();
    }

    public static float getNodeWireSpacingHierarchical( HierarchicalDirectiveInterface hdi, Pair layer, HierName name ) {
        Float value = (Float) hdi.getDirectiveHierarchicalWithBackup( DirectiveConstants.WIRESPACE,
                                                                      DirectiveConstants.NODE_TYPE,
                                                                      name,
                                                                      DirectiveConstants.LAYER_WIRESPACING,
                                                                      DirectiveConstants.LAYER_TYPE,
                                                                      layer  );
        return value.floatValue();
    }    

}
