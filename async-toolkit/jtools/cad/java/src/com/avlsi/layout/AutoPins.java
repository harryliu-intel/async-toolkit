package com.avlsi.layout;

import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedList;
import java.util.Map;
import java.util.HashMap;
import java.util.HashSet;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.io.Writer;

import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.container.FilteringIterator;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.container.Pair;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.tools.cadencize.CadenceActionInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.cell.CellInterface;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

import com.avlsi.util.debug.Debug;

/**
 *  Class to automatically place Pins in a {@link CellInterface}.
 **/

public class AutoPins extends CellProcessor {
        
    final protected int pgOffset;
    final protected int pgSpacing;
    final protected float pgWidth;
    final protected String pgGNDnet;
    final protected String pgVddnet;
    final protected float wirePitch;
    protected float cellHeight;
    protected String pinType;
    final protected Pair pinLayer;


    public AutoPins(CellInterface cell, Cadencize cad)
        throws PinPlaceException {
        this(cell, cad, null, -1);
    }

    public AutoPins(CellInterface cell, Cadencize cad, String pinType,
                    float height)
        throws PinPlaceException {
	super(cell, cad);
        
        //get cell directives

            //power grid GND net
            pgGNDnet = (String) getDirective(cell, DirectiveConstants.POWERGRID_GNDNET);
            
            //power grid Vdd net
            pgVddnet = (String) getDirective(cell, DirectiveConstants.POWERGRID_VDDNET);
            
            //cell-height
            final float cellHeightInBitPitches = 
                ((Float) getDirective(cell, DirectiveConstants.HEIGHT)).floatValue();
            final float cellBitPitch = 
                ((Float) getDirective(cell, DirectiveConstants.BITPITCH)).floatValue();
            if(cellBitPitch < 0 )
                cellHeight = -1;
            else
                cellHeight = cellHeightInBitPitches * cellBitPitch;
            
            //wire-pitch
            pinLayer = (Pair) getDirective(cell, DirectiveConstants.PINLAYER);
            final Map layerWirePitchMap = 
                getDirective(cell, 
                             DirectiveConstants.LAYER_WIREPITCH, 
                             DirectiveConstants.LAYER_TYPE);
            final Float wirePitchObj = (Float) layerWirePitchMap.get(pinLayer);
            if(wirePitchObj == null)
                wirePitch = -1;
            else 
                wirePitch = wirePitchObj.floatValue();
            
            final Map layerPGOffsetMap = 
                getDirective(cell, 
                             DirectiveConstants.LAYER_POWERGRID_OFFSET, 
                             DirectiveConstants.LAYER_TYPE);
            pgOffset = ((Integer)layerPGOffsetMap.get(pinLayer)).intValue();
            
            final Map layerPGSpacingMap = 
                getDirective(cell, 
                             DirectiveConstants.LAYER_POWERGRID_SPACING, 
                             DirectiveConstants.LAYER_TYPE);
            pgSpacing = ((Integer)layerPGSpacingMap.get(pinLayer)).intValue();
            
            final Map layerPGWidthMap = 
                getDirective(cell, 
                             DirectiveConstants.LAYER_POWERGRID_WIREWIDTH, 
                             DirectiveConstants.LAYER_TYPE);
            
            pgWidth = ((Float)layerPGWidthMap.get(pinLayer)).floatValue();
            
            //pintype        
            // unless one was explicitly specified, try to figure out the
            // proper pintype
            if (pinType == null ||
                (!pinType.equals(DirectiveConstants.PINTYPE_INPLACE) &&
                 !pinType.equals(DirectiveConstants.PINTYPE_PITCHED))) {
                //force in-place if no height
                if(cellHeight < 0 || wirePitch < 0) 
                    pinType = DirectiveConstants.PINTYPE_INPLACE;
                //otherwise get the command line if there
                else if(cell.containsCompletePrs() ) {
                    pinType =  DirectiveConstants.PINTYPE_PITCHED;        
                }
                //otherwise rely on directive
                else {          
                    pinType = (String) getDirective(cell, DirectiveConstants.PINTYPE);
                }
            }
               
            if(pinType != null )
                this.pinType = pinType;
            if(height > 0 )
                this.cellHeight = height;
            
    }

    public static Map getDirective(final CellInterface cell, 
                                   final String type,
                                   final String param)
        throws PinPlaceException {
        final Map d = DirectiveUtils.getTopLevelDirective(cell, type, param);
        if(d==null) { throw new PinPlaceException("Directive " + type + " could not be found!"); } 
        else return d;
    }

    public static Object getDirective(final CellInterface cell, 
                                      final String type)
        throws PinPlaceException {
        final Object d = DirectiveUtils.getTopLevelDirective(cell, type);
        if(d==null) { throw new PinPlaceException("Directive " + type + " could not be found!"); } 
        else return d;
    }
    
    /**
     * Places the pins from the given cell according to power grid parameters: pgOffset, pgSpacing
     * Writes the Skill Code to file
     **/	  	    
    public void process(Writer writer)
	throws PinPlaceException, IOException {     
	APPortDefFactory portFactory;	

        Debug.assertTrue( pinType.equals( DirectiveConstants.PINTYPE_PITCHED ) || 
                          pinType.equals( DirectiveConstants.PINTYPE_INPLACE ) );

	if( pinType.equals( DirectiveConstants.PINTYPE_PITCHED ) ) {
	    if( pgGNDnet == null) 
		throw new PinPlaceException("No powergrid_gndnet directive or command line argument");	
	    if( pgVddnet == null) 
		throw new PinPlaceException("No powergrid_vddnet directive or command line argument");	
	    if( pgWidth < 0 )
		throw new PinPlaceException("No powergrid_width directive or command line argument");
	    if( pgSpacing < 0 )
		throw new PinPlaceException("No powergrid_spacing directive or command line argument");
	}

	int totalPitches = (int) (cellHeight / wirePitch);
	    
	if( pinType.equals( DirectiveConstants.PINTYPE_PITCHED ) ) {	    
	    portFactory = new APDefaultPortDefFactory();	  
	}
	else if( pinType.equals( DirectiveConstants.PINTYPE_INPLACE ) ) {	
	    portFactory = new APInPlacePortDefFactory();	   
	}
	else
	    throw new PinPlaceException("Invalid pintype directive or command line argument");

        PinGlobal global = new PinGlobal( getLayerFloatDirective(cell, pinLayer, DirectiveConstants.LAYER_WIREPITCH ), 
                                          getLayerFloatDirective(cell, pinLayer, DirectiveConstants.LAYER_WIREWIDTH ),
                                          getLayerFloatDirective(cell, pinLayer, DirectiveConstants.LAYER_WIRESPACING ) );
	
	CellInterfacePortCollector collector = 
	    new CellInterfacePortCollector(portFactory, 
                                           pinLayer,
                                           pgGNDnet+"|"+pgVddnet,
                                           "_?[Rr][Ee][Ss][Ee][Tt]",
                                           pgWidth,
                                           false,
                                           false,
                                           true);

	List ports = getPorts(null, collector);  
        List pitchedPorts = new LinkedList();
        CollectionUtils.addAll(pitchedPorts, 
                               new FilteringIterator(ports.iterator(), new UnaryPredicate() {
                                       public boolean evaluate(Object o) {
                                           return ( ! ( o instanceof APInPlaceNodeDef || o instanceof APInPlaceChannelDef ) );
                                       }
                                   } )
                               );
                                               
        APLocale[] locales = new APLocale[2];
        for(int i=0; i<locales.length; i++)
            locales[i]  = new APLocale(totalPitches, wirePitch, i);	  	  	   	   		   
        
        for(int i =0; i<locales.length; i++) {	      	    
            locales[i].placePorts(cell, pitchedPorts, pgOffset, pgSpacing, pgGNDnet, pgVddnet);
        }

        writer.write( global.toSkillString() );
        
	//do inout first
	for(Iterator i=ports.iterator(); i.hasNext(); ) {
	    APPortDef def = (APPortDef) i.next();
	    if( def.getDirection() == PortDefinition.INOUT ){
                try {
                    writer.write( def.toSkillString(global) );
                }
                catch ( CDLRenameException e ) {
                    throw new PinPlaceException( e );
                }
            }
	}

       

	//now do others
	for(Iterator i=ports.iterator(); i.hasNext(); ) {
	    APPortDef def = (APPortDef) i.next();	
	    if( def.getDirection() != PortDefinition.INOUT ){
		try {
                    writer.write( def.toSkillString(global) );
                }
                catch ( CDLRenameException e ) {
                    throw new PinPlaceException( e );
                }
            }
	}
      
    }
}
 
