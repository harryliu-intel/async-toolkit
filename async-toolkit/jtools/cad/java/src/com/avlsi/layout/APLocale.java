
package com.avlsi.layout;

import java.util.LinkedList;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Enumeration;
import java.util.List;
import java.util.LinkedList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import com.avlsi.cell.CellInterface;
import com.avlsi.fast.ports.PortDefinition;

/**
 * Class representing a line of wire pitches in a cell. e.g. left or right edges.
 * This class places {@link APPortDef}s.  Should be called PinPlacer
 **/

public class APLocale {
    final private int locale;
    final private ArrayList reservedPitchList = new ArrayList();
    final private List globalTakenPitchList = new LinkedList();
    final private int pitches;
    final private float wirePitch;
    
    public APLocale( final int pitches, final float wirePitch, final int locale) {
	this.pitches = pitches;
        this.wirePitch = wirePitch;
	this.locale = locale;	    	   
    }
    
    private int getNodePitches( final APPitchedNodeDef node) {
        int ret =  (int) Math.ceil( ( node.getWidth() + node.getSpacing() ) / wirePitch );      
        return ret;
    }

    /**
     * Places an {@link APPowerGridDef} with the following offset and spacing
     * Reserves space for the grid, and returns the number of struts reserved
     **/
    private void placePowerGrid( final APPowerGridDef pg, final int pgOffset, final int pgSpacing ) {
	int gridPitch = pgOffset;

	pg.setGridOffset(pgOffset);
	pg.setGridSpacing(pgSpacing);
	
	while( gridPitch + getNodePitches(pg) <= pitches) {
	    reservePitch(gridPitch, getNodePitches(pg));
	    gridPitch += pgSpacing;
	}
    }
      
    /**
     * Inserts the given Integer value in a sorted list at the corrected location
     **/
    private void insertInList(final ArrayList list, final int value ) {	
	Integer obj = new Integer(value);
	int k = Collections.binarySearch( list, obj );
	if(k<0) {
	    k = -k - 1;
	    list.add( k, obj );	    
	}	    
    }   
    
     /**
     * Tests if the given pitch is occupied
     **/
    private boolean isOccupied( final int pitch, final int width, final List takenPitchList ) {
	for(int i = 0; i < width; i++) {	   
	    if( Collections.binarySearch( takenPitchList, new Integer(pitch+i) ) >= 0 )
		return true;
	}
	return false;
    }
    
    /**
     * Takes the given relative pitch
     * Return true if not already taken, false if already taken
     **/
    private void takePitch( final int pitch, final int width, final ArrayList takenPitchList ) {
	for(int i=0; i<width; i++) {
	    insertInList(takenPitchList, pitch+i);
	}
    }

    /**
     * Reserves the given absolute pitch.     
     **/
    private void reservePitch( final int pitch, final int width ) {	     
	for(int i=0; i<width; i++) {
	    insertInList(reservedPitchList, pitch+i);
	}		 
    }


    private int getRelativePitch( final int pitch ) {
	int k = Collections.binarySearch( reservedPitchList, new Integer(pitch) );
	if(k<0)
	    k = -k - 1;	
	return pitch-k;
    }

    private int getAbsolutePitch( final int pitch ) {
	return getAbsolutePitchImpl(0,2*pitches, pitch);
    }

    private int getAbsolutePitchImpl( final int min, final int max, final int pitch ) {
	int trial = (max+min)/2;
	int k=0;
		
	int maxEmptyAbs = trial;
	while( (k=Collections.binarySearch( reservedPitchList, new Integer(maxEmptyAbs) ) ) >= 0 ) {
	    maxEmptyAbs++;
	}  
	k = -k-1;
	int maxEmptyRel = maxEmptyAbs - k;
     	int minEmptyAbs = trial;
	while( (k=Collections.binarySearch( reservedPitchList, new Integer(minEmptyAbs) ) ) >= 0 ) {
	    minEmptyAbs--;
	}
	k = -k-1;
	int minEmptyRel = minEmptyAbs - k;

	/*
	  System.out.println( "" + min + " " + max + " " + pitch + " " + k);
	try {
	    System.in.read( new byte[1] );
	}
	catch( Exception e) { }
	*/

	//trial not in reserved list

	if( maxEmptyRel < pitch )
	    return getAbsolutePitchImpl(maxEmptyAbs, max, pitch);
	else if( minEmptyRel > pitch )
	    return getAbsolutePitchImpl(min, minEmptyAbs, pitch);	   	  
	else if (maxEmptyRel == pitch)
	    return maxEmptyAbs;
	else 
	    return minEmptyAbs;	    	
    }

    /**
     * Tests if the given {@link APPortDef} is associated with this locale
     **/
    private boolean isPort( final APPortDef def ) {
	if( def.getDirection() == locale || def.getDirection() == PortDefinition.INOUT )
	    return true;
	else 
	    return false;
    }

 
    /**
     * Places each {@link APPortDef} in the Vector of ports that came from the given cell.  
     * The power grid is reserved according to pgOffset and pgSpacing 
     **/
    public void placePorts( final CellInterface cell,
                            final List ports,
                            final int pgOffset, 
                            final int pgSpacing, 
                            final String pgGNDnet,
                            final String pgVddnet ) 
	throws PinPlaceException {	 

	LinkedList listPG               = new LinkedList();
	LinkedList listSub               = new LinkedList();
	LinkedList listInOut            = new LinkedList();
        int pgWidth = -1;

	//split up the ports
	for(Iterator i=ports.iterator(); i.hasNext(); ) {  	
	    APPortDef def = (APPortDef) i.next();	 
	    if( isPort( def )  )  {
		if(def.getDirection() == PortDefinition.INOUT &&
                   def instanceof APPitchedNodeDef ) {
		    if( def instanceof APPowerGridDef )
			listPG.add(def);
		    else
			listInOut.add(def);
		}
		else
		    listSub.add( def );
	    }	    
	}

        //setup pitch reservations(bound the viable pitches)
	reservePitch(-1, 1);
	reservePitch(pitches, 1);

	//power grid reserve
	for(Iterator i=listPG.iterator(); i.hasNext(); ) {  	
	    APPowerGridDef pg = (APPowerGridDef) i.next();	  		   
	    if( pg.getPortName().toString().equals(pgGNDnet) ) {
		placePowerGrid(pg, pgOffset, pgSpacing);
                pgWidth = getNodePitches( pg ); 
            }
	    else if( pg.getPortName().toString().equals(pgVddnet) ) {
		placePowerGrid(pg, pgOffset + pgSpacing/2, pgSpacing);
                pgWidth = getNodePitches( pg ); 
            }
	}
     
	//in-out nodes
        //in the middle
	for(Iterator i=listInOut.iterator(); i.hasNext(); ) {
	    APPitchedNodeDef node = (APPitchedNodeDef) i.next();
            final int relPitch = (pitches - reservedPitchList.size()) / 2 ;
            final int absPitch = getAbsolutePitch(relPitch);
            reservePitch(absPitch, getNodePitches(node));		 
            node.setPitch(absPitch);
	}
	
        if(pgSpacing <= pgWidth) {
            throw new PinPlaceException("Power grid spacing " + pgSpacing + " not sufficient");
        }
        if(pgWidth < 1 ) {
            throw new PinPlaceException("Power grid width " + pgWidth + " invalid");
        }

	//attempt to force interleave
	boolean success = 
	    placeSubPorts(listSub, 0, reservedPitchList.size()-1, pgSpacing, pgWidth, globalTakenPitchList, true);
	//if not, allow flattening
	if(!success) 
	    success = 
		placeSubPorts(listSub, 0, reservedPitchList.size()-1, pgSpacing, pgWidth, globalTakenPitchList, false);

	if(!success)
	    throw new PinPlaceException("Can't fit pins");
    }


    boolean place1ofN( final AP1ofNDef def,
                       final int enablePitch, 
                       final int pinSpacing, 
                       final int minPitch,
                       final int maxPitch, 
                       final ArrayList takenPitchList ) {
	//System.out.println( "1ofn: " + enablePitch + ", " + pinSpacing + ", " + minPitch + ", " + maxPitch);
	boolean bFit = true;	

        List nodes = new LinkedList( def.getNodes() );
	for(int j = 0; j<def.size(); j++) {	
            int pitch = ( j - (def.size()/2) ) * pinSpacing + enablePitch;	 
            int width = getNodePitches( (APPitchedNodeDef) nodes.get(j) );
	    //System.out.println( getAbsolutePitch( pitch) );
	    if(pitch < minPitch || pitch > maxPitch ||  
	       isOccupied(pitch, width, takenPitchList ) )
		bFit = false;
	}	   	
	if( bFit ) {
	    LinkedList pitchList = new LinkedList();
            for(int j = 0; j<def.size(); j++) {	
                int pitch = ( j - (def.size()/2) ) * pinSpacing + enablePitch;
                int width = getNodePitches( (APPitchedNodeDef) nodes.get(j) );
		takePitch(pitch, width, takenPitchList);
		int absPitch = getAbsolutePitch(pitch );
		if(j==(def.size()/2))
		{
                    def.setEnablePitch( absPitch );                 
                }
	        else
		    pitchList.add( new Integer( absPitch ) );		
	    }
		          
	    def.setPitchSpacing( pinSpacing );
	    def.setDataPitches( pitchList );	
	    def.setOutputType(AP1ofNDef.OUTPUT_PARENT);	
	    return true;
	}	
	else 
	    return false;
    }

    boolean placeSubPorts( final Collection ports,
                           final int minRegion, 
                           final int maxRegion, 
                           final int pgSpacing, 
                           final int pgWidth,	
                           final List parentTakenPitchList,
                           final boolean forceInterleave ) {

	int minPitch = getRelativePitch( ((Integer)reservedPitchList.get(minRegion)).intValue() );
	int maxPitch = getRelativePitch( ((Integer)reservedPitchList.get(maxRegion)).intValue() );     	

        //System.out.println( "rel min: " + minPitch + " max: :" + maxPitch);
        //System.out.println( "min: " + getAbsolutePitch(minPitch) + " max: :" + getAbsolutePitch(maxPitch-1) );
        //System.out.println( "pitches: " + pitches );

	if( maxPitch <= minPitch )
	    return false;

	ArrayList trialTakenPitchList = new ArrayList(parentTakenPitchList);

	LinkedList listNodes            = new LinkedList();
	LinkedList list1ofNBunched      = new LinkedList();
	LinkedList list1ofNSpread       = new LinkedList();
	LinkedList listBunchedChannels  = new LinkedList();

	int topUsedPitch = minPitch;

	//split up the ports
	for(Iterator i=ports.iterator(); i.hasNext(); ) {
	    APPortDef port = (APPortDef) i.next();
	    if( isPort( port )  )  {
		if(port instanceof APPitchedNodeDef) {
		    listNodes.add(port);		  
		}
		else if(port instanceof AP1ofNDef) {
		    AP1ofNDef chan = (AP1ofNDef) port;		  
		    if(chan.isBunched())
			list1ofNBunched.add(chan);		     
		    else
			list1ofNSpread.add(chan);	
		}
		else if(port instanceof APChannelDef) {
		    APChannelDef chan = (APChannelDef) port;	
		    if( chan.isBunched() )			
			listBunchedChannels.add( chan );
		    else {
			for(Iterator j=chan.getLeafPorts().iterator(); j.hasNext(); ) {
			    APPortDef subPort = (APPortDef) j.next();
			    if( isPort( subPort )  )  {
				if(subPort instanceof APPitchedNodeDef) {
				    listNodes.add(subPort);  
				}
				else if(subPort instanceof AP1ofNDef) {
				    AP1ofNDef subChan = (AP1ofNDef) subPort;		  
				    if(subChan.isBunched())
					list1ofNBunched.add(subChan);		     
				    else
					list1ofNSpread.add(subChan);	
				}
			    }
			}
		    }
		}
	    }
	}

	//sub bunched channels 
	int subMinRegion = minRegion;     ;
	for(Iterator i = listBunchedChannels.iterator(); i.hasNext(); ) {
	    APChannelDef chan = (APChannelDef) i.next();   

	    //attempt to force interleave
	    boolean interleaved = false;
	    for(int subMaxRegion = subMinRegion + 1; subMaxRegion < maxRegion; subMaxRegion++ ) {
		interleaved = 
		    placeSubPorts(chan.getSubPorts(), subMinRegion, subMaxRegion, pgSpacing, pgWidth,
				  trialTakenPitchList, true);	  

		if(interleaved) {
		    subMinRegion = subMaxRegion-1;
		    topUsedPitch = 
			getRelativePitch( ((Integer)reservedPitchList.get(subMinRegion)).intValue() );     
		    break;
		}	
	    }
	    
	    //allow flattening
	    boolean noninterleaved = false;
	    if( !interleaved ) {
		for(int subMaxRegion = subMinRegion + 1; subMaxRegion < maxRegion; subMaxRegion++ ) {	     
		    noninterleaved = 
			placeSubPorts(chan.getSubPorts(), subMinRegion, subMaxRegion, pgSpacing, pgWidth,
				      trialTakenPitchList, false);
		    
		    if(noninterleaved) {
			subMinRegion = subMaxRegion-1;
			topUsedPitch = 
			    getRelativePitch( ((Integer)reservedPitchList.get(subMinRegion)).intValue() );     
			break;
		    }
		}
	    }
	    if( !interleaved && !noninterleaved )
		return false;
	}

	//1ofn ports
	//sort them in order of decreasing size
	Collections.sort(list1ofNSpread, new Comparator() {
		public int compare(Object o1, Object o2) {
		    return compare((AP1ofNDef) o1, (AP1ofNDef) o2);
		}		  
		public int compare(AP1ofNDef o1, AP1ofNDef o2) {
		    return -o1.compareTo(o2);
		}	
	    } );

	Collections.sort(list1ofNBunched, new Comparator() {
		public int compare(Object o1, Object o2) {
		    return compare((AP1ofNDef) o1, (AP1ofNDef) o2);
		}		  
		public int compare(AP1ofNDef o1, AP1ofNDef o2) {
		    return -o1.compareTo(o2);
		}	
	    } );



	int enablePitch;
	//place the bunched 1ofNs
	for(Iterator i=list1ofNBunched.iterator(); i.hasNext(); ) {
	    AP1ofNDef def = (AP1ofNDef) i.next();
	    enablePitch = topUsedPitch + ( def.size() /2 ) + 1;	  
	    topUsedPitch += def.size(); 	
	    place1ofN(def, enablePitch, 1, minPitch, maxPitch, trialTakenPitchList);
	}

	//place the spread 1ofNs
	if(list1ofNSpread.size() > 0) {
	    int numUsedPitches = topUsedPitch + 1;
	    int numAvailableSpaces = maxPitch  - numUsedPitches;
	    int numSpacesBetweenGrid = pgSpacing/2 - pgWidth;
	    enablePitch = topUsedPitch  + (numAvailableSpaces - list1ofNSpread.size()) / 2;	 
	    int maxSize = ((AP1ofNDef)list1ofNSpread.get(0)).size();     
	    int pitchesToEnable = enablePitch - numUsedPitches;
	    
	    int maxPinSpacing;
            if ( maxSize < 2 ) {
                maxPinSpacing = 0;
            }
            else{
                maxPinSpacing = pitchesToEnable / ( maxSize / 2);
            }
	    int basePinSpacing = ( maxPinSpacing / numSpacesBetweenGrid) * numSpacesBetweenGrid;
	    if( basePinSpacing < list1ofNSpread.size() )
		basePinSpacing = maxPinSpacing;
	  
	    if ( ( basePinSpacing < 1 ) && ( maxSize > 1 ) )
		return false;

	    for(Iterator i=list1ofNSpread.iterator(); i.hasNext(); ) {	    
		AP1ofNDef def = (AP1ofNDef) i.next();
		
		while( enablePitch <= maxPitch) {
                    int pinSpacing;
                    if ( def.size() < 2 ) {
                        pinSpacing = 0;
                    }
                    else {
                        pinSpacing = pitchesToEnable / ( def.size() / 2 );
                    }
                    if ( basePinSpacing != 0 ) {
                        pinSpacing = ( pinSpacing / basePinSpacing ) * basePinSpacing;
                    }
                    else {
                        pinSpacing = 0;
                    }
		    boolean success =
			place1ofN(def, enablePitch, pinSpacing, minPitch, maxPitch, trialTakenPitchList);
		    // System.out.println( "" + basePinSpacing + " " + pinSpacing + " " + enablePitch );
		    enablePitch++;
		    if(success)
			break;
		}
		//if can't fit, then place them like flattened nodes, and use children output
		if(enablePitch > maxPitch ) {
		    if( forceInterleave )
			return false;
		    else {
			def.setOutputType(AP1ofNDef.OUTPUT_CHILDREN);		   
			listNodes.addAll ( def.getNodes() );	
		    }
		}				
	    }
	}

	//nodes
        int currPitch = minPitch;
        int numPitches = maxPitch - minPitch + 1;
	for(Iterator i = listNodes.iterator(); i.hasNext(); ) {
	    APPitchedNodeDef node = (APPitchedNodeDef) i.next();   	  
            boolean bFit = false;
	    for(int j=0; j < numPitches; j++) {
                int pitch = currPitch + j;
                if(pitch >= maxPitch )
                    pitch -= numPitches - 1;
		if( !isOccupied(pitch, getNodePitches(node), trialTakenPitchList) ) {
		    takePitch(pitch, getNodePitches(node), trialTakenPitchList);
                    node.setPitch(getAbsolutePitch(pitch));
                    currPitch += 2;
                    if(currPitch >= maxPitch) currPitch -= numPitches - 1;
                    bFit = true;
		    break;
		}
	    }
	    if( !bFit )
		return false; 
	}

	//sucess - commit changes and return true
	parentTakenPitchList.removeAll(trialTakenPitchList);
	parentTakenPitchList.addAll(trialTakenPitchList);
	return true;	
    }
}
