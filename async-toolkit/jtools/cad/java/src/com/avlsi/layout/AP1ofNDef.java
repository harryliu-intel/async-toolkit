package com.avlsi.layout;
import java.text.MessageFormat;
import java.util.LinkedList;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Collection;

import com.avlsi.file.common.HierName;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.util.container.MappingIterator;
import com.avlsi.util.functions.UnaryFunction;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

/**
 * Definition of a 1ofN channel extending {@link APPortDef }. Implements toSkillString() and allows flattening
 */

public class AP1ofNDef extends APChannelDef implements Comparable {	
   
    private APPitchedNodeDef enablePort;
    private List   dataPorts;  
    private int      pitchSpacing;
    private int      type;
    public static final int OUTPUT_CHILDREN = 0;
    public static final int OUTPUT_PARENT   = 1;

    public AP1ofNDef( final HierName portName, 
                      final String portType, 
                      final APPitchedNodeDef enablePort, 
                      final List dataPorts,
                      final List allPorts,
                      final int direction,
                      final boolean bunched ) {	
	super(portName, portType, allPorts, direction, bunched);
	this.enablePort = enablePort;
	this.dataPorts = dataPorts;	   	
	this.type = OUTPUT_CHILDREN;
    }		

    public void setOutputType( final int type ) {
        if(type == OUTPUT_CHILDREN) 
	    this.type = OUTPUT_CHILDREN;
	else
	    this.type = OUTPUT_PARENT;
    }

    /**
     * Gets a Collection of the sub {@link APNodeDef}s of this channel
     */
    
    public Collection getNodes() {
	ArrayList allPorts = new ArrayList(dataPorts);
	if ( enablePort != null )
            allPorts.add( size()/2, enablePort );
	return allPorts;
    }
    
    public int compareTo( final Object o ) throws ClassCastException {
	return compareTo((AP1ofNDef) o);
    }
    
    public int compareTo( final AP1ofNDef o ) {
	return getN() - o.getN();
    }
    
    public int getEnablePitch() {
	return enablePort.getPitch();
    }

    public void setEnablePitch( final int enablePitch ) {
	enablePort.setPitch(enablePitch);
    }

    public void setDataPitches( final List dataPitches ) {
	Iterator i=dataPitches.iterator();
	Iterator j=dataPorts.iterator();
	for(; i.hasNext(); ) {
	    int pitch = ((Integer)i.next()).intValue();
	    APPitchedNodeDef node = (APPitchedNodeDef)j.next();
	    node.setPitch(pitch);
	}
    }
    
    public Iterator getDataPitches() {
	return new MappingIterator(dataPorts.iterator(), new UnaryFunction() {
		public Object execute(Object arg) {
		    APPitchedNodeDef node = (APPitchedNodeDef)arg;
		    return new Integer( node.getPitch() );
		}
	    } );
    }
    
    public HierName getEnableNetName() {
	return enablePort.getNetName();
    }

    public HierName getEnablePortName() {
	return enablePort.getPortName();
    }
    
    public int getPitchSpacing() {
	return pitchSpacing;
    }

    public void setPitchSpacing( final int pitchSpacing ) {
	this.pitchSpacing = pitchSpacing;
    }

    public Iterator getDataNetNames() {
	return new MappingIterator(dataPorts.iterator(), new UnaryFunction() {
		public Object execute(Object arg) {
		    APNodeDef node = (APNodeDef)arg;
		    return node.getNetName();
		}
	    } );
    }
    
    public Iterator getDataPortNames() {
	return new MappingIterator(dataPorts.iterator(), new UnaryFunction() {
		public Object execute(Object arg) {
		    APNodeDef node = (APNodeDef)arg;
		    return node.getPortName();
		}
	    } );
    }

    public int getN() {
	return dataPorts.size();
    }
    
    public int size() {
	return dataPorts.size() + ( enablePort == null ? 0 : 1 );
    }
    
    static final MessageFormat formNumList = new MessageFormat( "( list {0} )" );
    static final MessageFormat formNet = new MessageFormat( "\"{0}\" " );
    static final MessageFormat formLeft = new MessageFormat( "( PinPlaceLeft1ofN \"{0}\" ( list {1} ) {2} {3} {4} {5} )\n" );
    static final MessageFormat formRight = new MessageFormat( "( PinPlaceRight1ofN \"{0}\" ( list {1} ) {2} {3} {4} {5} )\n" );
    static final MessageFormat formLeftRight = new MessageFormat( "( PinPlaceLeftRight1ofN \"{0}\" ( list {1} ) {2} {3} {4} {5} )\n" );
    
    public String toSkillString( final PinGlobal global ) throws CDLRenameException {        
	if( size() >= 1 ) {
            if(type == OUTPUT_CHILDREN)
                return super.toSkillString(global);
            
            //get data net names
            String strData = "";
            for(Iterator i=getDataNetNames();  i.hasNext(); ) {
                HierName dataNetName = (HierName) i.next();
	    strData += formNet.format( new Object[] { getCadenceNetName(dataNetName.toString()) } );
            }
            
            //get width and spacings
            String strWidth = "";
            String strSpacing = "";       
            boolean mustWriteWidth=false;
            for(Iterator i=getNodes().iterator();  i.hasNext(); ) {
                APPitchedNodeDef node = (APPitchedNodeDef) i.next();
                strWidth += node.getWidth() + " ";
                strSpacing += node.getSpacing() + " ";
                if( node.getWidth() != global.width )
                    mustWriteWidth = true;
            }        
            strWidth = ( mustWriteWidth ? formNumList.format( new Object[] { strWidth } ) : "" ) ;
            strSpacing = ( mustWriteWidth ? formNumList.format( new Object[] { strSpacing }  ) : "" );
            
            
            Object[] args = { getCadenceNetName(getEnableNetName().toString()), strData, new Integer(getEnablePitch()), new Integer(pitchSpacing), strWidth, strSpacing };
            
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
        else
            return "";
    }

    public boolean isNode() {
	return false;
    }

    public boolean isLeaf() {
	return true;
    }

}
