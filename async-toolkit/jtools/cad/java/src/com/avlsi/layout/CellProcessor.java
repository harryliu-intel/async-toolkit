/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.layout;

import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedList;
import java.util.Collection;
import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.Writer;

import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast.CastSyntaxException;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastFile;
import com.avlsi.cast.impl.CellInterfaceCollectionIterator;
import com.avlsi.cell.CellInterface;
import com.avlsi.io.FileSearchPath;
import com.avlsi.io.SearchPath;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cast2.directive.UnknownDirectiveException;

import com.avlsi.file.common.HierName;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.cell.CellInterface;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.container.Pair;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.file.cdl.util.rename.CDLRenameException;
import com.avlsi.file.cdl.util.rename.CadenceNameInterface;
import com.avlsi.file.cdl.util.rename.CadenceReverseNameInterface;

/**
 * Command line program to process some CellInterfaces
 **/

public abstract class CellProcessor implements CellProcessorInterface {
  
    final protected CellInterface cell;
    final protected CadenceInfo   cellInfo;
    final protected Cadencize cdz;
    final protected HierarchicalDirectiveInterface hdi;


    protected static boolean bVerbose;
    //protected static boolean bNoProcess;


    final protected HashMap cellToPortListMap   = new HashMap();
    final protected HashMap ciMap         = new HashMap();
    final protected HashMap cellToPortMapMap         = new HashMap();

    public static String getCASTNameFromCadenceName( final String cadenceName ) throws CDLRenameException {      
        String mungedName = cadenceName.replaceAll( "#2d", "-" );
        mungedName = mungedName.replaceAll( "#2e", "." );
        CadenceReverseNameInterface crni = new CadenceReverseNameInterface();        
        mungedName = crni.renameCell( mungedName );
        return mungedName;
    }

    public static String getUnEncodedCadenceNameFromCASTName( final String castName )
        throws CDLRenameException {
    
        final CadenceNameInterface cni = new CadenceNameInterface();        
        return cni.renameCell( castName );
    }

    public static String getCadenceNameFromCASTName( final String castName ) throws CDLRenameException {
        String mungedName = getUnEncodedCadenceNameFromCASTName( castName );
        mungedName = mungedName.replaceAll( "-", "#2d" );
        mungedName = mungedName.replaceAll( "\\.", "#2e" );
        return mungedName;
    }

    public static File getCadenceDirectoryFromCadenceName ( final String cadenceWD,
                                                            final String cadenceCellName ) {
        File cadenceDir = new File(cadenceWD); 
        final String[] dirs = cadenceCellName.split("#2e");      
        
        final int numDirs = dirs.length - 2;
        for ( int i = 0 ; i < numDirs ; i++ ) {
            cadenceDir = new File(cadenceDir, dirs[i]);
        }
        cadenceDir = new File( cadenceDir, cadenceCellName );

        return cadenceDir;
    }

    public static File getCadenceDirectory( final String cadenceWD, final String cellName) throws CDLRenameException {
        final String mungedName = getCadenceNameFromCASTName( cellName );
        return getCadenceDirectoryFromCadenceName( cadenceWD, mungedName );
    }

    public CellProcessor( final CellInterface cell ) {
        this(cell, new Cadencize(false));
    }

    public CellProcessor( final CellInterface cell, final Cadencize cdz ) {
        this.cell = cell;
        this.cdz = cdz;
        this.cellInfo = cdz.convert(cell);
        this.hdi = new HierarchicalDirectiveInterface(cell);
    }

    public static CommandLineArgs getArgs( final String[] args) {
        //setup args
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs );         
        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );
        final CommandLineArgs theArgs = cachedArgs;
        return theArgs;
    }

    public static List getCellNameStringsFromCellNameListFileName( String cellNameListFileName )
        throws IOException {
        final List cellNames = new LinkedList();
        final BufferedReader reader = new BufferedReader( new FileReader( new File(cellNameListFileName) ) );
        do {
            String anotherCellName = reader.readLine();                                                        
            if( anotherCellName != null) {                        
                if( !anotherCellName.equals("") ) {
                    cellNames.add(anotherCellName);
                }
            }
            else break;
        } while(true); 
        reader.close();
        return cellNames;
    }

    public static List getCells( CastFileParser parser, Collection cellNames ) 
        throws CDLRenameException, CastSemanticException, CastSyntaxException, IOException
    {
        final List cells = new LinkedList();
        for(Iterator i=cellNames.iterator(); i.hasNext(); ) {
            final String anotherCellName = (String)i.next();
            try {                  
                final CellInterface cell = parser.getFullyQualifiedCell( anotherCellName );                                    
                cells.add(  new Pair( cell, anotherCellName ) );
            }
            catch(Exception e) {
                String castName = getCASTNameFromCadenceName( anotherCellName );
                final CellInterface cell = parser.getFullyQualifiedCell( castName );                                    
                cells.add(  new Pair( cell, castName ) );
            }                
        }
             
        return cells;
    }
    
  
   
    public static HierName getCanonicalNodeName(CadenceInfo cellInfo, HierName name) {
        HierName cName = (HierName) cellInfo.getLocalNodes().getCanonicalKey(name);        
        return cName;
    }

    /**
     *  Returns the canonical -cadencized- name for the given node
     **/
    public HierName getCanonicalNodeName(HierName name) {
        return getCanonicalNodeName(cellInfo, name);
    }

    public List getPorts( final HierName cellName, final CellInterfacePortCollector collector) {
        if( cellToPortListMap.containsKey(cellName) ) {
            return (List) cellToPortListMap.get(cellName);
        }
        else {
            //then make a cell def with the ports
            CellInterface subCell;
            //get cell ports and put into a hashmap            
            if(cellName == null)
                subCell = cell;
            else
                subCell = cell.getSubcell(cellName);
                                          
            walk(collector, hdi, subCell, null, cellInfo, cellName, PortDefinition.NONE);           
            final List ports = collector.getPorts();                                            
            
            cellToPortListMap.put(cellName, ports);        
          
            return ports;
        }
    }

    public APPortDef getPort( final HierName cellName, 
                              final HierName portName, 
                              final CellInterfacePortCollector collector) {
        final HierName netName = getCanonicalNodeName(portName);
        HashMap portMap;

        if( cellToPortMapMap.containsKey(cellName) ) {
            portMap = (HashMap) cellToPortMapMap.get(cellName);
        }
        else {
            portMap = new HashMap();          
            cellToPortMapMap.put(cellName, portMap);
        }

        if( portMap.containsKey(netName) ) {
            return (APPortDef) portMap.get(netName);
        }
        else {            
            final List ports = getPorts(cellName, collector);            
            for(Iterator i=ports.iterator(); i.hasNext(); ) {
                APPortDef portDef = (APPortDef) i.next();
                portMap.put( portDef.getNetName(), portDef);
                //System.out.println( portDef.getPortName().toString() );
            }        
  
            //System.out.println( ( cellName == null ? "" : cellName.toString() ) + " " + portName.toString() + " " ); 
            return (APPortDef) portMap.get(netName);
           
        }
    }

    public static Iterator getSortedCellInterfaces( final CellInterface cell, final PortDefinition port) {
        final List cells = new LinkedList();
        Iterator portPairs = cell.getPortSubcellPairs();
        final String regEx = port.getName() + "\\[[\\d\\s,]*\\]";
        while( portPairs.hasNext() ) {                      
            final Pair pair = (Pair) portPairs.next();
            final HierName name = (HierName) pair.getFirst();                                                
            if( name.toString().equals( port.getName() ) || name.toString().matches( regEx ) )
                cells.add( pair );
        }

        //now sort the cells
        Collections.sort(cells, new Comparator() {
                public int compare(Object a, Object b) {
                    HierName nameA = (HierName) ((Pair) a).getFirst();        
                    HierName nameB = (HierName) ((Pair) b).getFirst();        
                    return nameA.compareTo(nameB);
                    
                }
            });
        return cells.iterator();
    } 
 
    /*
    //maps full port node names into the top level channel that contains them
    public APPortDef getPortTopLevelPort(HierName cellName, HierName portName, CellInterfacePortCollector collector) {
        if( portName.getParent() == null || portName.getParent().equals(cellName) ) {                               
            System.out.println( portName.toString() );
            return getPort(cellName, portName, collector);
        }
        else
            return getTopLevelPort(cellName, portName.getParent(), collector );                                
    }
    */

    //looks up the tree until it finds a bunched port, and returns the port
    public APPortDef getPortBunchChannel( final HierName cellName,
                                          final HierName portName, 
                                          final CellInterfacePortCollector collector) {
        //if the parent 'channel' is a cell or the parent cell(null) then return null
        if( portName.getParent() == null || portName.getParent().equals(cellName) )
            return null;
        //otherwise get the channel def...it should be there
        APChannelDef chan = (APChannelDef) getPort(cellName, portName.getParent(), collector );
        //if it's bunched, return it
        if(chan.isBunched() )
            return chan;
        //otherwise keep going up
        else 
            return getPortBunchChannel(cellName, chan.getPortName(), collector );                        
    }


    public static float getLayerFloatDirective(CellInterface cell, Pair layer, String directive) {
        Map map = DirectiveUtils.getTopLevelDirective(cell, 
                                                      directive, 
                                                      DirectiveConstants.LAYER_TYPE);
        Float value = (Float)map.get( layer );
        return ( value == null ? (float)-1.0 : value.floatValue() );
    }


   /** walks the CellInterface, differs from CadenceInfo in that Ports are traversed in order of port list
    *   
    * prefix    - prefix to prepend to all subcell names, typically HierName.makeHierName("") at top level
    * direction - direction that all children will inherit, typicall PortDefinition.NONE at top level     
    **/
    public static void walk( final CellInterfaceActionInterface action,
                             final HierarchicalDirectiveInterface hdi,
                             final CellInterface cell, 
                             final Pair layer,
                             final CadenceInfo cellInfo,                            
                             final HierName prefix, 
                             final int direction) {        

        //Make sure output is in same order as portDefinitions
        final Iterator portDefs  = cell.getPortDefinitions();

        while( portDefs.hasNext() ) {
            final PortDefinition port = (PortDefinition) portDefs.next();
            final Iterator portPairs = getSortedCellInterfaces(cell, port);
       
            //for each port with this name ( multiple for arrays )
            while( portPairs.hasNext() ) {           
                final Pair pair = (Pair) portPairs.next();
                final HierName name = (HierName) pair.getFirst();        
                final HierName fullName = HierName.append( prefix, name );
                final CellInterface subCell = (CellInterface) pair.getSecond();                     
                
                int newDirection;
                if(direction == PortDefinition.NONE)
                    newDirection = port.getDirection();
                else
                    newDirection = direction;                        
                         
                //nodes
                if( subCell.isNode() ) {
                    HierName netName = getCanonicalNodeName(cellInfo, fullName);
                    final boolean used =
                        ((Boolean) cellInfo.getPortNodes().getValue(fullName)).booleanValue();
                    if(used)
                        action.doPort(hdi, netName, fullName, newDirection);
                }
        
                //channels, structs               
                else {                    
                    action.doChannel(hdi, subCell, cellInfo, fullName, newDirection);
                }                                     
            }
        }
    }

    public abstract void process( final Writer writer ) 
        throws IOException, UnknownDirectiveException, CellProcessorException;
}
