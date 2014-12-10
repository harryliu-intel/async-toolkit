/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.floorplanning;


import java.util.Collections;
import java.util.Set;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedList;
import java.util.Map;
import java.util.Iterator;

import java.util.Date;
import java.util.Calendar;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.io.OutputStreamWriter;

import java.io.FileInputStream;
import java.io.FileOutputStream;

import java.io.InputStreamReader;

import java.io.FileNotFoundException;
import java.io.IOException;

import java.text.MessageFormat;

import antlr.RecognitionException;
import antlr.TokenStreamException;

import com.avlsi.util.debug.Debug;

import com.avlsi.util.container.StringContainerIterator;

import com.avlsi.io.FileSearchPath;
import com.avlsi.io.SearchPath;

import com.avlsi.file.common.HierName;

import com.avlsi.file.cdl.parser.ReadCDLIntoFactory;

import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.NullEnvironment;

import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.AbstractNode;
import com.avlsi.netlist.AbstractNetlistIterator;
import com.avlsi.netlist.AbstractNodeIterator;

import com.avlsi.netlist.impl.SimpleAbstractNetlistIterator;
import com.avlsi.netlist.impl.simple.SimpleNetlistFactory;

import com.avlsi.netlist.util.ChildrenFirstNetlistIterator;
import com.avlsi.netlist.util.SubcellIterator;
import com.avlsi.netlist.util.FETIterator;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;

import com.avlsi.floorplanning.PCellTypesInfo;
import com.avlsi.floorplanning.PCellTypesInfoImpl;
import com.avlsi.floorplanning.TransistorTypeInfo;

public class GenerateFloorPlanningData {

    static final String openSchematicFormatStr =
        "( defvar\n" +
        "  {0}\n" +
        "  (let (\n" +
        "        ( SchematicDDObj ( ddGetObj\n" +
        "                           {1}\n" +
        "                           {2}\n" +
        "                           {3}\n" +
        "                           \"pc.db\"\n" +
        "                           nil\n" +
        "                           \"r\"" +
        "                           )\n" +
        "                         )\n" +
        "        )\n" +
        "    (when ( or\n" +
        "            ( null SchematicDDObj )\n" +
        "            ( ddIsObjWritable SchematicDDObj )\n" +
        "            )\n" +
        "      ( dbOpenCellViewByType\n" +
        "        {1}\n" +
        "        {2}\n" +
        "        {3}\n" +
        "        \"schematic\"\n" +
        "        \"w\"\n" +
        "        )\n" +
        "      )\n" +
        "    )\n" +
        "  )\n" +
        "(if {0}\n" +
        "    ( printf \"%s %s %s\\n\" {1} {2} {3} )\n" +
        "  ( printf \"Skipping %s %s %s\\n\" {1} {2} {3} )\n" +
        "  )\n";

    static final String saveAndCloseSchematicFormatStr =
        "(when {0}\n" +
        "  ( DistributeInstances {0} 0 0 5 )\n" +
        "  ( dbReplaceProp\n" +
        "    {0}\n" +
        "    \"lastSchematicExtraction\"\n" +
        "    \"time\"\n" +
        "    \"{1,time,MMM dd HH:mm:ss yyyy}\"\n" +
        "    )\n" +
        "  ( dbSave {0} )\n" +
        "  ( dbPurge {0} )\n" +
        "  )\n";

    /*
      TransistorTable
      ModelName
      LibName
      CellName
      ViewName
     */
    static final String openPCellFormatStr =
        "( setarray\n" +
        "  {0}\n" +
        "  {1}\n" +
        "  ( dbOpenCellViewByType\n" +
        "    {2}\n" +
        "    {3}\n" +
        "    {4}\n" +
        "    nil\n" +
        "    \"r\"\n" +
        "    )\n" +
        "  )\n";

    static final String createTerminalFormatStr =
        "( when {0}\n" +
        "  ( dbCreateTerm\n" +
        "    ( dbMakeNet {0} {1} )" +
        "    {1}" +
        "    \"inputOutput\"\n" +
        "    )\n" +
        "  )\n";
    /*
      0  CurrCellView
      1  TransitorName
      2  W
      3  L
      4  GateNetName
      5  SourceNetName
      6  DrainNetName
      7  BulkNetName
      8  TransistorView      
      9  WParamName
      10 WParamType
      11 LParamName
      12 LParamType
      13 GateTermName
      14 SourceTermName
      15 DrainTermName
      16 BulkTermName
     */
    static final String createTransistorFormatStr =
        "(when {0}\n" +
        "  (let (\n" +
        "        ( TransistorInstance ( dbCreateParamInst\n" +
        "                               {0}\n" +
        "                               {8}\n" +
        "                               {1}\n" +
        "                               ( list 0 0 )\n" +
        "                               \"R0\"\n" +
        "                               1\n" +
        "                               ( list\n" +
        "                                 ( list\n" +
        "                                    {9}\n" +
        "                                    {10}\n" +
        "                                    {2}\n" +
        "                                  )\n" +
        "                                 ( list\n" +
        "                                    {11}\n" +
        "                                    {12}\n" +
        "                                    {3}\n" +
        "                                  )\n" +
        "                                )\n" +
        "                               )\n" +
        "                             )\n" +
        "        )\n" +
        "    (when TransistorInstance\n" +
        "      ( dbCreateConnByName\n" +
        "        ( dbMakeNet {0} {4} )\n" +
        "        TransistorInstance\n" +
        "        {13}\n" +
        "        )\n" +
        "      ( dbCreateConnByName\n" +
        "        ( dbMakeNet {0} {5} )\n" +
        "        TransistorInstance\n" +
        "        {14}\n" +
        "        )\n" +
        "      ( dbCreateConnByName\n" +
        "        ( dbMakeNet {0} {6} )\n" +
        "        TransistorInstance\n" +
        "        {15}\n" +
        "        )\n" +
        "      ( dbCreateConnByName\n" +
        "        ( dbMakeNet {0} {7} )\n" +
        "        TransistorInstance\n" +
        "        {16}\n" +
        "        )\n" +
        "      )\n" +
        "    )\n" +
        "  )\n";

    static final String createInstFormatStr =
        "(when {0}\n" +
        "  (let (\n" +
        "        ( InstanceMaster ( dbOpenCellViewByType\n" +
        "                           {2}\n" +
        "                           {3}\n" +
        "                           {4}\n" +
        "                           \"schematic\"\n" +
        "                           \"r\"\n" +
        "                           )\n" +
        "                         )\n" +
        "        )\n" +
        "    (when InstanceMaster\n" +
        "      ( defvar\n" +
        "        {1}\n" +
        "        ( dbCreateInst {0} InstanceMaster {5} ( list 0 0 ) \"R0\" )\n" +
        "        )\n" +
        "      )\n" +
        "    )\n" +
        "  )\n";

    /*
      CurrCellView
      Instance
      ConnectedNet
      TerminalName
     */
    static final String createConnectionFormatStr =
        "(when {0}\n" +
        "  (let (\n" +
        "        ( ConnectedNet ( dbMakeNet {0} {2} ) )\n" +
        "        )\n" +
        "    ( dbCreateConnByName ConnectedNet {1} {3} )\n" +
        "    )\n" +
        "  )\n";

    

    private interface NetlistNameParser {
        String getCellName( final HierName netlistName );
        String getLibName( final HierName netlistName );
    }

    public static class FloorPlanningException extends Exception {
        public FloorPlanningException( final String error ) {
            super( error );
        }
    }

    private static String quoteString( final String src ) {
        return "\"" + src + "\"";
    }
    
    private static Set getGateNetlists( final PCellTypesInfo pcellInfos,
                                        final SimpleNetlistFactory f )
    {

        final Iterator gatePCellTypeInfos = pcellInfos.getNonTransistorPCellTypeInfos();
        
        final String[] emptyStrArray = new String[0];

        final Environment emptyEnv = NullEnvironment.getInstance();

        while ( gatePCellTypeInfos.hasNext() ) {
            PCellTypeInfo currInfo = ( PCellTypeInfo ) gatePCellTypeInfos.next();
            f.beginSubcircuit( currInfo.getCellName(), 
                               emptyStrArray, 
                               emptyStrArray,
                               Collections.EMPTY_MAP, 
                               emptyEnv );
            f.endSubcircuit( currInfo.getCellName(),
                            emptyEnv );   
        }

        final Set ret = new HashSet( );

        final AbstractNetlistIterator netlistIter = f.getAllNetlists();
        
        while ( netlistIter.hasNext() ) {
            final AbstractNetlist netlist = netlistIter.next();
            ret.add( netlist );
        }

        return ret;
            
    }
    

    private static List getNetlistsInOrder( final File cdlFile,
                                            final SimpleNetlistFactory f,
                                            final Set gateNetlists ) 
    throws FloorPlanningException, IOException, FileNotFoundException,
           RecognitionException, TokenStreamException 
    {
        
        final InputStream cdlInputStream = 
            new FileInputStream( cdlFile );
        final Reader cdlReader = 
            new InputStreamReader( cdlInputStream, "UTF-8" );
        
        ReadCDLIntoFactory.readCDL( cdlReader, f );

        final AbstractNetlistIterator netlistIter = f.getAllNetlists();
        final Set cellsWeHaveAlreadySeen = new HashSet();
        
        final List ret = new LinkedList();

        while ( netlistIter.hasNext() ) {
            final AbstractNetlist curr = netlistIter.next();
            final String currName = curr.getName().toString();
            if ( ! ( cellsWeHaveAlreadySeen.contains( currName ) ) ) {
                final ChildrenFirstNetlistIterator innerNetlistIter =
                    new ChildrenFirstNetlistIterator( curr );
                while ( innerNetlistIter.hasNext() ) {
                    final AbstractNetlist innerCurr = innerNetlistIter.next();
                    final String innerCurrName = innerCurr.getName().toString();
                    if ( ! ( cellsWeHaveAlreadySeen.contains( innerCurrName ) ) ) {
                        cellsWeHaveAlreadySeen.add( innerCurrName );
                        if ( ! ( gateNetlists.contains( innerCurr ) ) ) {
                            ret.add( innerCurr );
                        }
                    }
                }
            }
        }
        
        return ret;
    }

    private static boolean isEmptyCell( final AbstractNetlist netlist ) {
        return ! netlist.getDevices().hasNext();
    }

    private static boolean isMidLevelCellNetlist( final AbstractNetlist netlist,
                                                  final Set gateNetlists ) {
        boolean isMidLevel = false;
        final SubcellIterator subCellIter = 
            new SubcellIterator( netlist.getDevices() );

        if ( subCellIter.hasNext() ) {
            final SubcellIterator.SubcellInstance instance =
                subCellIter.next();
            isMidLevel = 
                ! ( gateNetlists.contains( instance.getInstantiatedNetlist() ) ) ;
        }

        return isMidLevel;
    }

    private static boolean isLeafCellNetlist( final AbstractNetlist netlist,
                                              final Set gateNetlists ) {
       
        boolean isLeaf = false;
        final SubcellIterator subCellIter = 
            new SubcellIterator( netlist.getDevices() );

        if ( subCellIter.hasNext() ) {
            final SubcellIterator.SubcellInstance instance =
                subCellIter.next();
            isLeaf = gateNetlists.contains( instance.getInstantiatedNetlist() );
        }
        else {
            isLeaf = netlist.getDevices().hasNext();
        }

        return isLeaf;

    }

    private static void emitTerminals( final Writer w, 
                                       final AbstractNodeIterator terminalNodes,
                                       final String cellViewExpression,
                                       final MessageFormat formatter ) 
    throws IOException {
        final StringBuffer accumulator = new StringBuffer();
        while ( terminalNodes.hasNext() ) {
            final AbstractNode currNode = terminalNodes.next();
            
            final Object[] createTerminalArgs = {
                cellViewExpression,
                quoteString( currNode.getCanonicalName().toString() )
            };

            formatter.format( createTerminalArgs,
                              accumulator,
                              null );
            w.write( accumulator.toString() );
            accumulator.delete( 0, accumulator.length() );
        }
    }

    private static 
        void emitConnections( final Writer w,
                              final AbstractNodeIterator connectedNodes,
                              final AbstractNodeIterator masterNodes,
                              final String cellViewExpression,
                              final String currInstanceExpression,
                              final MessageFormat formatter) 
        throws IOException 
    
    {
    
        final StringBuffer accumulator = new StringBuffer();
        while ( masterNodes.hasNext() ) {
            final AbstractNode currMasterNode = masterNodes.next();
            final AbstractNode currConnectedNode = connectedNodes.next();
            
            final Object[] createConnectionArgs = {
                cellViewExpression,
                currInstanceExpression,
                quoteString( currConnectedNode.getCanonicalName().toString() ),
                quoteString( currMasterNode.getCanonicalName().toString() )
            };

            formatter.format( createConnectionArgs,
                              accumulator,
                              null );
            
            w.write( accumulator.toString() );
            accumulator.delete( 0, accumulator.length() );
        }
    }

    private static
        void openTransistors( final Writer writer,
                              final String transistorTableName,
                              final PCellTypesInfo pCellsInfo ) 
        throws IOException
    {
        final StringBuffer accum = new StringBuffer();
        final StringContainerIterator modelIter =
            pCellsInfo.getTransistorModels();

        if ( modelIter.hasNext() ) {

            final MessageFormat openPCellFormatter =
                new MessageFormat( openPCellFormatStr );

            writer.write( "( defvar " +
                          transistorTableName +
                          " ( makeTable \"foo\" nil ) )\n" );
            while( modelIter.hasNext() ) {
                final String currModelName = modelIter.next();
                final TransistorTypeInfo currTransistorType =
                    pCellsInfo.getTransistorInfo( currModelName );

                final String libName = currTransistorType.getLibName();
                final String cellName = currTransistorType.getCellName();
                final String viewName = 
                    currTransistorType.getSchematicViewName();

                if ( ( libName != null ) &&
                     ( cellName != null ) &&
                     ( viewName != null ) ) {

                    final Object[] openTransistorArgs = {
                        transistorTableName,
                        quoteString( currModelName ),
                        quoteString( libName ),
                        quoteString( cellName),
                        quoteString( viewName )
                    };
                    openPCellFormatter.format( openTransistorArgs, 
                                               accum,
                                               null );
                }
            }
            writer.write( accum.toString() );
        }
        
    }

    private static
        void openGates( final Writer writer,
                        final String gateTableName,
                        final PCellTypesInfo pCellsInfo,
                        final Set gateNetlists )
        throws IOException 
    {
        
        final StringBuffer accum = new StringBuffer();
        final Iterator gateIter = gateNetlists.iterator();

        if ( gateIter.hasNext() ) {

            final MessageFormat openPCellFormatter =
                new MessageFormat( openPCellFormatStr );

            writer.write( "( defvar " +
                          gateTableName +
                          " ( makeTable \"bar\" nil ) )\n" );
            while( gateIter.hasNext() ) {
                final AbstractNetlist currGate = ( AbstractNetlist ) gateIter.next();
                final String currGateName = currGate.getName().toString();
                final PCellTypeInfo currPCellType =
                    pCellsInfo.getPCellTypeInfo( currGateName );
                if ( currPCellType != null ) {
                    final String libName = currPCellType.getLibName();
                    final String cellName = currPCellType.getCellName();
                    final String viewName = 
                        currPCellType.getSchematicViewName();
                    if ( ( libName != null ) &&
                         ( cellName != null ) &&
                         ( viewName != null ) ) {
                        
                        final Object[] openGateArgs = {
                            gateTableName,
                            quoteString( currGateName ),
                            quoteString( libName ),
                            quoteString( cellName),
                            quoteString( viewName )
                        };
                        openPCellFormatter.format( openGateArgs, 
                                                   accum,
                                                   null );
                    }
                }
                else {
                    System.out.println( "Unable to find pcell info for \"" + currGateName + "\"." );
                }
            }
            writer.write( accum.toString() );
        }
    }

    private static
        void emitTransistors( final Writer writer,
                              final AbstractNetlist netlist, 
                              final String currCellViewExpression,
                              final String transistorTableName,
                              final PCellTypesInfo pCellsInfo,
                              final MessageFormat createTransistorFormatter )
        throws IOException 
    {
        final StringBuffer accumulator = new StringBuffer();

        final FETIterator fetIter =
            new FETIterator( netlist.getDevices() );

        while ( fetIter.hasNext() ) {
            final FETIterator.FET currFET = fetIter.next();

            final TransistorTypeInfo currTransistorInfo =
                pCellsInfo.getTransistorInfo( currFET.getType() );

            if ( currTransistorInfo != null ) {

                final String wParamType =
                    currTransistorInfo.getParamType( "W" );
                final String lParamType =
                    currTransistorInfo.getParamType( "L" );

                final String gateTerminalName =
                    currTransistorInfo.getGateTerminalName();
                
                final String sourceTerminalName =
                    currTransistorInfo.getSourceTerminalName();

                final String drainTerminalName =
                    currTransistorInfo.getDrainTerminalName();

                final String bulkTerminalName =
                    currTransistorInfo.getBulkTerminalName();

                if ( ( wParamType != null ) &&
                     ( lParamType !=null )   &&
                     ( gateTerminalName != null ) &&
                     ( sourceTerminalName != null ) &&
                     ( drainTerminalName != null ) &&
                     ( bulkTerminalName != null ) ) {

                    final String transistorViewExpression =
                        "( arrayref " +
                        transistorTableName +
                        " " +
                        quoteString( currFET.getType() ) +
                        " )";

                    final double w = currFET.getWidth();
                    final String wStr = Double.toString( w );
                    
                    final double l = currFET.getLength();
                    final String lStr = Double.toString( l );

                    final AbstractNode drainNode = currFET.getDrain();
                    final String drainNodeName = 
                        drainNode.getCanonicalName().toString();

                    final AbstractNode gateNode = currFET.getGate();
                    final String gateNodeName = 
                        gateNode.getCanonicalName().toString();

                    final AbstractNode sourceNode = currFET.getSource();
                    final String sourceNodeName = 
                        sourceNode.getCanonicalName().toString();
                    
                    final AbstractNode bulkNode = currFET.getBulk();
                    final String bulkNodeName = 
                        bulkNode.getCanonicalName().toString();

                    final Object[] createTransistorArgs =
                    {
                        currCellViewExpression,
                        quoteString( currFET.getName().toString() ),
                        wStr,
                        lStr,
                        quoteString( gateNodeName ),
                        quoteString( sourceNodeName ),
                        quoteString( drainNodeName ),
                        quoteString( bulkNodeName ),
                        transistorViewExpression,
                        "\"w\"",
                        quoteString( wParamType ),
                        "\"l\"",
                        quoteString( lParamType ),
                        quoteString( gateTerminalName ),
                        quoteString( sourceTerminalName ),
                        quoteString( drainTerminalName ),
                        quoteString( bulkTerminalName )
                    };

                    createTransistorFormatter.format( createTransistorArgs,
                                                      accumulator,
                                                      null );
                        
                }
            }
        }
        writer.write( accumulator.toString() );
    }

    private static
        void emitGates( final Writer writer,
                        final AbstractNetlist netlist,
                        final String currCellViewExpression,
                        final String gateTableName,
                        final PCellTypesInfo pCellsInfo,
                        final MessageFormat createConnectionFormatter  ) 
    throws FloorPlanningException, IOException {
       final String currInstanceVarName = "CurrInstance";
       final StringBuffer accumulator = new StringBuffer();
       
       final SubcellIterator subCellsIter =
           new SubcellIterator( netlist.getDevices() );
       
       while( subCellsIter.hasNext() ) {
           final SubcellIterator.SubcellInstance currInstance =
               subCellsIter.next();
           
           final AbstractNetlist instanceMaster = 
               currInstance.getInstantiatedNetlist();

           final String instanceMasterName = instanceMaster.getName().toString();
           
           final String instanceName = currInstance.getName().toString();

           final PCellTypeInfo currGateInfo =
               pCellsInfo.getPCellTypeInfo( instanceMasterName );

           
           if ( currGateInfo != null ) {
               final Map instanceParamMap = currInstance.getParams();
               
               final Set instanceParams = instanceParamMap.entrySet();

               final Iterator instanceParamsIter = instanceParams.iterator();

               writer.write( "( when " +
                             currCellViewExpression +
                             "\n" +
                             "  (let (\n" +
                             "        ( ParamValueList\n" +
                             "          ( list\n" );
               
               while ( instanceParamsIter.hasNext() ) {
                   final Map.Entry currParam = ( Map.Entry ) instanceParamsIter.next();
                   
                   final String currParamName = ( String ) currParam.getKey();
                   
                   final String currParamType = currGateInfo.getParamType( currParamName );
                   
                   if ( currParamType != null ) {
                       final Double currParamValue = ( Double ) currParam.getValue();
                       writer.write( "            ( list \"" +
                                     currParamName +
                                     "\" \"" +
                                     currParamType +
                                     "\" " +
                                     currParamValue.toString() +
                                     " )\n" );
                   }
               }
               writer.write( "            )\n" );
               writer.write( "          )\n" );
               writer.write( "        )\n" );
               writer.write( "    ( defvar\n" +
                             "      " +
                             currInstanceVarName +
                             "\n" +
                             "      ( dbCreateParamInst\n" +
                             "        " +
                             currCellViewExpression +
                             "\n" +
                             "        ( arrayref " +
                             gateTableName +
                             " " +
                             quoteString( instanceMasterName ) +
                             " )\n" +
                             "        " +
                             quoteString( instanceName ) +
                             "\n" +
                             "        ( list 0 0 )\n" +
                             "        \"R0\"\n" +
                             "        1\n" +
                             "        ParamValueList\n" +
                             "        )\n" +
                             "      )\n" +
                             "    )\n" +
                             "  )\n" );
               
               final AbstractNodeIterator connectedNodeIter =
                   currInstance.getConnectedNodes();
               
               final AbstractNodeIterator instanceInputNodeIter =
                   instanceMaster.getInputNodes();
               
               final AbstractNodeIterator instanceOutputNodeIter =
                   instanceMaster.getOutputNodes();
               
               emitConnections( writer,
                                connectedNodeIter,
                                instanceInputNodeIter,
                                currCellViewExpression,
                                currInstanceVarName,
                                createConnectionFormatter );
               
               emitConnections( writer,
                                connectedNodeIter,
                                instanceOutputNodeIter,
                                currCellViewExpression,
                                currInstanceVarName,
                                createConnectionFormatter );
           }
           else {
               throw new FloorPlanningException( "Unable to get pcell info for \"" +
                                                 instanceMasterName +
                                                 "\".");
           }

       }
    }
                              

    private static
        void emitSubCells( final Writer writer, 
                           final AbstractNetlist netlist,
                           final NetlistNameParser nParser,
                           final String currCellViewExpression,
                           final String viewNameExpression,
                           final MessageFormat createInstFormatter,
                           final MessageFormat createConnectionFormatter ) 
        throws IOException 
    {
        
        final String currInstanceVarName = "CurrInstance";
        final StringBuffer accumulator = new StringBuffer();

        final SubcellIterator subCellsIter =
            new SubcellIterator( netlist.getDevices() );
        
        while ( subCellsIter.hasNext() ) {
            final SubcellIterator.SubcellInstance currInstance =
                subCellsIter.next();

            final AbstractNetlist instanceMaster =
                currInstance.getInstantiatedNetlist();

            final Object[] createInstArgs = {
                currCellViewExpression,
                currInstanceVarName,
                quoteString( nParser.getLibName( instanceMaster.getName() ) ),
                quoteString( nParser.getCellName( instanceMaster.getName() ) ),
                viewNameExpression,
                quoteString( currInstance.getName().toString() ) 
            };

            createInstFormatter.format( createInstArgs,
                                        accumulator,
                                        null );

            writer.write( accumulator.toString() );
            accumulator.delete( 0, accumulator.length() );

            final AbstractNodeIterator connectedNodeIter =
                currInstance.getConnectedNodes();

            final AbstractNodeIterator instanceInputNodeIter =
                instanceMaster.getInputNodes();

            final AbstractNodeIterator instanceOutputNodeIter =
                instanceMaster.getOutputNodes();

            emitConnections( writer,
                             connectedNodeIter,
                             instanceInputNodeIter,
                             currCellViewExpression,
                             currInstanceVarName,
                             createConnectionFormatter );

            emitConnections( writer,
                             connectedNodeIter,
                             instanceOutputNodeIter,
                             currCellViewExpression,
                             currInstanceVarName,
                             createConnectionFormatter );

        }
    }

    private static 
        void openCellForWriting( final AbstractNetlist netlist,
                            final Writer writer,
                            final NetlistNameParser nParser,
                            final String viewNameExpression,
                            final String cellViewVarName,
                            final MessageFormat openSchematicFormatter ) 
        throws IOException
    {
        final Object[] openSchematicArgs = {
            cellViewVarName,
            quoteString( nParser.getLibName( netlist.getName() ) ),
            quoteString( nParser.getCellName( netlist.getName() ) ),
            viewNameExpression
        };

        final StringBuffer accum = new StringBuffer();

        
        openSchematicFormatter.format( openSchematicArgs,
                                       accum, 
                                       null );
        writer.write( accum.toString() );

    }

    private static void saveAndPurgeCell( final AbstractNetlist netlist,
                                          final Writer writer,
                                          final String currCellViewVarName,
                                          final MessageFormat formatter ) 
        throws IOException
    {
        final Calendar myCalendar = Calendar.getInstance();
        
        myCalendar.add( Calendar.YEAR, 20 );
        
        final Date myDate = myCalendar.getTime();

        final Object[] saveAndPurgeArgs = {
            currCellViewVarName,
            myDate
        };

        final StringBuffer accumulator = new StringBuffer();
        
        

        formatter.format( saveAndPurgeArgs, accumulator, null ); 
                         
        
        writer.write( accumulator.toString() );
    }


    private static
        void emitLeafCell( final AbstractNetlist netlist,
                           final Writer writer,
                           final String currCellViewExpression,
                           final String transistorTableName,
                           final String gateTableName,
                           final PCellTypesInfo pCellsInfo,
                           final MessageFormat createTerminalFormatter,
                           final MessageFormat createTransistorFormatter,
                           final MessageFormat createConnectionFormatter )
        throws FloorPlanningException, IOException
    {
        
        final AbstractNodeIterator inputNodesIter = netlist.getInputNodes();
        final AbstractNodeIterator outputNodesIter = netlist.getOutputNodes();
        
        emitTerminals( writer,
                       inputNodesIter,
                       currCellViewExpression,
                       createTerminalFormatter );

        emitTerminals( writer,
                       outputNodesIter,
                       currCellViewExpression,
                       createTerminalFormatter );
        
        emitTransistors( writer,
                         netlist,
                         currCellViewExpression,
                         transistorTableName,
                         pCellsInfo,
                         createTransistorFormatter );
        emitGates( writer,
                   netlist,
                   currCellViewExpression,
                   gateTableName,
                   pCellsInfo,
                   createConnectionFormatter );
    }
                           

    private static 
        void emitMidLevelCell( final AbstractNetlist netlist,
                               final Writer writer,
                               final String currCellViewExpression,
                               final NetlistNameParser nParser,
                               final String viewNameExpression,
                               final MessageFormat createTerminalFormatter,
                               final MessageFormat createTransistorFormatter,
                               final MessageFormat createInstFormatter,
                               final MessageFormat createConnectionFormatter ) 
        throws IOException
    {

        final AbstractNodeIterator inputNodesIter = netlist.getInputNodes();
        final AbstractNodeIterator outputNodesIter = netlist.getOutputNodes();

        emitTerminals( writer,
                       inputNodesIter,
                       currCellViewExpression,
                       createTerminalFormatter );

        emitTerminals( writer,
                       outputNodesIter,
                       currCellViewExpression,
                       createTerminalFormatter );

        emitSubCells( writer,
                      netlist,
                      nParser,
                      currCellViewExpression,
                      viewNameExpression,
                      createInstFormatter,
                      createConnectionFormatter );
    }

    public static Set getLibNames( final List cells,
                                   final NetlistNameParser nParser ) {
        final Set ret = new HashSet();
        final SimpleAbstractNetlistIterator cellIter =
            new SimpleAbstractNetlistIterator( cells.iterator() );

        while ( cellIter.hasNext() ) {
            final AbstractNetlist currCell = cellIter.next();
            ret.add( nParser.getLibName( currCell.getName() ) );
        }
        
        return ret;
    }

    public static Writer openOutputFile( final String fileName ) 
        throws FloorPlanningException, IOException 
    {
        final Writer ret;
        try {
            final OutputStream outputStream =
                new FileOutputStream( fileName );
            
            ret  = new OutputStreamWriter( outputStream );
        }
        catch( FileNotFoundException e ) {
            final String message =
                "Unable to create file \"" +
                fileName + "\"";
            throw new FloorPlanningException(message);
        }
        return ret;
    }

    public static void emitCells( final List cells,
                                  final Writer skillOutputWriter,
                                  final Writer cellListWriter,
                                  final NetlistNameParser nParser,
                                  final Set gateNetlists,
                                  final PCellTypesInfo pCellsInfo )
        throws FloorPlanningException, IOException
    {

        final MessageFormat openSchematicFormatter =
            new MessageFormat( openSchematicFormatStr );

        final MessageFormat saveAndCloseSchematicFormatter =
            new MessageFormat( saveAndCloseSchematicFormatStr );

        final MessageFormat createTerminalFormatter =
            new MessageFormat( createTerminalFormatStr );

        final MessageFormat createTransistorFormatter =
            new MessageFormat( createTransistorFormatStr );

        final MessageFormat createInstFormatter =
            new MessageFormat( createInstFormatStr );

        final MessageFormat createConnectionFormatter =
            new MessageFormat( createConnectionFormatStr );

        final SimpleAbstractNetlistIterator cellIter =
            new SimpleAbstractNetlistIterator( cells.iterator() );

        final String viewNameExpression = "\"netlist\"";
        final String currCellViewVarName = "CurrCellView" ;

        final String transistorTableName = "TransistorTable";
        final String gateTableName = "GateTable";

        if ( cellIter.hasNext() ) {
            openTransistors( skillOutputWriter,
                             transistorTableName,
                             pCellsInfo );
            openGates( skillOutputWriter,
                       gateTableName,
                       pCellsInfo,
                       gateNetlists);
        }

        while ( cellIter.hasNext() ) {
            final AbstractNetlist currNetlist = cellIter.next();

            if ( ! isEmptyCell( currNetlist ) ) {
                final String currCellName = 
                    currNetlist.getName().toString() + "\n";
                cellListWriter.write( currCellName );

                openCellForWriting( currNetlist,
                                    skillOutputWriter,
                                    nParser,
                                    viewNameExpression,
                                    currCellViewVarName,
                                    openSchematicFormatter );

                if ( isMidLevelCellNetlist( currNetlist,
                                            gateNetlists ) ) {    
                    emitMidLevelCell( currNetlist,
                                      skillOutputWriter,
                                      currCellViewVarName,
                                      nParser,
                                      viewNameExpression,
                                      createTerminalFormatter,
                                      createTransistorFormatter,
                                      createInstFormatter,
                                      createConnectionFormatter );
                }
                else {
                    emitLeafCell( currNetlist,
                                  skillOutputWriter,
                                  currCellViewVarName,
                                  transistorTableName,
                                  gateTableName,
                                  pCellsInfo,
                                  createTerminalFormatter,
                                  createTransistorFormatter,
                                  createConnectionFormatter );
                }
                saveAndPurgeCell( currNetlist,
                                  skillOutputWriter,
                                  currCellViewVarName,
                                  saveAndCloseSchematicFormatter );
                skillOutputWriter.flush();
            }

                
        }
    }

    public static void emitLibs( final List cells,
                                 final NetlistNameParser nParser,
                                 final Writer libListWriter ) 
    throws IOException 
    {
        final Set libNames = getLibNames( cells, nParser );

        final Iterator libNamesIter = libNames.iterator();

        while ( libNamesIter.hasNext() ) {
            final String currLibName = ( String ) libNamesIter.next();
            libListWriter.write( currLibName + "\n" );
        }
    }
                                 
            

    public static void emit( final List cells,
                             final String skillFileName,
                             final String outputCellListFileName,
                             final String libListFileName,
                             final Set gateNetlists,
                             final PCellTypesInfo pCellsInfo )
    throws FloorPlanningException, IOException  
    {
        final Writer skillOutputWriter =
            openOutputFile( skillFileName );
        
        final Writer cellListWriter =
            openOutputFile( outputCellListFileName );
        
        final Writer libListWriter =
            openOutputFile( libListFileName );
        
        
        final NetlistNameParser myNameParser =
            new NetlistNameParser() {
                    public String getCellName( final HierName netlistName ) {
                        return netlistName.toString();
                    }
                    public String getLibName( final HierName netlistName ) {
                        return netlistName.getParent().getParent().toString();
                    }
                };

        emitLibs( cells, myNameParser, libListWriter );

        emitCells( cells, 
                   skillOutputWriter,
                   cellListWriter,
                   myNameParser,
                   gateNetlists,
                   pCellsInfo );
        
        skillOutputWriter.flush();
        skillOutputWriter.close();
        cellListWriter.flush();
        cellListWriter.close();
        libListWriter.flush();
        libListWriter.close();
    }

    

    private static void usage(  ) {

        final String className = GenerateFloorPlanningData.class.getName();
        
        System.out.println( "Usage: " + 
                            System.getProperty( "java.home" ) +
                            System.getProperty( "file.separator" ) +
                            "bin" +
                            System.getProperty( "file.separator" ) +
                            "java " +
                            " -classpath " +
                            System.getProperty( "java.class.path" ) + " " +
                            className + "\n" +
                            "    --cast-path=path\n" +
                            "    --cdl-file=file\n" +
                            "    --cell=cell\n" +
                            "    --skill-output=file\n" +
                            "    --cell-list-output=file\n" +
                            "    --lib-list-output=file\n"       +
                            "    --pcell-libs-output=file\n"     +
                            "    --pcell-info-path=path\n" );
    }

    public static void main( String args[] ) throws Exception {
        
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
	    new CommandLineArgsWithConfigFiles( parsedArgs ); 

        final CommandLineArgs cachedArgs = 
	    new CachingCommandLineArgs( argsWithConfigs );

        final CommandLineArgs theArgs = cachedArgs;

        final SearchPath castPath = 
            new FileSearchPath( theArgs.getArgValue( "cast-path", "." ) );

        final String cdlFileName = theArgs.getArgValue( "cdl-file", null );

        final String outputSkillFileName = 
            theArgs.getArgValue( "skill-output", null );

        final String outputCellListFileName =
            theArgs.getArgValue( "cell-list-output", null );

        final String outputLibListFileName =
            theArgs.getArgValue( "lib-list-output", null );

        final String pcellLibListFileName =
            theArgs.getArgValue( "pcell-libs-output", null );

        final String pcellInfoPathStr =
            theArgs.getArgValue( "pcell-info-path", null );

        if (theArgs.argExists("version")) {
            System.out.println(
                com.avlsi.util.debug.VersionInfo.getVersionString(
                    GenerateFloorPlanningData.class));
        }

        if ( ( cdlFileName != null ) &&
             ( outputSkillFileName != null ) &&
             ( outputCellListFileName != null ) &&
             ( outputLibListFileName != null ) &&
             ( pcellLibListFileName != null ) &&
             ( pcellInfoPathStr != null ) ){

            final File cdlFile = new File( cdlFileName );
            
            final FileSearchPath pcellInfoPath = 
                new FileSearchPath( pcellInfoPathStr );

            final PCellTypesInfo pCellsInfo =
                new PCellTypesInfoImpl( pcellInfoPath.getSearchPath() );

            if ( cdlFile.isFile() && 
                 cdlFile.canRead() ) {
                final SimpleNetlistFactory netlistFactory =
                    new SimpleNetlistFactory();
                final Set gateNetlists = 
                    getGateNetlists( pCellsInfo , netlistFactory );
                try {
                    final List cells =
                        getNetlistsInOrder( cdlFile,
                                            netlistFactory,
                                            gateNetlists );

                    emit( cells,
                          outputSkillFileName,
                          outputCellListFileName,
                          outputLibListFileName,
                          gateNetlists,
                          pCellsInfo );
                    
                    final Writer pcellLibListWriter = 
                        openOutputFile( pcellLibListFileName );
                    
                    final Set pcellLibList = new HashSet();

                    final Iterator pcellsInfosIter = pCellsInfo.getAllPCellTypeInfos();

                    while ( pcellsInfosIter.hasNext() ) {
                        final PCellTypeInfo currInfo = 
                            ( PCellTypeInfo ) pcellsInfosIter.next();
                        
                        pcellLibList.add( currInfo.getLibName() );
                    }
                    
                    final Iterator pcellLibNamesIter = pcellLibList.iterator();

                    while ( pcellLibNamesIter.hasNext() ) {
                        final String currLibName = ( String ) pcellLibNamesIter.next();
                        pcellLibListWriter.write( currLibName + "\n" );
                    }
                    pcellLibListWriter.close();
                    
                }
                catch ( FloorPlanningException e ) {
                    System.out.println( e.getMessage() );
                }
                

            }
        }
        else {
            if ( cdlFileName == null ) {
                System.out.println( "The name of the cdl file to read" +
                                    " must be specified." );
            }
            if ( outputSkillFileName == null ) {
                System.out.println( "The name of the skill file to generate" +
                                    " must be specified." );
            }
            if ( outputCellListFileName == null ) {
                System.out.println( "The name of the cell list file" +
                                    " to generate" +
                                    " must be specified." );
            }
            if ( outputLibListFileName == null ) {
                System.out.println( "The name of the library list file" +
                                    " to generate must be specified." );
            }
            if ( pcellLibListFileName == null ) {
                System.out.println( "The name of the pcell library list file" +
                                    " to generate must be specified." );
            }
            if ( pcellInfoPathStr == null ) {
                System.out.println( "You must specify a path in which to" +
                                    " search for pcell information." );
            }
            usage();
        }

        
    }


}
