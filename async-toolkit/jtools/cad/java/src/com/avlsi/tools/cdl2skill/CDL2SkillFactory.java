/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cdl2skill;


import java.util.Map;
import java.util.Set;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import java.text.MessageFormat;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.BufferedWriter;
import java.io.Writer;
import java.io.IOException;
import java.io.FileNotFoundException;

import com.avlsi.cast.impl.Environment;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.common.HierName;

import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLstat;

import com.avlsi.tools.cdl2skill.CDL2SkillException;
import com.avlsi.tools.jauto.TechnologyData;

public class CDL2SkillFactory implements CDLFactoryInterface {

    private final MessageFormat headerFormatter;
    private final MessageFormat makeInstanceFormatter;
    private final MessageFormat makeConnectionFormatter;
    private final MessageFormat makeInstanceParameterFormatter;
    private final MessageFormat makeNetFormatter;
    private final MessageFormat makeTerminalFormatter;
    private final MessageFormat makeStatisticsFormatter;

    private final StringBuffer accumulator;

    private final File mOutputDir;

    private final Set mLibSet;
    private final List mCellList;

    private final Map mCellTemplateMap;
    private final Map mCellStatMap;

    //Nets that have already been written.
    private final Set mWrittenNets;

    //Map from cell name to list of ports.
    private final Map mCellPorts;

    private Writer mCurrWriter;

    private String mCurrCellName;

    private CDL2SkillException mError;

    private final TechnologyData mTechData;

    public CDL2SkillFactory( final File outputDir, 
                             final String skillFunctionPrefix,
                             final String skillTableName,
                             final Set libSet,
                             final List cellList,
                             final Map cellTemplateMap,
                             final Map cellStatMap,
                             final TechnologyData techData ) {

        // Cell name, Library name
        final String headerFormatStr =
            "( setq\n" +
            "  " + skillTableName + "\n" +
            "  ( " + skillFunctionPrefix + "CreateEmptyNetlistTable\n" +
            "    \"{1}\"\n" +
            "    \"{0}\" ) )\n" ;
        
        // Instance name, Cell Name of master of instance, Library Name of master of instance
        final String makeInstanceFormatStr =
            "( " + skillFunctionPrefix + "CreateInstanceInNetlist\n" +
            "  " + skillTableName + "\n" +
            "  \"{0}\"\n" +
            "  {2}\n" +
            "  \"{1}\" )\n";
        
        // Instance Name, Terminal Name, Net Name
        final String makeConnectionFormatStr =
            "( " + skillFunctionPrefix + "AddConnectionToInstanceInNetlist\n" +
            "  " + skillTableName + "\n" +
            "  \"{0}\"\n" +
            "  \"{1}\"\n" +
            "  \"{2}\" )\n";
        
        // Instance Name, Parameter name, Parameter value
        final String makeInstanceParameterFormatStr =
            "( " + skillFunctionPrefix + "AddParameterToInstanceInNetlist\n" +
            "  " + skillTableName + "\n" +
            "  \"{0}\"\n" +
            "  \"{1}\"\n" +
            "  {2} )\n";
        
        // Net name
        final String makeNetFormatStr =
            "( " + skillFunctionPrefix + "AddNet\n" +
            "  " + skillTableName + "\n" +
            "  \"{0}\" )\n";
        
        // Net name
        final String makeTerminalFormatStr =
            "( " + skillFunctionPrefix + "AddTerminal\n" +
            "  " + skillTableName + "\n" +
            "  \"{0}\" )\n";
        
        
        // Transistor Count, Area, Width, Length
        final String makeStatisticsFormatStr =
            "( " + skillFunctionPrefix + "SetTotalTransistorCount\n" +
            "  " + skillTableName + "\n" +
            "  {0,number,#############} )\n" +
            "( " + skillFunctionPrefix + "SetTotalTransistorGateArea\n" +
            "  " + skillTableName + "\n" +
            "  {1,number,0.0000E0} )\n" +
            "( " + skillFunctionPrefix + "SetTotalTransistorWidth\n" +
            "  " + skillTableName + "\n" +
            "  {2,number,0.0000E0} )\n" +
            "( " + skillFunctionPrefix + "SetTotalTransistorLength\n" +
            "  " + skillTableName + "\n" +
            "  {3,number,0.0000E0} )\n";
        
        headerFormatter = new MessageFormat( headerFormatStr );
        makeInstanceFormatter = new MessageFormat( makeInstanceFormatStr );
        makeConnectionFormatter = new MessageFormat( makeConnectionFormatStr );
        makeNetFormatter = new MessageFormat( makeNetFormatStr );
        makeTerminalFormatter = new MessageFormat( makeTerminalFormatStr );
        makeInstanceParameterFormatter = new MessageFormat( makeInstanceParameterFormatStr );
        makeStatisticsFormatter = new MessageFormat( makeStatisticsFormatStr );

        accumulator = new StringBuffer();

        mOutputDir = outputDir;

        mLibSet = libSet;
        mCellList = cellList;

        mCellTemplateMap = cellTemplateMap;
        mCellStatMap = cellStatMap;

        mWrittenNets = new HashSet( );

        mCellPorts = new HashMap();

        mCurrWriter = null;
        mCurrCellName = null;
        mError = null;
        mTechData = techData;
    }

    private String getLibNameFromCellName( final String cellName ) throws CDL2SkillException {
        final int lastDot = cellName.lastIndexOf( '.' );
        if ( lastDot > 0 ) {
            final int secondToLastDot = cellName.lastIndexOf( '.', lastDot - 1 );
            if ( secondToLastDot > 0 ) { 
                return cellName.substring( 0, secondToLastDot );
            }
        }
        throw new CDL2SkillException( "\"" + cellName + "\" is not a valid cell name." );
    }

    private void writeString( final String str ) {
        if ( mError == null ) {
            try {
                mCurrWriter.write( str );
            }
            catch ( IOException e ) {
                mError = new CDL2SkillException( "Unable to write \"" + str + "\" to writer.", e );
            }
        }
    }

    private String makeHeaderStr( final String cellName ) throws CDL2SkillException {
        
        final String libName = getLibNameFromCellName( cellName );

        // Cell Name, Library Name
        final Object[] headerParams = {
            cellName,
            libName
        };
        
        headerFormatter.format( headerParams, accumulator, null );
        
        final String ret = accumulator.toString();

        accumulator.setLength( 0 );

        return ret;

    }

    private void writeHeader( final String cellName ) {
        try {
            writeString( makeHeaderStr( cellName ) );
        }
        catch ( CDL2SkillException e ) {
            mError = e;
        }
    }

    private String makeInstanceStr( final String instanceName,
                                    final String masterLibName,
                                    final String masterCellName )  {
        return makeInstanceStr(instanceName,
                               masterLibName,
                               masterCellName,
                               true);        
    }

    private String makeInstanceStr( final String instanceName,
                                    final String masterLibName,
                                    final String masterCellName,
                                    final boolean quote)  {
        final Object[] instanceParams = {
            instanceName,
            masterCellName,
            quote?"\"" + masterLibName + "\"" : masterLibName
        };
        
        makeInstanceFormatter.format( instanceParams, accumulator, null );
        
        final String ret = accumulator.toString();
        
        accumulator.setLength( 0 );
        
        return ret;
    
    }

    private String makeInstanceStr( final String instanceName,
                                    final String masterCellName ) throws CDL2SkillException {
        final String masterLibName = getLibNameFromCellName( masterCellName );
        return makeInstanceStr( instanceName,
                                masterLibName,
                                masterCellName );
    }

    private void writeInstance( final String instanceName,
                                final String masterLibName,
                                final String masterCellName) {
        writeInstance(instanceName,
                      masterLibName,
                      masterCellName,
                      true);
    }

    private void writeInstance( final String instanceName,
                                final String masterLibName,
                                final String masterCellName,
                                final boolean quote) {
       writeString( makeInstanceStr( instanceName,
                                     masterLibName,
                                     masterCellName,
                                     quote) );
    }

    private void writeInstance( final String instanceName,
                                final String masterCellName ) {
       try {
           writeString( makeInstanceStr( instanceName, masterCellName ) );
       }
       catch ( CDL2SkillException e ) {
            mError = e;
       }
    }

    private String makeConnectionStr( final String instanceName,
                                      final String terminalName,
                                      final String netName ) {
        final Object[] connectionParams = {
            instanceName,
            terminalName,
            netName
        };
        
        makeConnectionFormatter.format( connectionParams, accumulator, null );
        
        final String ret = accumulator.toString();
        
        accumulator.setLength( 0 );
        
        return ret;

    }

    private void writeConnection( final String instanceName,
                                  final String terminalName,
                                  final String netName ) {
        writeString( makeConnectionStr( instanceName, terminalName, netName ) );
    }

    private String makeInstanceParameterStr( final String instanceName,
                                             final String parameterName,
                                             final String parameterValue ) {
        final Object[] instanceParameterParams = {
            instanceName,
            parameterName,
            parameterValue
        };

        makeInstanceParameterFormatter.format( instanceParameterParams, accumulator, null );
        
        final String ret = accumulator.toString();
        
        accumulator.setLength( 0 );
        
        return ret;

    }
    
    private void writeInstanceParameter( final String instanceName,
                                         final String parameterName,
                                         final String parameterValue ) {
        writeString( makeInstanceParameterStr( instanceName, 
                                               parameterName,
                                               parameterValue ) );
    }

    private String makeNetStr( final String netName ) {
        final Object[] netParams = {
            netName
        };

        makeNetFormatter.format( netParams, accumulator, null );
        
        final String ret = accumulator.toString();
        
        accumulator.setLength( 0 );
        
        return ret;

    }

    private void writeNet( final String netName ) {
        if ( ! ( mWrittenNets.contains( netName ) ) ) {
            mWrittenNets.add( netName );
            writeString( makeNetStr( netName ) );
        }
    }

    private String makeTerminalStr( final String terminalName ) {
        final Object[] terminalParams = {
            terminalName
        };
        makeTerminalFormatter.format( terminalParams, accumulator, null );
        
        final String ret = accumulator.toString();
        
        accumulator.setLength( 0 );
        
        return ret;

    }

    private void writeTerminal( final String terminalName ) {
        if ( ! ( mWrittenNets.contains( terminalName ) ) ) {
            mWrittenNets.add( terminalName );
            writeString( makeTerminalStr( terminalName ) );
        }
    }

    private String makeStatisticsString( final String cellName ) {
        final CDLstat.CellStat stat = CDLstat.getCellStat( cellName,
                                                           mCellTemplateMap,
                                                           mCellStatMap,
                                                           null,
                                                           mTechData
                                                           );
        return makeStatisticsString( cellName, stat );
    }

    /**
     * Override to provide statistical information about the cell specified by
     * cellName.
     **/
    protected String makeStatisticsString( final String cellName,
                                           final CDLstat.CellStat stat ) {
        final Object statParams[] = {
            new Integer( stat.transistors ),
            new Double( stat.area ),
            new Double( stat.width ),
            new Double( stat.length )
        };
        
        makeStatisticsFormatter.format( statParams, accumulator, null );

        final String ret = accumulator.toString();
        
        accumulator.setLength( 0 );
        return ret;                     
    }

    private void writeStatistics( final String cellName ) {
        
        writeString( makeStatisticsString( cellName ) );
        
    }

    public void makeResistor( final HierName name,
                              final HierName n1,
                              final HierName n2,
                              final CDLLexer.InfoToken val,
                              final Map parameters,
                              final Environment env ) {
        //dummy implementation...errors will occur in skill if used
        if ( mError == null ) {
            final String instanceName = name.toString();

            final String ns1 = n1.toString();
            final String ns2 = n2.toString();

            writeNet( ns1 );
            writeNet( ns2 );

            writeInstance( instanceName, "", "" );
            
            writeConnection( instanceName, "", ns1 );
            writeConnection( instanceName, "", ns2 );

            final Double rVal = val.getValue( env );

            writeInstanceParameter( instanceName,
                                    "",
                                    rVal.toString() );            
        }
    }

    public void makeCapacitor( final HierName name, 
                               final HierName npos,
                               final HierName nneg,
                               final CDLLexer.InfoToken val,
                               final Map parameters,
                               final Environment env ) {
        //dummy implementation...errors will occur in skill if used
        if ( mError == null ) {
            final String instanceName = name.toString();

            final String pos = npos.toString();
            final String neg = nneg.toString();

            writeNet( pos );
            writeNet( neg );

            writeInstance( instanceName, "", "" );
            
            writeConnection( instanceName, "", pos );
            writeConnection( instanceName, "", neg );

            final Double cVal = val.getValue( env );

            writeInstanceParameter( instanceName,
                                    "",
                                    cVal.toString() );            
        }
    }

    public void makeTransistor( final HierName name, 
                                final String type,
                                final HierName ns, 
                                final HierName nd,
                                final HierName ng, 
                                final HierName nb,
                                final CDLLexer.InfoToken w,
                                final CDLLexer.InfoToken l,
                                final Map parameters,
                                final Environment env ) {
        if ( mError == null ) {

            final String instanceName = name.toString();

            final String source = ns.toString();
            final String drain = nd.toString();
            final String gate = ng.toString();
            final String bulk = nb.toString();

            final Double wVal = w.getValue( env );
            final Double lVal = l.getValue( env );

            if ( ( wVal != null ) && ( lVal != null ) ) {
                writeNet( source );
                writeNet( drain );
                writeNet( gate );
                writeNet( bulk );
                
                writeInstance( instanceName, "TransistorLibrary", type, false );
                
                writeConnection( instanceName, "S", source );
                writeConnection( instanceName, "D", drain );
                writeConnection( instanceName, "G", gate );
                writeConnection( instanceName, "B", bulk );
                
                writeInstanceParameter( instanceName,
                                        "w",
                                        wVal.toString() );
                writeInstanceParameter( instanceName,
                                        "l",
                                        lVal.toString() );
            }
            else {
                mError = new CDL2SkillException( "\"" + 
                                                 mCurrCellName + 
                                                 "\": \"" + 
                                                 w.getText( env ) + 
                                                 "\" is not a valid transistor width." );
            }
        }
    }

    public void makeDiode( final HierName name,
                           final String type,
                           final HierName npos,
                           final HierName nneg,
                           final CDLLexer.InfoToken val,
                           final Map parameters,
                           final Environment env) {
        //dummy implementation...errors will occur in skill if used
        if ( mError == null ) {
            final String instanceName = name.toString();

            final String pos = npos.toString();
            final String neg = nneg.toString();

            writeNet( pos );
            writeNet( neg );

            writeInstance( instanceName, "", "" );
            
            writeConnection( instanceName, "", pos );
            writeConnection( instanceName, "", neg );

            final Double dVal = val.getValue( env );

            writeInstanceParameter( instanceName,
                                    "",
                                    dVal.toString() );            
        }
    }

    public void makeInductor( final HierName name,
                              final HierName npos,
                              final HierName nneg,
                              final CDLLexer.InfoToken val,
                              final Map parameters,
                              final Environment env ) {
        if ( mError == null ) {
            mError = new CDL2SkillException( "\"" + mCurrCellName + "\": Inductors are not supported." );
        }
    }

    public void makeBipolar( final HierName name,
                             final String type,
                             final HierName nc,
                             final HierName nb,
                             final HierName ne,
                             final CDLLexer.InfoToken val,
                             final Map parameters,
                             final Environment env) {
        if ( mError == null ) {
            final String instanceName = name.toString();

            final String snc = nc.toString();
            final String snb = nb.toString();
            final String sne = ne.toString();

            writeNet( snc );
            writeNet( snb );
            writeNet( sne );

            writeInstance( instanceName, "", "" );
            
            writeConnection( instanceName, "", snc );
            writeConnection( instanceName, "", snb );
            writeConnection( instanceName, "", sne );

            final Double dVal = val.getValue( env );

            writeInstanceParameter( instanceName,
                                    "",
                                    dVal.toString() );            
        }
    }

    public void makeCall( final HierName name,
                          final String subName, 
                          final HierName[] args,
                          final Map parameters,
                          final Environment env ) {
        if ( mError == null ) {
            final String instanceName = name.toString();
            final String[] terminalNames = ( String[] ) mCellPorts.get( subName );
            if ( terminalNames != null ) {
                if ( terminalNames.length == args.length ) {
                    int i;
                    for ( i = 0 ; i < args.length ; ++i ) {
                        writeNet( args[i].toString() );
                    }
                    
                    writeInstance( instanceName,
                                   subName );
                    
                    final Iterator paramIter = parameters.entrySet().iterator();
                    
                    while ( paramIter.hasNext() ) {
                        final Map.Entry currEntry = ( Map.Entry ) paramIter.next();
                        
                        final String currParamName = ( String ) currEntry.getKey();
                        final String currParamValue = 
                            ( ( CDLLexer.InfoToken ) currEntry.getValue() ).getText();
                        writeInstanceParameter( instanceName, 
                                                currParamName, 
                                                "\"" + currParamValue + "\"" );
                    }
                    
                    for ( i = 0 ; i < args.length ; ++i ) {
                        writeConnection( instanceName, terminalNames[i], args[i].toString() );
                    }
                }
            }
            else {
                mError = new CDL2SkillException( "\"" + 
                                                 mCurrCellName + 
                                                 "\": Unable to find port list for \"" +
                                                 subName +
                                                 "\"." );
            }
        }
    }

    private void doPorts( final String cellName, 
                          final String[] in,
                          final String[] out ) {
        final String[] terminals = new String[ in.length + out.length ];
        int destIndex = 0;
        
        int i;
        for ( i = 0 ; i < in.length ; ++i ) {
            terminals[destIndex] = in[i];
            ++destIndex;
        }

        for ( i = 0 ; i < out.length ; ++i ) {
            terminals[destIndex] = out[i];
            ++destIndex;
        }

        mCellPorts.put( cellName, terminals );

        for ( i = 0 ; i < terminals.length ; ++i ) {
            writeTerminal( terminals[i] );
        }
    }

    private void addCellToSets( final String cellName ) {
        if ( mError == null ) {
            try {
                final String libName = getLibNameFromCellName( cellName );
                mLibSet.add( libName );
                mCellList.add( cellName );
            }
            catch ( CDL2SkillException e ) {
                mError = e;
            }
        }
    }
    
    public void beginSubcircuit( final String subName,
                                 final String[] in,
                                 final String[] out,
                                 final Map parameters,
                                 final Environment env ) {

        if ( mError == null ) {
            
            final File skillFile = new File( mOutputDir, subName + ".netlist.il" );
            try {
                final OutputStream outputStream = new FileOutputStream( skillFile );
                mCurrWriter = new BufferedWriter( new OutputStreamWriter( outputStream, "UTF-8" ) );

                mCurrCellName = subName;

                writeHeader( subName );
                doPorts( subName, in, out );

                addCellToSets( subName );

            }
            catch ( FileNotFoundException e ) {
                mError = new CDL2SkillException( "Unable to open " + 
                                                 skillFile.toString() +
                                                 ".",
                                                 e );
            }
            catch ( IOException e ) {
                mError = new CDL2SkillException( "Unable to open " + 
                                                 skillFile.toString() +
                                                 ".",
                                                 e );
            }
        }
    }

    public void endSubcircuit( final String subName, final Environment env) {

        if ( mError == null ) {
            try {

                writeStatistics( subName );

                mCurrWriter.close();
                mCurrWriter = null;
                mCurrCellName = null;
                mWrittenNets.clear();
            }
            catch( IOException e ) {
                mError = new CDL2SkillException( "Unable to close the writer.", e );
            }
        }
    }

    public CDL2SkillException getError() {
        return mError;
    }
    

}
