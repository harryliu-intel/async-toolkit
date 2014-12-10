/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.lvs;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.ExclusiveNodeSet;
import com.avlsi.cell.ExclusiveNodeSets;
import com.avlsi.fast.DirectiveBlock;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.BlockIterator;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.aspice.AspiceFile;
import com.avlsi.file.aspice.Transistor;
import com.avlsi.file.common.HierName;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.MultiSet;
import com.avlsi.util.container.Pair;
import com.avlsi.prs.ProductionRule;
import com.avlsi.prs.ProductionRuleSet;
import com.avlsi.util.bool.*;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.cast.CastFile;
import com.avlsi.cast.CastFileParser;

import com.avlsi.file.cdl.CDLParser;
import java.io.PrintWriter;
import java.io.FileWriter;
import com.avlsi.io.FileSearchPath;


import com.avlsi.util.container.StringContainerIterator;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;

/**
 * Generate a netlist cdl from a prs body of a cell.
 * Includes computing a minimal gate network, adding staticizers,
 * and matching library gates.
 *
 * @author Andrew Lines
 * @version $Revision$ $Date$
 **/
public final class PrsToNet {

    /** Enable debugging output. */
    static boolean verbose = false;

    /** 
     * Generate a NetGraph from a CellInterface.  libraryFile,
     * staticizerType, weakInverterType, and smallInverterType
     * may all be null if castCell already has a gate-matched
     * netlist block.  If libraryFile is specified, all gates
     * are used for gate matching.
     **/
    public static NetGraph getNetGraph(final CellInterface castCell,
                                       final CastFileParser cfp,
                                       final String libraryFile,
                                       final String staticizerType,
                                       final String weakInverterType,
                                       final String smallInverterType)
        throws Exception {
        return getNetGraph(castCell, cfp, libraryFile, staticizerType,
                           weakInverterType, smallInverterType,
                           Collections.emptyList());
    }

    /** 
     * Generate a NetGraph from a CellInterface.  libraryFile,
     * staticizerType, weakInverterType, smallInverterType, and
     * gateTypes may all be null if castCell already has a
     * gate-matched netlist block.
     **/
    public static NetGraph getNetGraph(final CellInterface castCell,
                                       final CastFileParser cfp,
                                       final String libraryFile,
                                       final String staticizerType,
                                       final String weakInverterType,
                                       final String smallInverterType,
                                       final List gateTypes) throws Exception {

        if (verbose) System.out.println("Building netgraph for subcell "+
                               castCell.getFullyQualifiedType());

        // load library staticizer and gates
        NetGraph staticizerGraph = null;
        NetGraph weakInverterGraph = null;
        NetGraph smallInverterGraph = null;
        List gates = new ArrayList();
        if (libraryFile != null) {
            try {
                // parse library file
                CDLParser cdlParser = new CDLParser();
                if (libraryFile != null) {
                            cdlParser.parseFile(libraryFile);
                }
                Iterator t;
                staticizerGraph = getCdlNetGraph(cdlParser,staticizerType);
                weakInverterGraph = getCdlNetGraph(cdlParser,weakInverterType);
                smallInverterGraph = getCdlNetGraph(cdlParser,smallInverterType);
                if (gateTypes.size()>0) t = gateTypes.iterator(); // specified
                else t = cdlParser.getCellTypes(); // match all
                while (t.hasNext()) {
                    String gateType = (String) t.next();
                    NetGraph gateGraph = getCdlNetGraph(cdlParser,gateType);
                    if (gateGraph==null) continue;
                    gates.add(gateGraph);
                }
            }
            catch (Exception e) {
                System.err.println("Couldn't parse "+libraryFile+".");
                System.err.println(e.toString());
                System.exit(1);
            }
        }

        // Flatten to get the right namespace
        CellInterface flatCell = castCell.flatten(-1);

        // make a cadencizer
        final Cadencize cadencizer = new Cadencize(true);
        final CadenceInfo ci = cadencizer.convert(flatCell);
        
        // get CAST namespace
        AliasedSet namespace = ci.getLocalNodes();
        if (verbose) System.err.println("Namespace=\n" + namespace);
        
        // get exclusives declarations
        final ExclusiveNodeSets exclusives = ci.getLocalExclusiveNodeSets();
        if (verbose) System.err.println("Exclusives=\n" + exclusives);

        // get staticizer related directives
        final Map nostats =
            DirectiveUtils.getPrsDirective(castCell,
                                           DirectiveConstants.NO_STAT,
                                           DirectiveConstants.NODE_TYPE);
        final Set nostatnodes = DirectiveUtils.getExplicitTrues(nostats);
            
        // build a NetGraph from prs
        List problems = new ArrayList();
        NetGraph netgraph = new NetGraph(namespace,exclusives,problems,
                                         HierName.makeHierName("Vdd"),
                                         HierName.makeHierName("GND"),
                                         nostatnodes);
        if (problems.size()>0) System.err.println("Problems:\n" + problems);
        
        // Recursively add each subcell's netgraph
        if (!castCell.containsCompletePrs()) {
            HashMap ngCache = new HashMap();
            for (Iterator sci=castCell.getLocalSubcellPairs(); sci.hasNext();) {
                Pair subcellPair = (Pair)sci.next();
                HierName name = (HierName) subcellPair.getFirst();
                CellInterface subcell = (CellInterface) subcellPair.getSecond();
                String type = subcell.getFullyQualifiedType();
                if (!subcell.hasRealProductionRule()) continue;
                NetGraph subcellNetgraph = (NetGraph) ngCache.get(type);
                if (subcellNetgraph == null) {
                    subcellNetgraph = getNetGraph(subcell,cfp,libraryFile,
                                                  staticizerType,weakInverterType,
                                                  smallInverterType,gateTypes);
                    ngCache.put(type,subcellNetgraph);
                }
                TreeMap prefixMap = subcellNetgraph.constructPrefixMap(name);
                netgraph.addNetGraph(subcellNetgraph,prefixMap,false);
            }
            // Not propagated correctly by addNetGraph()
            netgraph.markPortNodes(castCell);
            netgraph.prepareForLvs(); // needed to find paths properly
        }
        else {
            // Production rules, gate matching, and minimization.
            netgraph.addCellInterface(castCell, gates, cfp, cadencizer);
            //if (verbose) System.err.println("DNF NetGraph=\n" + netgraph);

            netgraph.prepareForLvs(); // needed to find paths properly
            
            // add staticizers
            netgraph.addStaticizers((NetGraph[]) gates.toArray(),
                                    staticizerGraph,weakInverterGraph,
                                    smallInverterGraph,true,
                                    castCell,cfp,cadencizer);
        }
        
        // output netgraph
        if (verbose) System.err.println("MGN NetGraph=\n" + netgraph);

        return netgraph;
    }

    /** Usage error. */
    private static void usage( int exitCode ) {
	System.err.println("Usage: java " + PrsToNet.class.getName() + "\n" +
                           "  [--verbose=0|1]\n" +
                           "  [--config=file]\n" + 
			   "  [--cast-path=castpath]\n" +
			   "  [--cast-version=1 | --cast-version=2]\n" + 
			   "  [--library=library.cdl]\n" +
                           "  [--staticizer=staticizerFQCN]\n" +
                           "  [--weak-inverter=weakInverterFQCN]\n" +
                           "  [--small-inverter=smallInverterFQCN]\n" +
			   "  [--gates=gate1,gate2,...]\n" +
			   "  --cell=castCell");

	System.exit( exitCode );
    }
    
    /** Parses a list for a string where each entry is 
	seperated from the next by a <code>,</code>.
	
	@param gateList String to parse.
	@return An ArrayList each entry of which is a java.lang.String.
    */
    private static ArrayList parseGateList( String gateList ) {
	ArrayList ret = new ArrayList();
	
	StringBuffer accumulator = new StringBuffer();
	int i=0;
	
	for( i=0; i<gateList.length();++i) {
	    if ( gateList.charAt(i) == ',' ) {
		if ( accumulator.length() > 0 ) {
		    ret.add( accumulator.toString() );
		    accumulator.delete( 0, accumulator.length() );
		}
	    }
	    else {
		accumulator.append( gateList.charAt(i) );
	    }
	}
        if ( accumulator.length() > 0 ) {
            ret.add( accumulator.toString() );
            accumulator.delete( 0, accumulator.length() );
        }
	return ret;
    }

    /** Make a NetGraph given a cdl file name and cell name. */
    static NetGraph getCdlNetGraph(CDLParser cdlParser, String cdlCellName)
	throws Exception {
	AspiceFile aspiceCell = cdlParser.getCell(cdlCellName);
        
	if (aspiceCell == null) {
	    System.err.println("WARNING: cell " + cdlCellName + " not found");
	    return null;
	}
	
        List problems = new ArrayList();
	NetGraph netgraph = new NetGraph(null,null,problems,
					 HierName.makeHierName("Vdd"),
					 HierName.makeHierName("GND"),
                                         Collections.EMPTY_SET);
        if (problems.size()>0) System.err.println("Problems in " + cdlCellName + 
                                                  ":\n" + problems);
        netgraph.gateType = cdlCellName;
	netgraph.addAspice(aspiceCell);
        netgraph.prepareForLvs(); // needed to find paths properly
	return netgraph;
    }

    /** Output network from prs body of a cell. */
    public static void main(String[] args) throws Exception {
	CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
	CommandLineArgs argsWithConfigs =
	    new CommandLineArgsWithConfigFiles( parsedArgs ); 
	CommandLineArgs cachedArgs = 
	    new CachingCommandLineArgs( argsWithConfigs );
	CommandLineArgs theArgs = cachedArgs;
	
	// get arguments for cast parsing
	final FileSearchPath castPath = 
            new FileSearchPath( theArgs.getArgValue( "cast-path", "." ) );
        final String castVersion = theArgs.getArgValue("cast-version",null);
        final CastFileParser cfp = new CastFileParser(castPath,castVersion,verbose);

        // get the staticizer and gate list arguments
        if (theArgs.getArgValue("verbose","").equals("1")) {
            verbose = true;
        }
        final String libraryFile = theArgs.getArgValue("library", null);
        final String staticizerType = theArgs.getArgValue("staticizer", null);
        final String weakInverterType = theArgs.getArgValue("weak-inverter", null);
        final String smallInverterType = theArgs.getArgValue("small-inverter", null);

        final ArrayList gateTypes = parseGateList( theArgs.getArgValue( "gates", "" ) );

        // get cast cell name
        String castCellName = theArgs.getArgValue("cell",null);
        if (castCellName==null) usage(1);

        // Get the CellInterface
        final CellInterface castCell = cfp.getFullyQualifiedCell(castCellName);
        
        // build a NetGraph from the cellInterface
        NetGraph netgraph = getNetGraph(castCell,cfp,libraryFile,
                                        staticizerType,weakInverterType,
                                        smallInverterType,gateTypes);
        
        // output .cdl and .il files
        final Cadencize cadencizer = new Cadencize(true);
        final CadenceInfo ci = cadencizer.convert(castCell);
        String cellType = castCell.getFullyQualifiedType();
        PrintWriter cdlOut = new PrintWriter(new FileWriter(cellType + ".cdl"));
        PrintWriter ilOut  = new PrintWriter(new FileWriter(cellType + ".il"));
        final AliasedMap portNodes = ci.getPortNodes();
        Iterator t = portNodes.getCanonicalKeys();
        String portlist = "";
        String quotedportlist = "";
        while (t.hasNext()) {
            HierName name = (HierName) t.next();
            if (((Boolean) portNodes.getValue(name)).booleanValue()) {
                portlist += " " + name;
                quotedportlist += " \"" + name + "\"";
            }
        }
        cdlOut.println(".SUBCKT " + cellType + "" +
                       portlist + "\n" +
                       netgraph.cdlString() + ".ENDS");
        ilOut.println("pinNames = (list" + quotedportlist + ")");
        ilOut.println("(defun createGates () \n" +
                      netgraph.skillString() + ")");
        cdlOut.close();
        ilOut.close();
        
        // output statistics
        t = netgraph.getEdges().iterator();
        int numNMOS=0, numPMOS=0, numGateNMOS=0, numGatePMOS=0;
        int numGates=0, numStaticizers=0;
        while (t.hasNext()) {
            NetGraph.NetEdge e = (NetGraph.NetEdge) t.next();
            if      (e.type == DeviceTypes.N_TYPE) {
                if (e.library && !e.floating) numGateNMOS++;
                else numNMOS++;
            }
            else if (e.type == DeviceTypes.P_TYPE) {
                if (e.library && !e.floating) numGatePMOS++;
                else numPMOS++;
            }
        }
        t = netgraph.getNodes().iterator();
        while (t.hasNext()) {
            NetGraph.NetNode n = (NetGraph.NetNode) t.next();
            if (n.gate != null) numGates++;
            if (n.staticizer != null) numStaticizers++;
        }
        System.out.println(castCellName +
                           "\nGates=" + numGates + " Staticizers=" + numStaticizers +
                           "\nN=" + numNMOS + " P=" + numPMOS + 
                           " GateN=" + numGateNMOS + " GateP=" + numGatePMOS + 
                           " Total=" + (numNMOS + numPMOS + numGateNMOS + numGatePMOS));
    }
}
