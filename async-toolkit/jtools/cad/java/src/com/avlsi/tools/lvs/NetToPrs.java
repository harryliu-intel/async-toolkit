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
import com.avlsi.util.container.MultiSet;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.AliasedMap;
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
 * Generate a prs set from a netlist body.  Use for importing standard gates.
 *
 * @author Andrew Lines
 * @version $Revision$ $Date$
 **/
public final class NetToPrs {

    /** Enable debugging output. */
    static boolean verbose = false;

    /** Usage error. */
    private static void usage( int exitCode ) {
	System.err.println("Usage: java " + NetToPrs.class.getName() + "\n" +
                           "  [--config=file]\n" + 
			   "  [--cast-path=castpath]\n" +
			   "  [--cast-version=1 | --cast-version=2]\n" + 
			   "  --cell=castCell");

	System.exit( exitCode );
    }
    
    /** Output prs from cdl. */
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
        String castVersion= theArgs.getArgValue("cast-version",null);
        
        // start cast parser
        String castCellName = theArgs.getArgValue("cell",null);
        if (castCellName==null) usage(1);
        final CastFileParser cfp = new CastFileParser(castPath,castVersion,verbose);
        
        // get CellInterface
        final CellInterface castCell;
        castCell = cfp.getFullyQualifiedCell( castCellName );
        
        // make a cadencizer
        final Cadencize cadencizer = new Cadencize(true);
        final CadenceInfo ci = cadencizer.convert(castCell);
        
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
        
        // build a NetGraph from cdl [or prs] of cast cell
        List gates = new ArrayList();
        List problems = new ArrayList();
        NetGraph netgraph = new NetGraph(namespace,exclusives,problems,
                                         HierName.makeHierName("Vdd"),
                                         HierName.makeHierName("GND"),
                                         nostatnodes);
        netgraph.addCellInterface(castCell,gates,cfp,cadencizer);
        netgraph.prepareForLvs();
        if (problems.size()>0) System.err.println("Problems:\n" + problems);
        
        // emit prs
        ProductionRuleSet prs = netgraph.getProductionRuleSet();
        for (Iterator i = netgraph.nodes.iterator(); i.hasNext(); ) {
            NetGraph.NetNode node = (NetGraph.NetNode) i.next();
            if (!node.isPort() && !node.isStaticizerInverter() && 
                (node.isGate() || node.isOutput()))
                System.out.println("node \"" + node.name + "\";");
        }
        System.out.print(prs.toString());
    }
}
