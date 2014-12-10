/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */



package com.avlsi.tools.jauto;

import java.util.ArrayList;
import java.io.BufferedWriter;
import java.util.Collections;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Iterator;
import java.io.PrintWriter;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.ExclusiveNodeSet;
import com.avlsi.cell.ExclusiveNodeSets;
import com.avlsi.file.common.HierName;
import com.avlsi.io.FileSearchPath;
import com.avlsi.prs.ProductionRule;
import com.avlsi.prs.UnimplementableProductionRuleException;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.container.AliasedSet;

/**
 * Given a cell, writes the non-combinational nodes and the mutual
 * exclusion rules out to two separate files.  Will be used in the
 * checkcharge utility.  
 **/
public class CheckChargeTools {

    /**
     * This class cannot be instantiated.
     **/
    private CheckChargeTools() {
        throw new AssertionError();
    }

    private static void printMutualExclusionRules(CadenceInfo ci,
                                                  String filename,
                                                  String instanceName)
        throws IOException {
	final PrintWriter output =
            new PrintWriter(new BufferedWriter(new FileWriter(filename)));

        final ExclusiveNodeSets sets = ci.getLocalExclusiveNodeSets();
        final AliasedSet nodeAliases = ci.getLocalNodes();
        for (final Iterator i=sets.getIterator(); i.hasNext(); ) {
            ExclusiveNodeSet set = (ExclusiveNodeSet) i.next();
            final int hiLo = set.getHiLo();
            if (hiLo == ExclusiveNodeSet.HI)
                output.print("exclhi(");
            else if (hiLo == ExclusiveNodeSet.LO)
                output.print("excllo(");
            else if (hiLo == ExclusiveNodeSet.CC)
                output.print("exclcc(");
            else throw new AssertionError("Invalid hiLo value: " + hiLo);
            for (final Iterator nodes=set.getNodes(); nodes.hasNext(); ) {
                HierName nodeName = (HierName) nodes.next();
                HierName canonicalName =
                    (HierName) nodeAliases.getCanonicalKey(nodeName);
                output.print(instanceName + "." + canonicalName);
                if (nodes.hasNext())
                    output.print(",");
            }
            output.println(")");
        }

        output.close();
    }

    private static void printStaticizableNodes(Cadencize cad,
                                               CellInterface cell,
                                               CastFileParser cfp,
                                               String filename,
                                               String instanceName)
        throws IOException, UnimplementableProductionRuleException {
	final PrintWriter output =
            new PrintWriter(new BufferedWriter(new FileWriter(filename)));

        CadenceInfo ci = cad.convert(cell);
        // Prepare the netgraph
        final NetGraph graph = new NetGraph(ci.getLocalNodes(),
                                            ci.getLocalExclusiveNodeSets(),
                                            new ArrayList(),
                                            HierName.makeHierName("Vdd!"),
                                            HierName.makeHierName("GND!"),
                                            Collections.EMPTY_SET);
        graph.addCellInterface(cell, Collections.EMPTY_LIST, cfp, cad);
        graph.prepareForLvs();

        // Query all the nodes in the netgraph
        final Iterator nodes = graph.getNodes().iterator();
        while (nodes.hasNext()) {
            final NetGraph.NetNode node = (NetGraph.NetNode) nodes.next();
            final HierName name = node.getName();
            if ((! node.isCombinational()) &&
                (node.isNamed()) &&
                (node.isOutput()))
                output.println(instanceName + "." + name);
                
        }

        output.close();
    }

    private static void usage() {
        System.out.println("bad usage");
        System.exit(3);
    }

    public static void main(String [] args) throws Exception {
        //
        // Command-line parsing
        //
        CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 
        CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );        
        CommandLineArgs theArgs = cachedArgs;
        
        final String cellName = theArgs.getArgValue("cell", null);
        // The instance name of the cell.  All references to nodes get
        // prefixed with this.
        final String instanceName = theArgs.getArgValue("prefix", null);
        final boolean makeMutualExclusionRules = theArgs.argExists("mer");
        final boolean makeStaticizableNodes = theArgs.argExists("nodes");

	final FileSearchPath castSearchPath =
            new FileSearchPath( theArgs.getArgValue( "cast-path", "." ),
                                System.getProperty( "user.home" ) );


        // Things which may be command-line arguments in the future
        final String castVersion = "2";
        final String mutualExclusionOutputFile = "mutual_exclusion_rules";
        final String staticizableNodesOutputFile = "staticizable_nodes";
        final boolean verbose = false;

        // Basic sanity-checking
        if (cellName == null || instanceName == null)
            usage();

        //
        // Actual functionality
        //
        final CastFileParser castFileParser =
            new CastFileParser(castSearchPath, castVersion, verbose);
        final CellInterface castCell =
            castFileParser.getFullyQualifiedCell( cellName );
	final Cadencize cadencizer = new Cadencize(true);
	final CadenceInfo ci = cadencizer.convert(castCell);

        if (makeMutualExclusionRules)
            printMutualExclusionRules(ci, mutualExclusionOutputFile,
                                      instanceName);
        if (makeStaticizableNodes)
            printStaticizableNodes(cadencizer, castCell, castFileParser,
                                   staticizableNodesOutputFile,
                                   instanceName);

    }
}
