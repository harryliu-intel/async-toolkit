/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.spicemine;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.TreeSet;
import java.util.Comparator;
import java.util.Set;
import java.util.SortedSet;
import java.util.HashSet;
import java.io.IOException;
import java.io.FileOutputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.BufferedWriter;
import java.io.PrintStream;
import java.io.FileWriter;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.BufferedReader;

import com.avlsi.circuit.LumpedRCGraph;
import com.avlsi.circuit.ResistiveSubnet;
import com.avlsi.circuit.AbstractNode;
import com.avlsi.circuit.CullingCircuitGraph;
import com.avlsi.circuit.TestSubnet;
import com.avlsi.circuit.ConjunctivePath;
import com.avlsi.circuit.AbstractCircuit;
import com.avlsi.file.aspice.AspiceCell;
import com.avlsi.file.aspice.CastTestEnvironment;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.spice.SpiceParser;
import com.avlsi.file.spice.SpiceFileFormatException;
import com.avlsi.util.text.NumberFormatter;
import com.avlsi.util.debug.Debug;

/********************************************************************
 * Mines a .spice (or .cdl) file for various data relating to its
 * resistive subnets.  Identifies nets with alarmingly high wire
 * resistance and/or load capacitance.  Currently in a pretty 
 * rough form.
 *
 * @see LumpedRCGraph
 *
 * @author Mike Davies
 * @version $Revision$ $Date$
 ********************************************************************/
public final class SpiceMine {

    private static boolean quiet = false;
    private static boolean debug = false;

    private static final int RES      = 0;
    private static final int RC       = 1;
    private static final int COUPLING = 2;

    private static final String confFile = "/usr/local/cad/lib/spicemine.conf";

    /*****************************************************************
     * Exit routine which outputs SpiceMine usage information
     *****************************************************************/
    private static void usage(final int exitStatus) 
    {
        System.err.println(
        "Usage: spicemine\n"+
        "       [-D]                        - "+
                "Enable debugging output.\n"+
        "       [--print subnets|auto]      - "+
                "Select output format mode.\n"+
        "       [--net netname]             - "+
                "Examine only the specified net.\n"+
        "       [--netfile file]            - "+
                "Examine only the nets in 'file'.\n"+
        "       [-s|--sort res|rc|coupling] - "+
                "Sort nets by max res or rc.\n"+
        "       [--worst N[%]]              - "+
                "Examine worst N or N% of nets.\n"+
        "       [-q|--quiet]                - "+
                "Be quiet.\n"+
        "       [--cell cellname]           - "+
                "Examine cellname from spicefile.\n"+
        "       [--reset reset_time]        - "+
                "Specify the aspice reset time period (seconds).\n"+
        "       [--dtu dtu_time]            - "+
                "Specify the dsim time unit (seconds).\n"+
        "       [--tau tau_time]            - "+
                "Specify the transition time allowance (seconds).\n"+
        "       <spicefile>                 - "+
                "Spice file to read.\n"
        );
        System.exit(exitStatus);
    }

    /*****************************************************************
     * Simple UI line count update ParserCallback class
     *****************************************************************/
    static final class LineUpdateCallback extends ProgressUpdateCallback
                                  implements SpiceParser.ParsingCallback {
        private final int linePeriod;

        /**
         * Constructor.
         * @param linePeriod Screen update will happen every time this many
         *        lines are processed.
         * @param outStream output will be written to this stream.
         **/
        LineUpdateCallback(int linePeriod, final PrintStream outStream) {
            super(outStream);
            this.linePeriod = linePeriod;
        }

        /**
         * The parser will call this.
         **/
        public void update(int lineNum) {
            if (lineNum % linePeriod == 0) {
                String s = String.valueOf(lineNum);
                clearLast();
                printToStream(s);
            }
        }
    }
                
    /*****************************************************************
     * UI callback class for LumpedRCGraph methods.
     *****************************************************************/
    static final class LumpedGraphCallback extends ProgressUpdateCallback
                       implements LumpedRCGraph.CallbackInterface {
        private int updatePeriod;
        private int numNets;

        LumpedGraphCallback(int updatePeriod, final PrintStream outStream) {
            super(outStream);
            this.updatePeriod = updatePeriod;
        }

        public void identifyNetsUpdate(int num, final Set uncoveredNodes) {
            if (num% updatePeriod == 0) {
                String s = String.valueOf(uncoveredNodes.size());
                s += ":" + num;
                printToStream(s);
            }
        }

        public void setNumNets(int num) { numNets = num; }

        public void netAlgorithmUpdate(int num) {
            if (numNets > 100 && num % (numNets/100) == 0) {
                String s = String.valueOf(num);
                s += ":" + numNets;
                printToStream(s);
            }
        }
    }
 
    /*****************************************************************
     * Main: Read the extract file and do stuff.
     *****************************************************************/
    public static void main(final String[] args) 
    {
        //
        // Initialization & Command Line Parsing
        //
        boolean printSubnets = false;
        boolean printAutoSubnets = false;
        boolean sortSubnets  = false;
        boolean examineWorstNets = false;
        boolean examineByPercent = false;
        int     numExamineNets = 0;
        int     sortBy = RES;
        String  cellName = "";
        String  netName = "";
        String  netFile = "";
        String  outputFile = "";

        int i=0;
        while (i<args.length && args[i].startsWith("-")) {
            if (args[i].equals("--cell")) {
                if (i+1==args.length) usage(1);
                cellName = args[i+1];
                i++;
            }
            else if (args[i].equals("-D")) {
                debug = true;
            }
            else if (args[i].equals("-s") || args[i].equals("--sort")) {
                if (i+1==args.length) {
                    System.err.println("Need to specify a sorting metric.");
                    System.exit(1);
                }
                sortSubnets = true;
                if (args[i+1].equals("res")) {
                    sortBy = RES;
                }
                else if (args[i+1].equals("rc")) {
                    sortBy = RC;
                }
                else if (args[i+1].equals("coupling")) {
                    sortBy = COUPLING;
                }
                else {
                    System.err.println("Unrecognized sorting metric '"+
                                       args[i+1]+"'.");
                    System.exit(1);
                }
                i++;
            }
            else if (args[i].equals("--print")) {
                if (i+1==args.length) {
                    System.err.println("Need to specify a data set.");
                    System.exit(1);
                }
                if (args[i+1].equals("subnets")) {
                    printSubnets = true;
                }
                else if (args[i+1].equals("auto")) {
                    printAutoSubnets = true;
                }
                else {
                    System.err.println("Unrecognized set '"+args[i+1]+"'");
                    System.exit(1);
                }
                i++;
            }
            else if (args[i].equals("--net")) {
                if (i+1==args.length) usage(1);
                netName = args[i+1];
                i++;
            }
            else if (args[i].equals("--netfile")) {
                if (i+1==args.length) usage(1);
                netFile = args[i+1];
                i++;
            }
            else if (args[i].equals("--worst")) {
                if (i+1==args.length) usage(1);
                int l = args[i+1].length();
                if (args[i+1].endsWith("%")) {
                    l = args[i+1].length()-1;
                    examineByPercent = true;
                }
                try {
                    numExamineNets = Integer.parseInt(args[i+1].substring(0,l));
                }
                catch (NumberFormatException e) {
                    System.err.println("Invalid numeric argument: "+args[i+1]);
                    System.exit(1);
                }
                examineWorstNets = true;
                i++;
            }
            else if (args[i].equals("-o")) {
                if (i+1==args.length) usage(1);
                outputFile = args[i+1];
                i++;
            }
            else if (args[i].equals("--reset")) {
                if (i+1==args.length) usage(1);
                try {
                    TestSubnet.T_RESET = Float.parseFloat(args[i+1]);
                }
                catch (NumberFormatException e) {
                    System.err.println("Invalid numeric argument: "+args[i+1]);
                    System.exit(1);
                }
                i++;
            }
            else if (args[i].equals("--dtu")) {
                if (i+1==args.length) usage(1);
                try {
                    TestSubnet.DTU = Float.parseFloat(args[i+1]);
                }
                catch (NumberFormatException e) {
                    System.err.println("Invalid numeric argument: "+args[i+1]);
                    System.exit(1);
                }
                i++;
            }
            else if (args[i].equals("--tau")) {
                if (i+1==args.length) usage(1);
                try {
                    TestSubnet.DSIM_TIME_TAU_MAX = Integer.parseInt(args[i+1]);
                }
                catch (NumberFormatException e) {
                    System.err.println("Invalid numeric argument: "+args[i+1]);
                    System.exit(1);
                }
                i++;
            }
            else if (args[i].equals("-q") || args[i].equals("--quiet")) {
                quiet = true;
            }
            else {
                System.err.println("Unrecognized option '"+args[i]+"'");
                usage(1);
            }
            i++;
        }
        if (args.length-i < 1) usage(1);
        final String spiceFile = args[i];

        // 
        // Specific net analysis:
        // Either --net or --netfile was given.
        //
        if (netName != "" || netFile != "") {
            HashSet netSet = new HashSet();
            if (netFile != "") {
                try {
                    netSet.addAll(readNetsFromFile(netFile));
                }
                catch (Exception e) {
                    System.err.println("Error reading "+netFile+":");
                    System.err.println(" "+e.getMessage());
                    System.exit(1);
                }
            }
            if (netName != "") {
                try {
                    netSet.add(HierName.makeHierName(netName,'/'));
                }
                catch (InvalidHierNameException e) {
                    System.err.println("Error: '"+netName+"' is an invalid "+
                                       "net name.");
                    System.err.println("       "+e.getMessage());
                    System.exit(1);
                }
            }
            examineNets(spiceFile,cellName,netSet);
        }

        //
        // Otherwise examine all resistive subnets and, if specified,
        // pick out the worst ones.
        //
        else {
            //
            // Perform RC analysis
            //
            SortedSet orderedNets = 
                doResistiveAnalysis(spiceFile,cellName,printSubnets,
                                    printAutoSubnets,outputFile,sortSubnets,
                                    sortBy,numExamineNets,examineByPercent);
            if (sortSubnets && examineWorstNets) {
                /*
                //
                // Cull nets for examination
                //
                int beginAt;
                if (examineByPercent)
                    beginAt = orderedNets.size() -
                        (int)((float)numExamineNets*orderedNets.size()/100);
                else
                    beginAt = orderedNets.size() - numExamineNets;

                */
                HashSet netSet = new HashSet();
                Iterator it = orderedNets.iterator();
                /*
                for (i=0; i < beginAt; i++) {
                    Debug.assertTrue(it.hasNext());
                    it.next();
                }
                */
                while (it.hasNext()) {
                    ResistiveSubnet rsn = (ResistiveSubnet) it.next();
                    netSet.add(rsn.getCanonicalName().getResistiveSubnetName());
                }
                //
                // Allow the ResistiveSubnets to be garbage collected
                //
                orderedNets = null;

                // 
                // Examine the suspicious nets
                //
                examineNets(spiceFile,cellName,netSet);
            }
            else if (!sortSubnets && examineWorstNets) {
                System.err.println("Cannot identify suspcious nets: "+
                                   "--worst requires --sort.");
                System.exit(1);
            }
        }
    }

    /*****************************************************************
     * Reads a set of net names from the specified file.  Comment
     * lines (beginning with a '#') and blank lines are ignored.
     *****************************************************************/
    public static Set readNetsFromFile(final String fileName) 
        throws FileNotFoundException, IOException
    {
        BufferedReader reader = new BufferedReader(new FileReader(fileName));
        HashSet netSet = new HashSet();
        StringBuffer sb = new StringBuffer();
        int ch, lineNum = 1;
        while ((ch = reader.read()) != -1) {
            if (ch != '\n') sb.append((char)ch);
            else {
                String line = sb.toString().trim();
                if (!line.startsWith("#") && line.length() != 0) {
                    try {
                        netSet.add(HierName.makeHierName(sb.toString(),'/'));
                    }
                    catch (InvalidHierNameException e) {
                        System.err.println("Invalid net name found on line "+
                                           lineNum+" of "+fileName+":");
                        System.err.println(" "+e.getMessage());
                        System.exit(1);
                    }
                }
                sb = new StringBuffer();
                lineNum++;
            }
        }
        reader.close();
        return netSet;
    }

    /*****************************************************************
     * Determines the cell name from the parsed AbstractCircuit
     * repository.  Relies on there only being one SUBCKT per
     * extract file.  Exits if no cell or multiple cells are defined.
     *****************************************************************/
    public static String lookupCell(AbstractCircuit.Repository repos)
    {
        Iterator it;
        it = repos.getCellTypesIterator();
        if (!it.hasNext()) {
            System.err.println("No cell definitions found");
            System.exit(1);
        }
        String cellName = (String)it.next();
        if (it.hasNext()) {
            System.err.println("Multiple cell definitions found.  "+
                               "Use the --cell option.");
            System.exit(1);
        }
        return cellName;
    }

    /*****************************************************************
     * Performs an R/C analysis of all resistive subnets in the
     * specified cell of the spice extract file.  Generates a
     * cell.report analysis report file, as well as an optional
     * listing of all resistive subnets.  If <code>sortSubnets</code>
     * is set, will return a set of the resistive subnets, sorted
     * according to the metric specified.
     *****************************************************************/
    public static SortedSet doResistiveAnalysis(String spiceFile,
                                                String cellName,
                                                boolean printSubnets,
                                                boolean printAutoSubnets,
                                                String  subnetFile,
                                                boolean sortSubnets,
                                                int     sortBy,
                                                int     sortWorstNum,
                                                boolean worstByPercent)
    {
        //
        // Parse the extract file
        //
        LineUpdateCallback parsingCallback = null;
        if (!quiet) {
            System.err.print("Parsing "+spiceFile+"... ");
            parsingCallback = new LineUpdateCallback(10000,System.err);
            parsingCallback.update(0);
        }
        SpiceParser parser = new SpiceParser('.', parsingCallback);
        final LumpedRCGraph.Repository repos 
            = new LumpedRCGraph.Repository();
        parser.setAssuraRCXParsing();
        parser.beQuiet();
        try {
            parser.parseFile(spiceFile,repos);
        }
        catch (SpiceFileFormatException e) {
            System.err.println("Syntax error in "+spiceFile+":");
            System.err.println(" "+e.getMessage());
            System.exit(1);
        }
        catch (Exception e) {
            System.err.println("Error parsing "+spiceFile+":");
            System.err.println(" "+e.getMessage());
            System.exit(1);
        }
        if (!quiet) {
            parsingCallback.clearLast();
            System.err.println("[done]");
        }

        // 
        // Determine the cell name
        //
        if (cellName.length() == 0) cellName = lookupCell(repos);

        //
        // Get the LumpedRCGRaph for the extracted cell
        //
        final LumpedRCGraph circuit = (LumpedRCGraph)repos.getCell(cellName);
        if (circuit == null) {
            System.err.println("Couldn't find cell " + cellName);
            System.exit(1);
        }

        // 
        // Generate the statistics tracking object
        //
        NetStatistics stats = new NetStatistics(circuit,spiceFile);

        // 
        // Identify all resistive subnets in cellName.
        //
        LumpedGraphCallback lumpedCallback = null;
        if (!quiet) {
            System.err.print("Identifying resistive subnets... ");
            lumpedCallback = new LumpedGraphCallback(1000,System.err);
        }
        circuit.identifyNets(lumpedCallback);
        if (!quiet) {
            lumpedCallback.clearLast();
            System.err.println("[done]");
            lumpedCallback.setNumNets(circuit.getNets().size());
        }

        //
        // Identify the virtual driver nodes of all subnets
        //
        if (!quiet) {
            System.err.print("Identifying VDNs of resistive subnets... ");
            lumpedCallback.netAlgorithmUpdate(0);
        }
        if (debug) LumpedRCGraph.enableVdnDebug();
        Iterator it = circuit.getNetsIterator();
        int progress = 0;
        while (it.hasNext()) {
            ResistiveSubnet rsn = (ResistiveSubnet)it.next();
            rsn.identifyVDN();
            progress++;
            if (!quiet) lumpedCallback.netAlgorithmUpdate(progress);
        }
        circuit.clearMarks();
        if (!quiet) {
            lumpedCallback.clearLast();
            System.err.println("[done]");
        }

        //
        // Find the max R path on each resistive net, or the
        // one specified, and the wire cap along that path.
        //
        if (!quiet) {
            System.err.print("Finding max paths of all resistive "+
                             "subnets");
            if (sortSubnets) System.err.print(" (w/sort)");
            System.err.print("... ");
            lumpedCallback.netAlgorithmUpdate(0);
        }

        // 
        // Set the comparator for subnet ordering
        //
        Comparator c = ResistiveSubnet.newResComparator();
        if (sortBy == RC) 
            c = ResistiveSubnet.newResCapComparator();
        else if (sortBy == COUPLING)
            c = ResistiveSubnet.newCouplingComparator();
        TreeSet orderedSubnets = null;
        if (sortSubnets) orderedSubnets = new TreeSet(c);

        // 
        // Iterate through all subnets
        //
        it = circuit.getNetsIterator();
        progress = 0;
        while (it.hasNext()) {
            ResistiveSubnet rsn = (ResistiveSubnet)it.next();
            rsn.findMaxPathRes();
            stats.addNet(rsn);
            if (sortSubnets) orderedSubnets.add(rsn);
            progress++;
            if (!quiet) lumpedCallback.netAlgorithmUpdate(progress);
        }
        if (!quiet) {
            lumpedCallback.clearLast();
            System.err.println("[done]");
        }

        // 
        // Generate the cell report
        //
        stats.generateReport();

        // 
        // Pare down orderedSubnets if sortWorstNum > 0
        //
        if (sortWorstNum > 0 && sortSubnets) {
            int beginAt;
            if (worstByPercent)
                beginAt = orderedSubnets.size() -
                    (int)((float)sortWorstNum*orderedSubnets.size()/100);
            else
                beginAt = orderedSubnets.size() - sortWorstNum;

            it = orderedSubnets.iterator();
            for (int i=0; i < beginAt; i++) {
                Debug.assertTrue(it.hasNext());
                it.next();
                it.remove();
            }
        }

        //
        // Output subnet info
        //
        if (subnetFile.equals("")) {
            int i0 = spiceFile.lastIndexOf('/');
            Debug.assertTrue(i0 != spiceFile.length()-1);
            int i1 = spiceFile.lastIndexOf('.');
            if (i1 == -1) subnetFile = spiceFile.substring(i0+1) + ".subnets";
            else subnetFile = spiceFile.substring(i0+1,i1) + ".subnets";
        }
        if (printSubnets) {
            if (!sortSubnets) 
                printSubnets(circuit.getNetsIterator(),subnetFile);
            else
                printSubnets(orderedSubnets.iterator(),subnetFile);
        }
        if (printAutoSubnets) {
            if (!sortSubnets) 
                printAutoSubnets(circuit.getNetsIterator(),subnetFile);
            else
                printAutoSubnets(orderedSubnets.iterator(),subnetFile);
        }

        return orderedSubnets;
    }

    /*
    public static void analyzeResistiveSubnet()
    {
        System.err.print("Finding max resistive path of subnet "+
                         netName+"... ");
        it = circuit.getNetsIterator();
        boolean done = false;
        ResistiveSubnet rsn = null;
        while (it.hasNext() && !done) {
            rsn = (ResistiveSubnet)it.next();
            if (rsn.getVDN().sameResNet(netName)) {
                rsn.findMaxPathRes();
                done = true;
            }
        }
        System.err.println("[done]");
        if (rsn != null) {
        String maxR  = NumberFormatter.format(rsn.getMaxPathRes(),4);
        String gateC = NumberFormatter.format(rsn.getGateCap(),4);
        String wireC = NumberFormatter.format(rsn.getWireCap(),4);
        String goodC = NumberFormatter.format(rsn.getGoodCap(),4);
        String maxRC = NumberFormatter.format(rsn.getMaxRC(),4);
        System.out.println(netName+": VDN="
                           +rsn.getVDN().name.getAsString('.')
                           +" MaxR="+maxR+" Cw="+wireC+" Cg="+goodC
                           +" Cl="+gateC+" maxRC="+maxRC);
        }
    }
    */

    /*****************************************************************
     * Examines a spice file for a specific set of nets.  Creates
     * aspice CAST test environments for the nets in the directory
     * spiceFileBase (spiceFile without the ".spice" extension.)
     * Suggested use of these .cast files are as follows:
     * <pre>
     *      cflat -ADspice testfile.cast &gt; testnet.asp
     *      aspice testnet -traceAllR
     *      tpp -skew testnet testnet
     *      tpp -slew testnet testnet
     * </pre>
     *****************************************************************/
    public static void examineNets(String spiceFile,
                                   String cellName, 
                                   Set cullNets)
    {
        //
        // Parse the extract file again, this time culling
        // for the specified node.
        //
        LineUpdateCallback parsingCallback = null;
        if (!quiet) {
            System.err.print("Parsing "+spiceFile+" w/ culling... ");
            parsingCallback = new LineUpdateCallback(10000,System.err);
            parsingCallback.update(0);
        }
        SpiceParser parser = new SpiceParser('.', parsingCallback);
        final CullingCircuitGraph.Repository cullRepos
            = new CullingCircuitGraph.Repository(cullNets);
        parser.setAssuraRCXParsing();
        parser.beQuiet();
        try {
            parser.parseFile(spiceFile,cullRepos);
        }
        catch (Exception e) {
            System.err.println("Error parsing spice file "+spiceFile+":");
            System.err.println(" "+e.getMessage());
            System.exit(1);
        }
        if (!quiet) {
            parsingCallback.clearLast();
            System.err.println("[done]");
        }
        // 
        // Determine the cell name
        //
        if (cellName.length() == 0) cellName = lookupCell(cullRepos);

        //
        // Get the CullingCircuitGraph for the extracted cell
        //
        final CullingCircuitGraph culledCircuit = 
            (CullingCircuitGraph)cullRepos.getCell(cellName);
        if (culledCircuit == null) {
            System.err.println("Couldn't find cell " + cellName);
            System.exit(1);
        }

        //
        // Set up Cast test environments for each culled subnet
        //
        CastTestEnvironment envFile = new CastTestEnvironment(confFile);
        String dirName = createTestDirectory(spiceFile);
        if (!quiet && !debug) System.err.print("Building cast environments "+
                                               "for "+cullNets.size()+
                                               " culled subnets... ");
        LumpedGraphCallback callback = null;
        if (!quiet) {
            callback = new LumpedGraphCallback(100,System.err);
            callback.setNumNets(cullNets.size());
            callback.netAlgorithmUpdate(0);
        }
        Iterator it = cullNets.iterator();
        int progress = 0;
        while (it.hasNext()) {
            HierName netName = (HierName) it.next();

            //
            // Get the specified TestSubnet
            //
            if (debug) System.err.print("Building the TestSubnet for subnet "+
                                        netName+"... ");
            TestSubnet testSubnet = culledCircuit.getTestSubnet(netName);
            if (debug) System.err.println("[done]");
            if (testSubnet == null) {
                System.err.println("Warning: Could not build subnet '"+
                                   netName+"'.");
                continue;
            }

            // 
            // Initialize the cast test environment file.
            //
            testSubnet.setCastTestEnvironment(envFile);

            if (debug) System.err.print("Finding conjunctive driver paths... ");
            testSubnet.findConjunctiveDriverPaths();
            if (debug) System.err.println("[done]");

            // 
            // Write parasitic capacitances to dir/SPICEFILE.cap
            // 
            String capFile = 
                dirName + "/" + netName.getResistiveSubnetString() + ".cap";
            if (debug) System.err.println("Dumping parasitic capacitances to "+
                                          capFile+"... ");
            try {
                FileOutputStream capStream = new FileOutputStream(capFile);
                testSubnet.printParasiticCapToStream(capStream);
            }
            catch (FileNotFoundException e) {
                System.err.println("Couldn't write to "+capFile+". Skipping.");
            }
            
            if (debug) {
                System.err.println("Driver conjunctive pull-up paths "+
                                          "for subnet "+netName+":");
                Iterator jt = testSubnet.getConjunctivePullUpPaths();
                while (jt.hasNext()) {
                    ConjunctivePath p = (ConjunctivePath) jt.next();
                    p.printToStream(System.out);
                }
                System.err.println("Driver conjunctive pull-down paths for "+
                                   "subnet "+netName+":");
                jt = testSubnet.getConjunctivePullDownPaths();
                while (jt.hasNext()) {
                    ConjunctivePath p = (ConjunctivePath) jt.next();
                    p.printToStream(System.out);
                }
                System.err.print("Feedback subnets: ");
                jt = testSubnet.getFeedbackSubnets();
                while (it.hasNext()) {
                    HierName n = (HierName) jt.next();
                    System.err.print(n.getAsString('.') + " ");
                }
                System.err.print("\n");
            }

            // 
            // Generate the cast test environment file.
            //
            String castTestEnvFile = 
                dirName + "/" + netName.getResistiveSubnetString() + ".cast";
            if (debug) System.err.print("Writing cast test environment to "+
                                        castTestEnvFile+"... ");
            try {
                testSubnet.writeCastEnvironmentFile(castTestEnvFile);
            }
            catch (IOException e) {
                System.err.println("Error writing cast test file "+
                                   castTestEnvFile+":");
                System.err.println(" "+e.getMessage());
                System.err.println(" Skipping.");
            }
            if (debug) System.err.println("[done]");
           
            progress++;
            if (!quiet) callback.netAlgorithmUpdate(progress);
        }
        if (!quiet && !debug) {
            callback.clearLast();
            System.err.println("[done]");
        }
    }

    /*****************************************************************
     * Creates an output directory for placing subnet test cast
     * files, based on the spice extract file name.  Usually this
     * will be the base name of the spice file (".spice" suffix
     * stripped off), created in the working directory.  If that
     * directory can't be created, '.' is returned instead.
     *****************************************************************/
    static String createTestDirectory(final String spiceFilename) {
        int i0 = spiceFilename.lastIndexOf('/');
        if (i0 == -1) i0 = 0;
        else i0++;
        int i1 = spiceFilename.lastIndexOf('.');
        String dirName = ".";
        File spDir = null;
        if (i1 != -1) spDir = new File(spiceFilename.substring(i0,i1));
        else spDir = new File(spiceFilename.substring(i0)+".subnets");
        if (!spDir.isDirectory() && !spDir.exists()) {
            if (!spDir.mkdir()) {
                System.err.println("Warning: Could not create directory '"+
                                   spDir.getName()+"'. Using '.'.");
            }
            else dirName = spDir.getName();
        }
        else if (spDir.isDirectory()) dirName = spDir.getName();
        return dirName;
    }

    public static void printAutoSubnets(Iterator subnetIterator, 
                                        String fileName) 
    {
        PrintWriter outWriter = null;
        try {
            // 
            // Set output writer
            //
            if (fileName.equals("")) 
                outWriter = new PrintWriter(System.out);
            else
                outWriter = new
                    PrintWriter(new BufferedWriter(new FileWriter(fileName)));
            //
            // print out all resistive subnets in a format
            // suitable for parsing by auto.
            //
            while (subnetIterator.hasNext()) {
                ResistiveSubnet rsn = (ResistiveSubnet)subnetIterator.next();
                String wireC = NumberFormatter.format(rsn.getWireCap(),4);
                String goodC = NumberFormatter.format(rsn.getGoodCap(),4);
                String gateC = NumberFormatter.format(rsn.getGateCap(),4);
                String maxR  = NumberFormatter.format(rsn.getMaxPathRes(),4);
                String maxRC = NumberFormatter.format(rsn.getMaxRC(),4);
                outWriter.print(rsn.getCanonicalNameAsString()+
                                ": Cw="+wireC+" Cg="+goodC+" Cl="+gateC+
                                " R="+maxR+" RC="+maxRC);
                if (rsn.hasCycles()) outWriter.println("  // HAS CYCLES");
                else outWriter.println("");
            }
        }
        catch (Exception e) {
            e.printStackTrace();
            System.err.println(e + ": " + e.getMessage());
            System.exit(1);
        }
        finally {
            if (outWriter != null) outWriter.close();
        }
    }

    public static void printSubnets(Iterator subnetIterator, String fileName)
    {
        PrintWriter outWriter = null;
        try {
            // 
            // Set output writer
            //
            if (fileName.equals("")) 
                outWriter = new PrintWriter(System.out);
            else
                outWriter = new
                    PrintWriter(new BufferedWriter(new FileWriter(fileName)));
            //
            // print out all resistive subnets in a fairly 
            // verbose format.
            //
            outWriter.println("--------------------------------------------");
            outWriter.println("RESISTIVE SUBNETS IN CIRCUIT:");
            while (subnetIterator.hasNext()) {
                ResistiveSubnet rsn = (ResistiveSubnet)subnetIterator.next();
                String wireC = NumberFormatter.format(rsn.getWireCap(),4);
                String goodC = NumberFormatter.format(rsn.getGoodCap(),4);
                String gateC = NumberFormatter.format(rsn.getGateCap(),4);
                outWriter.println(rsn.getCanonicalNameAsString()+
                                  " (Cw="+wireC+" Cgood="+goodC+
                                  " Cg="+gateC+")");
                Iterator jt = rsn.getDriverNodes();
                boolean found_vdn = false;
                if (jt.hasNext()) {
                    found_vdn = true;
                    outWriter.print("   Driven at: ");
                    while (jt.hasNext()) {
                        AbstractNode n = (AbstractNode)jt.next();
                        if (n == rsn.getVDN()) outWriter.print("<");
                        outWriter.print(n.name.getAsString('.'));
                        if (n == rsn.getVDN()) outWriter.print(">");
                        outWriter.print(" ");
                    }
                    outWriter.print("\n");
                }
                jt = rsn.getGateNodes();
                if (jt.hasNext()) {
                    outWriter.print("   Drives at: ");
                    while (jt.hasNext())
                        outWriter.print(((AbstractNode)jt.next()).
                                        name.getAsString('.')+" ");
                    outWriter.print("\n");
                }
                if (found_vdn) {
                    String vdnR = NumberFormatter.format(rsn.getVdnRes(),4);
                    String vdnC = NumberFormatter.format(rsn.getVdnCap(),4);
                    outWriter.println("   VDN: " +
                                      rsn.getVDN().name.getAsString('.') +
                                      " R="+vdnR+" C="+vdnC);
                }
                String maxR  = NumberFormatter.format(rsn.getMaxPathRes(),4);
                String maxRC = NumberFormatter.format(rsn.getMaxRC(),4);
                outWriter.println("   Max gate path: "+" R="+maxR+
                                  " RC="+maxRC);
                if (rsn.hasCycles()) outWriter.println("   HAS CYCLES.");
            }
        }
        catch (Exception e) {
            e.printStackTrace();
            System.err.println(e + ": " + e.getMessage());
            System.exit(1);
        }
        finally {
            if (outWriter != null) outWriter.close();
        }
    }
}
