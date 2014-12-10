package com.avlsi.tools.slacker;

import java.io.*;
import java.util.*;
 
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cast2.util.StandardParsingOption;
import com.avlsi.cell.CellInterface;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.CommandLineArgsUtil;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.container.MultiSet;
import com.avlsi.io.FileUtil;
import com.avlsi.cast.CastFile;
import com.avlsi.cast.CastFileParser;
import com.avlsi.io.FileSearchPath;
import java.text.SimpleDateFormat;
import java.util.Date;

/** slack matching tool */
public class Slacker {

    /** Path to glpsol executable */
    final String execSolve;

    /** Directory for intermediate files */
    final String runDirectory;

    /** Ignore existing slack */
    final boolean ignoreExistingSlack;

    /** Force top cell to be slacker_leaf=false, slacker_alignment=0 */
    final Boolean checkLeaf;

    /** All cell Types */
    final ArrayList types = new ArrayList();

    /**
     * Estimated area cost per slack; <code>costs[N]</code> contains the cost
     * for a channel with N+1 rails.
     **/
    final double[] costs;

    /** Constructor */
    Slacker(String execSolve, String runDirectory,
            double cycleSlack, double globalFreeSlack, double hierWeight,
            boolean emitLeafResults, boolean ignoreExistingSlack,
            boolean allowNonInteger, boolean reportZeroBuffers,
            boolean reportSubcellTimes, boolean checkOnly, Boolean checkLeaf,
            double costFreeSlack, double [] costs) {
        this.execSolve = execSolve;
        this.runDirectory = runDirectory;
        this.costs = costs;
        this.ignoreExistingSlack = ignoreExistingSlack;
        this.checkLeaf = checkLeaf;
        Type.emitLeafResults = emitLeafResults;
        Type.hierWeight      = hierWeight;
        Type.cycleSlack      = cycleSlack;
        Type.globalFreeSlack = globalFreeSlack;
        Type.allowNonInteger = allowNonInteger;
        Type.reportZeroBuffers = reportZeroBuffers;
        Type.reportSubcellTimes = reportSubcellTimes;
        Type.checkOnly = checkOnly;
        Type.costFreeSlack = costFreeSlack;
    }

    /** Add this type to types list */
    public void addType(Type type) {
        types.add(type);
    }

    /** Debugging printout */
    public void print() {
        for (Iterator i = types.iterator(); i.hasNext(); ) {
            Type type = (Type) i.next();
            type.print();
        }
    }

    /** Slack match a CAST cell */
    public void castCell(final CellInterface cell) {
        final Cast cast = new Cast(this);
        // if neither --check-leaf or --report-leaf is specified, imply check
        // leaf if a slacker_leaf but not slacker_primitive
        if (checkLeaf == Boolean.TRUE ||
            checkLeaf == null &&
            Boolean.TRUE.equals(
                DirectiveUtils.getTopLevelDirective(
                    cell, DirectiveConstants.SLACKER_LEAF)) &&
            !Boolean.TRUE.equals(
                DirectiveUtils.getTopLevelDirective(
                    cell, DirectiveConstants.SLACKER_PRIMITIVE))) {
            cast.topCell = cell;
        }
        Type top = cast.process(cell);
        top.slackMatch(execSolve,runDirectory);
        for (Iterator i = types.iterator(); i.hasNext(); ) {
            Type type = (Type) i.next();
            type.slackResults(type==top);
        }
    }

    /**
     * Given the number of rails, return the estimated area cost.
     **/
    public double getCost(final int rails) {
        if (rails < 0) return 0; // unsupported channel type, no cost
        if (rails <= costs.length) return costs[rails - 1];
        return rails + 1; // default is data rails + enable rail
    }

    /** Usage banner */
    static public void usage() {
        System.err.println("USAGE: slacker\n" +
                           "  --cell=FQCN\n" +
                           "  --cast-path=path\n" + 
                           "  [--run-dir=dir]\n" +
                           "  [--verbose]\n" + 
                           "  [--leaves]\n" + 
                           "  [--integer-slack-only]\n" +
                           "  [--enable-inlining]\n" +
                           "  [--ignore-existing-slack]\n" +
                           "  [--report-zero-buffers]\n" +
                           "  [--report-subcell-times]\n" +
                           "  [--check-only]\n" +
                           "  [--check-leaf]\n" +
                           "  [--report-leaf]\n" +
                           "  [--cycle-slack=stages_of_slack_per_token]\n" +
                           "  [--global-free-slack=leftover_slack_at_slower_cycle_slack]\n" +
                           "  [--hierarchy-weight=cost_weight_per_depth]\n" +
                           "  [--costs=<val>,<val>,<val>...]\n" +
                           "  [--cost-free-slack=<val>]\n" +
                           "  [--glpsol=glpsol]\n" +
                           "  [--keep-temp-files]");
        System.exit(1);
    }

    /** Slack matching tool command line interface */
    static public void main(String [] args) throws Exception {

        // get command line args
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
	final CommandLineArgs argsWithConfigs =
	    new CommandLineArgsWithConfigFiles(parsedArgs); 
	final CommandLineArgs cachedArgs = 
	    new CachingCommandLineArgs(argsWithConfigs);
	final CommandLineArgs theArgs = cachedArgs;
        
        // get options
        boolean keepTempFiles = theArgs.argExists("keep-temp-files");
        boolean reportZeroBuffers = theArgs.argExists("report-zero-buffers");
        boolean reportSubcellTimes = theArgs.argExists("report-subcell-times");
        boolean checkOnly = theArgs.argExists("check-only");
        boolean checkLeaf = theArgs.argExists("check-leaf");
        boolean reportLeaf = theArgs.argExists("report-leaf");
        boolean emitLeafResults = theArgs.argExists("leaves");
        boolean verbose = theArgs.argExists("verbose");
        boolean allowNonInteger = !theArgs.argExists("integer-slack-only");
        boolean enableInlining = theArgs.argExists("enable-inlining");
        boolean ignoreExistingSlack = theArgs.argExists("ignore-existing-slack");
        String execSolve = theArgs.getArgValue("glpsol","glpsol --cpxlp");
        String runDirectory = theArgs.getArgValue("run-dir",".");
        String hierWeightStr = theArgs.getArgValue("hierarchy-weight",null);
        double hierWeight = 1.01;
        if (hierWeightStr!=null) hierWeight = Double.parseDouble(hierWeightStr);
        String costFreeSlackStr = theArgs.getArgValue("cost-free-slack",null);
        double costFreeSlack = 0.01;
        if (costFreeSlackStr!=null) costFreeSlack = Double.parseDouble(costFreeSlackStr);
        final FileSearchPath castPath =
            new FileSearchPath(theArgs.getArgValue( "cast-path", "." ));
        String cycleSlackStr = theArgs.getArgValue("cycle-slack",null);
        double cycleSlack = 9;
        if (cycleSlackStr!=null) cycleSlack = Double.parseDouble(cycleSlackStr);
        String globalFreeSlackStr = theArgs.getArgValue("global-free-slack",null);
        double globalFreeSlack = 0;
        if (globalFreeSlackStr!=null) globalFreeSlack = Double.parseDouble(globalFreeSlackStr);
        final double[] costs =
            CommandLineArgsUtil.getDoubleArgList(theArgs, "costs");
        String castCellName = theArgs.getArgValue("cell",null);
        final String logName = theArgs.getArgValue("log-file", null);
        final PrintStream log =
            logName == null ? null : new PrintStream(logName);

        if (castCellName==null) usage();

        final Boolean leafCheck;
        if (checkLeaf && reportLeaf) {
            System.err.println("ERROR: Cannot specify both --check-leaf and --report-leaf");
            System.exit(1);
            leafCheck = null;
        } else {
            leafCheck = checkLeaf ? Boolean.TRUE
                                  : reportLeaf ? Boolean.FALSE
                                               : null;
        }

        System.err.println("NOTE: Parsing started  at " +
                           new SimpleDateFormat("HH:mm:ss MM/dd/yyyy").format(new Date()));

        // create scratch subdirectory
        runDirectory += "/" + castCellName;
        File runFile = new File(runDirectory);
        runFile.mkdirs();

        // create slacker object
        Slacker slacker = new Slacker(execSolve,runDirectory,cycleSlack,
                                      globalFreeSlack,hierWeight,
                                      emitLeafResults,ignoreExistingSlack,
                                      allowNonInteger,reportZeroBuffers,
                                      reportSubcellTimes,checkOnly,leafCheck,
                                      costFreeSlack,costs);

        // parse CAST
        final StandardParsingOption spo = enableInlining ?
            new StandardParsingOption(theArgs) :
            new StandardParsingOption(theArgs) {
                public boolean processInline(CellInterface cell) {
                    return !Cast.isLeaf(cell);
                }
            };
        final CastFileParser cfp =
            new CastFileParser(castPath, null, verbose, null,spo);
        CellInterface cell = null;
        try {
            cell = cfp.getFullyQualifiedCell(castCellName);
        } catch (CastSemanticException e) {
            com.avlsi.tools.dsim.ExceptionPrettyPrinter.printException(
                e, System.err);
            System.exit(1);
        }

        System.err.println("NOTE: Slacker started  at " +
                           new SimpleDateFormat("HH:mm:ss MM/dd/yyyy").format(new Date()));
        PrintStream oldOut = null;
        PrintStream oldErr = null;
        try {
            if (log != null) {
                oldOut = System.out; System.setOut(log);
                oldErr = System.err; System.setErr(log);
            }

            // slack match
            slacker.castCell(cell);
            
            // debugging printout
            if (verbose) {
                System.out.println("/******* Problem *******/");
                slacker.print();
            }

            if (log != null) {
                log.close();
            }
        } finally {
            if (oldOut != null) System.setOut(oldOut);
            if (oldErr != null) System.setErr(oldErr);
        }
        System.err.println("NOTE: Slacker finished at " +
                           new SimpleDateFormat("HH:mm:ss MM/dd/yyyy").format(new Date()));
        if (!keepTempFiles) FileUtil.recursiveDelete(runFile);
    }
}
