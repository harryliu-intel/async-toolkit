/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.csp2tt;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import com.avlsi.cast.CastFile;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cell.CellInterface;
import com.avlsi.csp.ast.CSPProgram;
import com.avlsi.csp.csp2java.runtime.CspSnoopingInterface;
import com.avlsi.csp.grammar.ParsePosition;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.file.common.HierName;
import com.avlsi.io.CopyingOutputStream;
import com.avlsi.io.FileSearchPath;
import com.avlsi.tools.cosim.ChannelDictionary;
import com.avlsi.tools.cosim.ChannelFactoryInterface;
import com.avlsi.tools.cosim.ChannelTimingInfo;
import com.avlsi.tools.cosim.NodeFactoryInterface;
import com.avlsi.tools.dsim.ExceptionPrettyPrinter;
import com.avlsi.tools.dsim.Node;
import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.ChannelOutput;
import com.avlsi.tools.tsim.WideNode;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.container.Triplet;
import com.avlsi.util.text.StringUtil;

/**
 * Generates truth table from csp program.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class Csp2TT {

    private final CellInterface cell;

    private final PrintWriter out;

    /**
     * Class constructor.
     * 
     * <pre><jml>
     *   public normal_behavior
     *     requires cell != null;
     *     requires out != null;
     * </jml></pre>
     **/
    public Csp2TT(CellInterface cell, PrintWriter out) {
        this.cell = cell;
        this.out = out;
    }

    public void generateTruthTable() throws IOException {
        final SnoopingChannelFactory chanFactory =
            new SnoopingChannelFactory();
        final DeathNodeFactory nodeFactory = new DeathNodeFactory();
        final ChannelDictionary channelDict =
            cell.getCSPCoSimInfo().createChannels(null, cell, chanFactory,
                                                  nodeFactory);

        final CSPSnooper cspSnooper =
            new CSPSnooper(chanFactory.getInputChannels(),
                           chanFactory.getOutputChannels());

        try {
            cell.buildCSPClass(HierName.makeHierName("x"), channelDict, -1,
                               false, cspSnooper);
        } catch (Exception e) {
            System.err.println("trouble building class");
            e.printStackTrace();
            System.exit(2);
        }
    }

    private final class CSPSnooper implements CspSnoopingInterface {
        private final List/*<PlayingChannelInput>*/ inputChannels;
        private final List/*<RecordingChannelOutput>*/ outputChannels;
        private BigInteger index = null;
        private BigInteger numInputValues = null;
        private final HashSet/*<Triplet<String,String,Integer>>*/ warnings =
            new HashSet/*<Triplit<String,String,Integer>>*/();

        private CSPSnooper(final List/*<PlayingChannelInput>*/
                               inputChannels,
                           final List/*<RecordingChannelOutput>*/
                               outputChannels) {
            this.inputChannels = inputChannels;
            this.outputChannels = outputChannels;
        }

        /**
         * <pre><jml>
         *   private normal_behavior
         *     requires channelName != null;
         * </jml></pre>
         **/
        private String mungeChannelName(final String channelName) {
            // replace ][ with ,
            return StringUtil.replaceSubstring(channelName, "][", ",");
        }

        /**
         * <pre><jml>
         *   private normal_behavior
         *     requires channelName != null;
         *     requires radix >= 2;
         *     requires width >= 1;
         * </jml></pre>
         **/
        private void printChannelHeader(final String channelName,
                                        final int radix,
                                        final int width) {
            if (width > 1) {
                for (int i = 0; i < width; ++i)
                    out.print(mungeChannelName(channelName +
                                               '[' + i + ']') +
                              ':' + radix + ' ');
            } else {
                out.print(channelName + ':' + radix + ' ');
            }
        }

        /**
         * <pre><jml>
         *   private normal_behavior
         *     requires radix >= 2;
         *     requires width >= 1;
         * </jml></pre>
         **/
        private void printChannelValue(final BigInteger value,
                                       final int radix,
                                       final int width,
                                       final String suffix) {
            if (value == null) {
                for (int i = 0; i < width; ++i)
                    out.print("- ");
            } else {
                BigInteger v = value;
                final BigInteger r = BigInteger.valueOf(radix);
                for (int i = 0; i < width; ++i) {
                    out.print(v.mod(r).toString() + suffix + ' ');
                    v = v.divide(r);
                }
            }
        }

        /**
         * Prints a warning that num ops were done on name, but only
         * if it hasn't already printed this warning before.
         * @param  name   the name of the channel
         * @param  op     "send" or "receive"
         * @param  num    number of sends or receives
         */
        private void printWarning(String name, String op, int num) {
            Triplet/*<String,String,Integer>*/ warning =
                new Triplet/*<String,String,Integer>*/(name, op,
                        new Integer(num));
            if (!warnings.contains(warning)) {
                warnings.add(warning);
                System.err.println("warning: " + num + " " +
                                   op + "s on " + name);
            }
        }

        public void onStart() {
            numInputValues = BigInteger.ONE;
            index = BigInteger.valueOf(-1);

            // input channels
            for (Iterator i = inputChannels.iterator(); i.hasNext(); ) {
                final PlayingChannelInput chan =
                    (PlayingChannelInput) i.next();
                final BigInteger numValues = chan.getNumPossibleValues();

                // ouput channel name(s) for header
                printChannelHeader(chan.getName(), chan.getRadix(),
                                   chan.getWidth());

                // update possible values
                numInputValues = numInputValues.multiply(numValues);
                chan.startNewCycle(BigInteger.ZERO);
            }

            // input / output separator
            out.print("=> ");

            // output channels
            for (Iterator i = outputChannels.iterator(); i.hasNext(); ) {
                final RecordingChannelOutput chan =
                    (RecordingChannelOutput) i.next();

                printChannelHeader(chan.getName(), chan.getRadix(),
                                   chan.getWidth());
            }

            out.println();
            out.flush();
        }

        public void onOutermostLoopStart() {
            if (index.equals(BigInteger.valueOf(-1))) {
                // check that channels have not been read or written
                for (Iterator i = inputChannels.iterator(); i.hasNext(); ) {
                    final PlayingChannelInput chan =
                        (PlayingChannelInput) i.next();
                    if (chan.getNumReceives() != 0)
                        System.err.println("warning: " + chan.getName() +
                                           " read before main loop");
                }
                for (Iterator i = outputChannels.iterator(); i.hasNext(); ) {
                    final RecordingChannelOutput chan =
                        (RecordingChannelOutput) i.next();
                    if (chan.getNumSends() != 0) {
                        System.err.println("warning: " + chan.getName() +
                                           " written before main loop");
                        chan.startNewCycle();
                    }
                }
            } else {
                // input channels
                for (Iterator i = inputChannels.iterator(); i.hasNext(); ) {
                    final PlayingChannelInput chan =
                        (PlayingChannelInput) i.next();
                    
                    // check that there was at most one receive
                    int numReceives = chan.getNumReceives();
                    if (numReceives > 1)
                        printWarning(chan.getName(), "receive", numReceives);

                    printChannelValue(chan.getValue(), chan.getRadix(),
                                      chan.getWidth(),
                                      (numReceives == 0 ? "n" : ""));
                }

                out.print("=> ");

                // output channels
                for (Iterator i = outputChannels.iterator(); i.hasNext(); ) {
                    final RecordingChannelOutput chan =
                        (RecordingChannelOutput) i.next();

                    // check that there was at most one send
                    int numSends = chan.getNumSends();
                    if (numSends > 1)
                        printWarning(chan.getName(), "send", numSends);

                    printChannelValue(chan.getValue(), chan.getRadix(),
                                      chan.getWidth(), "");

                    chan.startNewCycle();
                }

                out.println();
            }

            index = index.add(BigInteger.ONE);
            BigInteger tempVal = index;

            if (index.equals(numInputValues)) {
                // we are done
                out.flush();
                System.exit(0);
            } else {
                // input channels
                for (Iterator i = inputChannels.iterator(); i.hasNext(); ) {
                    final PlayingChannelInput chan =
                        (PlayingChannelInput) i.next();
                    final BigInteger numValues = chan.getNumPossibleValues();

                    // set new channel value
                    chan.startNewCycle(tempVal.mod(numValues));
                    tempVal = tempVal.divide(numValues);
                }
            }

            out.flush();
        }

        public void onEnd() {
            // This shouldn't happen
            System.err.println("onEnd");
            out.flush();
        }

        public void onDeadlock(String message, String whereAmI) {
            System.err.println("error: Deadlock detected at " + whereAmI +
                               ": " + message);
            out.flush();
            System.exit(3);
        }

        public void onArbitrate(String message, String whereAmI) {
            System.err.println("error: Arbitration detected at " + whereAmI +
                               ": " + message);
            out.flush();
            System.exit(3);
        }

        public void onOutOfBoundsAccess(String message, String whereAmI) {
            System.err.println("error: Array out of bounds access at " +
                               whereAmI + ": " + message);
            out.flush();
            System.exit(3);
        }
    }

    private static final class SnoopingChannelFactory
        implements ChannelFactoryInterface {

        private final List/*<PlayingChannelInput>*/ inputChannels =
            new ArrayList/*<PlayingChannelInput>*/();
        private final List/*<RecordingChannelOutput>*/ outputChannels =
            new ArrayList/*<RecordingChannelOutput>*/();

        public ChannelInput makeInputChannel(final String name,
                                             final int radix,
                                             final int width,
                                             final ChannelTimingInfo cti) {
            final ChannelInput chan =
                new PlayingChannelInput(name, radix, width);
            inputChannels.add(chan);
            return chan;
        }

        public ChannelOutput makeOutputChannel(final String name,
                                               final int radix,
                                               final int width,
                                               final ChannelTimingInfo cti) {
            final ChannelOutput chan =
                new RecordingChannelOutput(name, radix, width);
            outputChannels.add(chan);
            return chan;
        }

        private List/*<PlayingChannelInput>*/ getInputChannels() {
            return inputChannels;
        }

        private List/*<RecordingChannelOutput>*/ getOutputChannels() {
            return outputChannels;
        }
    }

    private static final class DeathNodeFactory
        implements NodeFactoryInterface {
        public WideNode makeWideNode(final String name, int width,
                                     int direction, boolean isArrayed,
                                     boolean readOnly) {
            return new WideNode() {
                private void die() {
                    System.err.println("trouble building class");
                    System.err.println(
                            "error: cannot handle node-level operations " +
                            "found on node " + name + ".  Did you really " +
                            "want to process CSP with node-level " +
                            "operations?  See bug 6928.");
                    System.exit(2);
                }
                public BigInteger getValue() { die(); return null; }
                public Node[] getNodes() { return new Node[0]; }
                public boolean stable() { die(); return false; }
                public void setValue(final int[] indices,
                                     final byte[] values)  { die(); }
                public void setValue(final int begin,
                                     final int end,
                                     final BigInteger val) { die(); }
                public void setValue(final BigInteger val) { die(); }
                public boolean isArrayed() { die(); return false; }
            };
        }
    }

    private static void usage(int exitStatus) {
        System.out.println("Usage: csp2tt --cast-path=cast_path" +
                                        " [--cell=cell_name]" +
                                        " [--version]");
        System.exit(exitStatus);
    }

    public static void main(String[] args) throws Exception {

        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
	final CommandLineArgs argsWithConfigs =
	    new CommandLineArgsWithConfigFiles(parsedArgs); 
	final CommandLineArgs cachedArgs = 
	    new CachingCommandLineArgs(argsWithConfigs);
	final CommandLineArgs theArgs = cachedArgs;
	
        if (theArgs.argExists("version")) {
            System.out.println(
                com.avlsi.util.debug.VersionInfo.getVersionString(
                    Csp2TT.class));
        }
	
	final FileSearchPath castFSP =
            new FileSearchPath(theArgs.getArgValue("cast-path", "."));

        final String castCellName = theArgs.getArgValue("cell", null);

        if (castCellName == null ||
            theArgs.nonParsedArgumentsIterator().hasNext())
            usage(1);

        final CastFileParser cfp = new CastFileParser(castFSP, "2");
        CellInterface cell = null;
        try {
            cell = cfp.getFullyQualifiedCell(castCellName);
        } catch (CastSemanticException e) {
            ExceptionPrettyPrinter.printException(e, System.err);
            System.exit(99);
        }

        final PrintWriter systemOutWriter =
            new PrintWriter(new OutputStreamWriter(System.out));
        
        /* Now that we've captured the real stdout in systemOutWriter,
         * redirect any other use of System.out (such as CSP print
         * statements) so that they don't contaminate the truth
         * table output.  (Fixes bug 4487.) */

        OutputStream[] none = new OutputStream[0];
        System.setOut(new PrintStream(new CopyingOutputStream(none)));

        new Csp2TT(cell, systemOutWriter).generateTruthTable();
    }
}
