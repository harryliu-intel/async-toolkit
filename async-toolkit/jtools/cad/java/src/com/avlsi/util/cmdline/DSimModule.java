/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

// vim:sw=4:expandtab:ai:cin:ts=4

package com.avlsi.util.cmdline;

//import java.util.Hashtable;
import java.lang.reflect.Field;

import java.math.BigInteger;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringReader;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Iterator;
import java.util.Vector;
import java.util.Enumeration;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import antlr.RecognitionException;
import antlr.TokenStreamException;

import com.avlsi.cell.NoSuchEnvironmentException;
import com.avlsi.tools.cosim.spec.CoSim;
import com.avlsi.tools.cosim.spec.CoSimLexer;
import com.avlsi.tools.cosim.spec.CoSimParser;
import com.avlsi.tools.cosim.spec.DuplicateInstanceSpecException;
import com.avlsi.tools.cosim.spec.ExtraInstanceSpecException;
import com.avlsi.tools.cosim.spec.HierarchyDepthException;
import com.avlsi.tools.cosim.spec.NoSuchInstanceException;
import com.avlsi.tools.dsim.CutoffChecker;
import com.avlsi.tools.dsim.DSim;
import com.avlsi.tools.dsim.DigitalScheduler;
import com.avlsi.tools.dsim.NoBehaviorFoundException;
import com.avlsi.tools.dsim.NoSuchCellException;
import com.avlsi.tools.dsim.Node;
import com.avlsi.tools.dsim.Rule;
import com.avlsi.tools.dsim.DSimUtil;
import com.avlsi.tools.dsim.ExceptionPrettyPrinter;
import com.avlsi.tools.dsim.RunStaticTiming;
import com.avlsi.tools.dsim.RunSlewChecks;
import com.avlsi.tools.tsim.AbstractDevice;
import com.avlsi.tools.sigscan.DebugOpts;
import com.avlsi.tools.sigscan.Sigscan;
import com.avlsi.tools.sigscan.SigscanException;
import com.avlsi.tools.tsim.AbstractDevice;
import com.avlsi.tools.tsim.AccurateWait;
import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.ChannelOutput;
import com.avlsi.tools.tsim.ChannelStatus;
import com.avlsi.tools.tsim.Wait;
import com.avlsi.tools.tsim.WaitFactory;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;

import com.avlsi.io.FileSearchPath;
import com.avlsi.io.SearchPath;

import com.avlsi.util.container.NaturalOrderComparator;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.StringContainerIterator;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgFormatException;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.CommandLineArgsUtil;
import com.avlsi.util.cmdlineargs.InvalidCommandLineArgException;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;

import com.avlsi.util.classloader.ConfigurableClassLoader;
import com.avlsi.util.exception.ExceptionUtils;
import com.avlsi.util.text.StringUtil;
import com.avlsi.util.text.PrintfFormat;
import com.avlsi.util.text.NaturalStringComparator;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.container.FilteringIterator;
import com.avlsi.util.container.IterableIterator;
import com.avlsi.util.functions.BinaryAction;
import com.avlsi.util.functions.BinaryPredicate;
import com.avlsi.util.functions.NumericPredicate;
import com.avlsi.util.functions.UnaryPredicate;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast.CastSyntaxException;

import com.avlsi.csp.coverage.Monitor;
import com.avlsi.csp.coverage.ParseException;
import com.avlsi.csp.csp2java.runtime.CspArray;
import com.avlsi.csp.csp2java.runtime.CspBoolean;
import com.avlsi.csp.csp2java.runtime.CspInteger;
import com.avlsi.csp.csp2java.runtime.CspRuntimeAbstractDevice;
import com.avlsi.csp.csp2java.runtime.CspStructure;
import com.avlsi.csp.csp2java.runtime.CspValue;
import com.avlsi.csp.csp2java.runtime.StackFrame;

/** Interface between the command line interpreter and DSim simulator */
public class DSimModule extends CmdLine {
    /** The underlying simulation object that does most of the work. **/
    DSim dsim = null;

    /** Which version of cast are we using? **/
    String castVersion = null;
    
    /** Utility DSim functionality wrapper **/
    DSimUtil utils;

    /** Print out exception stack traces? **/
    boolean debug = false;

    /** Number of ASTA threads **/
    int astaThreads = 2;

    private RunStaticTiming sta = null;
    private RunSlewChecks slint = null;

    /** Interrupt handler. Catches Ctrl-C and tells the simulator to stop. **/
    Signal.Handler intHandler = new Signal.Handler() {
        public void execute() {
            if (dsim!=null) {
                dsim.interrupt(DigitalScheduler.InterruptedBy.USER);
            }
        }
    };

    /** Previous exception stack trace.  (See bug 1843) */
    private String lastStackTrace = "No previous exception!";

    public DSimModule() {
        super(false);
        // utils = new DSimUtil();
        addIntHandler(intHandler);
        addCommands(commands);

	Runtime.getRuntime().addShutdownHook(new Thread() {
		public void run() {
		    if(dsim != null && dsim.emitCSPCoverageProbes) {
			try {
			    Monitor.global.save();
			} catch(IOException e) {
			    System.out.println("CSP coverage monitor: Error saving data: "+
					       "caught IO exception: "+e);
			}
		    }
		}
	    });
    }
    public String getName() { return "DSim"; }
    public String getHelpMsg() { return "\tDSim/\t\t\tdigital simulation module"; }
    public String getExtendedHelpMsg() {
        return "\nModule DSim\n\n"+
               "\tDSim digitally simulates production rules specified by a\n"+
               "\thierarchical CAST netlist.\n\n";
    }
    // returns 0,1,2 for "0","1","u" respectively. default is -1
    public byte parseNodeVal(String s) {
        byte val = -1;
        if (s.equals("u") || s.equals("U")) { val=2; }
        else { try { val = Byte.parseByte(s); } catch (Exception e) {} }
        return val;
    }
    /** Creates a simulation object and tells it to load production rules from <code>file</code>. **/
    public void loadFile(String file) {
        if (checkFile()) {
            System.err.println("DSim: Cast file already loaded");
            return;
        }
        try {
            dsim = DSim.get();
	    
            System.out.println("Loading Cast File: "+file+" ...");
            dsim.loadFile(file);
            System.out.println("Finished");
        } catch (Exception e) {
            dsim = null;
            System.err.println("Error Initializing DSim: "+e.getMessage());
            if (debug) {
                e.printStackTrace();
            }
        }
    }
    /** Notification that this module was loaded into another, with invocation arguments. **/
    public void install(CmdModule parent, String args[]) {
        // process any arguments from the invocation command line
      
      final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
      final CommandLineArgs argsWithConfigs =
          new CommandLineArgsWithConfigFiles( parsedArgs ); 

      final CommandLineArgs cachedArgs = 
          new CachingCommandLineArgs( argsWithConfigs );
      
      final CommandLineArgs theArgs = cachedArgs;

      castVersion = theArgs.getArgValue( "cast-version", CastFileParser.defaultCastVersion );
      final boolean showVersion = theArgs.argExists( "version" );
      final SearchPath castPath = new FileSearchPath( theArgs.getArgValue( "cast-path", "." ) );
      final SearchPath devicePath = new FileSearchPath( theArgs.getArgValue( "device-path", "." ) );

      dsim = DSim.get();
      dsim.setCastVersion( castVersion );
      dsim.setCastPath( castPath );
      dsim.setArgs( theArgs );

      try {
          final Integer cosimSlack =
              CommandLineArgsUtil.getIntegerArgValue(theArgs, "cosim-slack",
                                                     null);
          if (cosimSlack != null) {
              if (cosimSlack <= 0) {
                  System.err.println("Error: invalid cosim slack specified; " +
                                     "slack must be at least 1.");
                  System.exit(1);
              }
              dsim.setCosimSlack(cosimSlack);
          }

      } catch (CommandLineArgFormatException e) {
          System.err.println("Error: Invalid cosim slack specified; " +
                             "malformed integer specified.");
          System.exit(1);
      }

      try {
          astaThreads = 
              CommandLineArgsUtil.getNonNegativeIntegerArgValue(
                      theArgs, "asta-threads", 2);
      } catch (CommandLineArgFormatException e) {
          System.err.println("Error: Invalid asta-threads specified; " +
                             "malformed integer specified.");
          System.exit(1);
      } catch (InvalidCommandLineArgException e) {
          System.err.println("Error: " + e.getMessage());
          System.exit(1);
      }
      
      final ClassLoader deviceLoader = new ConfigurableClassLoader( devicePath );
      dsim.setDeviceLoader( deviceLoader );
      

      if ( showVersion ) {
	  final Package packageInfo = this.getClass().getPackage();
	  final String versionStr;
	  if ( packageInfo != null ) {
	       versionStr = packageInfo.getImplementationVersion();
	  }
	  else {
	      versionStr = null;
	  }

	  if ( versionStr != null ) {
	      System.out.println( "DSim Build: " + versionStr );
	  }
	  else {
	      System.out.println( "Unknown DSim Build" );
	  }
      }
      
      StringContainerIterator strIter = theArgs.nonParsedArgumentsIterator();
      
      if ( strIter.hasNext() ) {
          loadFile(strIter.next());
      }
    }
    /** The prefix (if any) tacked onto any node names in commands. **/
    String nodePrefix = null;
    public void setNodePrefix(String prefix) { nodePrefix=prefix; }
    /** returns a list of matching strings, and possibly prints warning for non-matches **/
    public boolean expandNodes(String name, Collection v, boolean nowarn) {
        if (name==null || name.length()<1) { return false; }
        String fullname = (nodePrefix==null) ? name : (nodePrefix+name);
        return dsim.expandNodes(fullname, v, false, nowarn);
    }
    /** Callback class that does somthing for each node in a list. **/
    public interface NodeIterator {
        void processNode(HierName name, Node node);
    }
    /** Callback class that does something for each half-op in a list. **/
    public interface HalfOpIterator {
        // dir is Boolean.TRUE for up, Boolean.FALSE for down, null for both
        void processHalfOp(HierName name, Node node, Boolean dir);
    }

    /** Loops over a list of node arguments, expanding them and calling the NodeIterator for each.
        For nodes that are not found, a warning is printed  and the iterator is not called. **/
    public void processNodeArgs(String args[], int start, int end, NodeIterator ni) {
        if (args!=null && args.length>0) {
            HashSet names = new HashSet();
            for (int i=start; i<end; i++) {
                expandNodes(args[i], names, false);
            }
            for (Iterator it = names.iterator(); it.hasNext(); ) {
                HierName name = (HierName)it.next();
                Node node = dsim.findNode(name);
                ni.processNode(name, node);
            }
        }
    }

    public void processHalfOpArgs(String args[], int start, int end,
                                  HalfOpIterator ni) {
        if (args != null && args.length > 0) {
            for (int i = start; i < end; ++i) {
                final Boolean dir;
                if (args[i].endsWith("+")) {
                    dir = Boolean.TRUE;
                } else if (args[i].endsWith("-")) {
                    dir = Boolean.FALSE;
                } else {
                    dir = null;
                }

                final String glob =
                    dir == null ? args[i]
                                : args[i].substring(0, args[i].length() - 1);
                final TreeSet names = new TreeSet();
                expandNodes(glob, names, false);
                for (Iterator it = names.iterator(); it.hasNext(); ) {
                    HierName name = (HierName) it.next();
                    Node node = dsim.findNode(name);
                    ni.processHalfOp(name, node, dir);
                }
            }
        }
    }

    private static Pair<String[],OutputStream> getRedirection(String[] args) {
        if (args == null || args.length == 0)
            return new Pair<String[],OutputStream>(args, null);

        final String last = args[args.length - 1];
        String fileName = null;
        boolean append = false;

        // check last argument for redirection
        if (last.startsWith(">>")) {
            append = true;
            fileName = last.substring(2);
        } else if (last.startsWith(">")) {
            fileName = last.substring(1);
        }

        String[] newArgs = args;
        OutputStream os = null;
        if (fileName != null) {
            try {
                os = new FileOutputStream(fileName, append);
            } catch (FileNotFoundException e) {
                System.err.println("Cannot redirect output: " + e.getMessage());
            }

            // strip off the last argument and return the rest
            newArgs = new String[args.length - 1];
            System.arraycopy(args, 0, newArgs, 0, args.length - 1);
        }
        return new Pair<String[],OutputStream>(newArgs, os);
    }

    private abstract class RedirectCommand extends CmdCommand {
        public RedirectCommand(String _name, String _usage, String _desc) {
            super(_name, _usage, _desc);
        }
        public RedirectCommand(String _name, String _usage, String _desc,
                               String _extDesc) {
            super(_name, _usage, _desc, _extDesc);
        }

        public void execute(final String[] args) {
            final Pair<String[],OutputStream> p = getRedirection(args);
            final OutputStream os = p.getSecond();

            // if redirection not found, or file successfully opened
            if (args == p.getFirst() || os != null) {
                final PrintWriter pw =
                    new PrintWriter(os == null ? System.out : os);
                try {
                    execute(p.getFirst(), pw);
                } finally {
                    pw.flush();
                    if (os != null) pw.close();
                }
            }
        }

        public abstract void execute(final String[] args, final PrintWriter pw);
    }

    private class DelayCommand extends CmdCommand {
        private int mode;
        public DelayCommand(String _name, String _usage, String _desc,
                            String _extDesc, int mode) {
            super(_name, _usage, _desc, _extDesc);
            this.mode = mode;
        }
        public void execute(final String[] args) {
            if (args != null && args.length != 1) {
                System.out.println("Usage: " + usage);
                return;
            }

            checkDSim();

            if (args == null) {
                System.out.println(this.getName() + " is " +
                        (dsim.useDelayMode(mode) ?
                            Float.toString(dsim.getDelayTau(mode)) :
                            "off"));
                return;
            }

            if (args[0].equals("off")) {
                dsim.disableDelayMode(mode);
            } else {
                float tau;
                try {
                    tau = Float.parseFloat(args[0]);
                } catch (NumberFormatException e) {
                    tau = Float.NaN;
                }
                if (tau <= 0 || Float.isNaN(tau)) {
                    System.out.println("Invalid tau specified: " + args[0]);
                } else {
                    dsim.enableDelayMode(mode, tau);
                }
            }
        }
    }

    private static void chanFill(final boolean sortFull,
                                 final boolean glob,
                                 final boolean verbose,
                                 final boolean fullOnly,
                                 String regex) {
        if (regex == null) {
            regex = ".*";
        } else if (glob) {
            regex = StringUtil.getRegexForGlob(regex);
        }
        final Pattern pat = Pattern.compile(regex);
        final TreeSet<ChannelStatus> matched =
            new TreeSet<ChannelStatus>(
                new Comparator<ChannelStatus>() {
                    private float getRatio(ChannelStatus s) {
                        final int cap = s.getCapacity();
                        final int avail = s.getAvailable();
                        return cap == 0 ? 0 : (1 - avail / (float) cap);
                    }
                    public int compare(ChannelStatus s1,
                                       ChannelStatus s2) {
                        if (sortFull) {
                            int x = Float.compare(getRatio(s1), getRatio(s2));
                            if (x != 0) return x;
                            return NaturalStringComparator.compareString(
                                s1.getName(), s2.getName());
                        } else {
                            int x = NaturalStringComparator.compareString(
                                s1.getName(), s2.getName());
                            if (x != 0) return x;
                            return Float.compare(getRatio(s1), getRatio(s2));
                        }
                    }
                }
            );

        final UnaryPredicate filter = new UnaryPredicate() {
            public boolean evaluate(Object o) {
                if (o instanceof ChannelStatus) {
                    final ChannelStatus chan = (ChannelStatus) o;
                    return pat.matcher(chan.getName()).matches() &&
                           (!fullOnly || chan.getAvailable() == 0);
                }
                return false;
            }
        };

        for (CspRuntimeAbstractDevice cspDevice :
                CspRuntimeAbstractDevice.getAllCspDevices()) {
            CollectionUtils.addAll(
                matched,
                new FilteringIterator(cspDevice.getInputChannels().iterator(),
                                      filter));
            CollectionUtils.addAll(
                matched,
                new FilteringIterator(cspDevice.getOutputChannels().iterator(),
                                      filter));
        }

        int maxLen = 0;
        int maxCap = 0;
        int maxFill = 0;
        for (ChannelStatus chan : matched) {
            maxLen = Math.max(maxLen, chan.getName().length());
            maxCap = Math.max(maxCap, chan.getCapacity());
            maxFill = Math.max(maxFill,
                               chan.getCapacity() - chan.getAvailable());
        }

        final String fmt =
            "%-" + (maxLen + 1) + "s" +
            "%" + (Integer.toString(maxCap).length()) + "d/" +
            "%" + (Integer.toString(maxFill).length()) + "d";

        for (ChannelStatus chan : matched) {
            final String s =
                String.format(fmt, chan.getName(), chan.getCapacity(),
                              chan.getCapacity() - chan.getAvailable());
            if (verbose) {
                final String v = chan.getVerboseStatus();
                final String[] parts = StringUtil.split(v, ' ');
                if (parts.length == 0) {
                    System.out.println(s);
                } else {
                    System.out.println(s + " " + parts[0]);

                    final String blanks =
                        StringUtil.repeatString(" ", s.length() + 1);
                    for (int i = 1; i < parts.length; ++i) {
                        System.out.println(blanks + parts[i]);
                    }
                }
            } else {
                System.out.println(s);
            }
        }
    }
 
    private static int[] getFillInfo(final CspRuntimeAbstractDevice dev,
                                     String chan) {
        int opIndex = chan.lastIndexOf('#');
        if (opIndex == -1) opIndex = chan.lastIndexOf('!');
        if (opIndex == -1) opIndex = chan.lastIndexOf('?');
        if (opIndex != -1) chan = chan.substring(0, opIndex);

        for (ChannelInput c : dev.getInputChannels()) {
            if (chan.equals(c.getName())) {
                if (c instanceof ChannelStatus) {
                    ChannelStatus s = (ChannelStatus) c;
                    return new int[] { s.getCapacity(),
                                       s.getCapacity() - s.getAvailable() };
                }
            }
        }

        for (ChannelOutput c : dev.getOutputChannels()) {
            if (chan.equals(c.getName())) {
                if (c instanceof ChannelStatus) {
                    ChannelStatus s = (ChannelStatus) c;
                    return new int[] { s.getCapacity(),
                                       s.getCapacity() - s.getAvailable() };
                }
            }
        }

        return null;
    }

    private void matchingDevices(boolean glob, String regex,
                                 Collection<String> result,
                                 boolean cspOnly) {
        // fill in member variable dsim
        checkDSim();
        if (regex == null) {
            regex = ".*";
        } else if (glob) {
            regex = StringUtil.getRegexForGlob(regex);
        }

        final Pattern pat = Pattern.compile(regex);

        for (String name :
                new IterableIterator<String>(dsim.getDeviceNames())) {
            if (pat.matcher(name).matches() &&
                (!cspOnly ||
                 dsim.getDevice(name) instanceof CspRuntimeAbstractDevice))
                result.add(name);
        }
    }

    private void printDevices(final boolean glob,
                              final boolean showPos,
                              boolean showFill,
                              boolean showChan,
                              boolean showFull,
                              String regex) {
        // fill in member variable dsim
        checkDSim();

        // print all device names
        final TreeSet<String> devices =
            new TreeSet<String>(NaturalStringComparator.getInstance());
        matchingDevices(glob, regex, devices, false);
        int maxDevice = 0;
        int maxChan = 0;
        int maxCap = -1;
        int maxFill = -1;
        for (String name : devices) {
            maxDevice = Math.max(maxDevice, name.length());
            final AbstractDevice dev = dsim.getDevice(name);
            if (dev instanceof CspRuntimeAbstractDevice) {
                final CspRuntimeAbstractDevice cdev =
                    (CspRuntimeAbstractDevice) dev;

                if (cdev.currChanOp != null) {
                    maxChan = Math.max(maxChan, cdev.currChanOp.length() -
                                                name.length() - 1);
                    int[] fillInfo = getFillInfo(cdev, cdev.currChanOp);
                    if (fillInfo != null) {
                        maxCap = Math.max(maxCap, fillInfo[0]);
                        maxFill = Math.max(maxFill, fillInfo[1]);
                    }
                }
            }
        }

        if (maxChan == 0) showChan = false;
        if (!showChan || maxCap < 0) showFill = false;

        final String nameFmt = "%-" + maxDevice + "s";
        final String chanFmt = "%-" + maxChan + "s";
        final String fillFmt =
            "%" + (Integer.toString(maxCap).length()) + "d/" +
            "%" + (Integer.toString(maxFill).length()) + "d";
        final String emptyFill =
            StringUtil.repeatString(" ", 1 + Integer.toString(maxCap).length() +
                                         Integer.toString(maxFill).length());

        for (String name : devices) {
            final AbstractDevice dev = dsim.getDevice(name);
            final String namePart = String.format(nameFmt, name);
            boolean first = true;
            char lead = ' ';
            StringBuilder extraBuf = new StringBuilder();
            if (dev instanceof CspRuntimeAbstractDevice) {
                final CspRuntimeAbstractDevice cdev =
                    (CspRuntimeAbstractDevice) dev;
                if (cdev.whereAmI != null) {
                    String chan = null;
                    String fill = emptyFill;

                    if (cdev.currChanOp != null) {
                        final String chanName =
                            cdev.currChanOp.substring(name.length() + 1);
                        chan = String.format(chanFmt, chanName);
                        final int[] fillInfo =
                            getFillInfo(cdev, cdev.currChanOp);
                        if (fillInfo != null) {
                            fill = String.format(fillFmt, fillInfo[0],
                                                 fillInfo[1]);
                        }
                    }

                    if (showChan) {
                        if (chan == null) chan = String.format(chanFmt, "");
                        if (first) first = false; else extraBuf.append("  ");
                        extraBuf.append(chan);
                        if (showFill) {
                            extraBuf.append("  ");
                            extraBuf.append(fill);
                        }
                    }

                    if (showPos) {
                        if (first) first = false; else extraBuf.append("  ");
                        String whereAmI = cdev.whereAmI;
                        if (!showFull) {
                            final int space = whereAmI.lastIndexOf(' ');
                            if (space >= 0) {
                                final String filePart =
                                    whereAmI.substring(0, space);
                                final String rest = whereAmI.substring(space);
                                whereAmI = (new File(filePart)).getName() +
                                           rest;
                            }
                        }
                        extraBuf.append(whereAmI);
                    }
                }
                if (cdev.getYieldCount() == 0) lead = '*';
            }
            final String extraPart =
                extraBuf.length() == 0 ? "" : "  (" + extraBuf.toString() + ")";

            System.out.println(lead + namePart + extraPart);
        }
    }

    private void enumerateVariables(final String name, final CspValue val,
                                    final Map<String,CspValue> result,
                                    final Pattern pat) {
        final boolean matched = pat == null || pat.matcher(name).matches();
        if (val instanceof CspInteger) {
            if (matched) result.put(name, val);
        } else if (val instanceof CspStructure) {
            //if (matched) result.put(name, val);
            final Field[] fields = val.getClass().getFields();
            for (Field field : fields) {
                final String fname = field.getName();
                final Object obj;
                try {
                    obj = field.get(val);
                } catch (IllegalAccessException e) {
                    System.out.println("Trouble accessing field " + fname +
                                       " of structure " +
                                       val.getClass().getName());
                    continue;
                }

                if (fname.startsWith("_") && obj instanceof CspValue) {
                    enumerateVariables(name + '.' + fname.substring(1),
                                       (CspValue) obj,
                                       result, pat);
                }
            }
        } else if (val instanceof CspArray) {
            //if (matched) result.put(name, val);
            final CspArray array = (CspArray) val;
            for (int i = array.getMinIndex(); i <= array.getMaxIndex(); ++i) {
                final CspInteger idx =
                    new CspInteger(new BigInteger(Integer.toString(i)));
                enumerateVariables(name + '[' + i + ']',
                                   array.get(idx),
                                   result, pat);
            }
        }
    }

    private void enumerateVariables(final StackFrame frame,
                                    final Map<String,CspValue> result,
                                    final Pattern pat) {
        final Map<String,Pair<String,CspValue>> vars = frame.getVariables();
        for (Map.Entry<String,Pair<String,CspValue>> entry : vars.entrySet()) {
            enumerateVariables(entry.getKey(), entry.getValue().getSecond(),
                               result, pat);
        }
    }

    private void processVariables(
            final Collection<String> names, final Pattern pat, int id,
            final BinaryAction<String,Map.Entry<String,CspValue>> act) {
        boolean warnReset = false;
        boolean warnFrame = false;
        for (String name : names) {
            final CspRuntimeAbstractDevice dev =
                (CspRuntimeAbstractDevice) dsim.getDevice(name);
            final TreeMap<String,CspValue> vals =
                new TreeMap<String,CspValue>(
                    NaturalStringComparator.getInstance());
            if (dev.topFrame() == null) {
                warnReset = true;
            } else {
                final Collection<StackFrame> frames = dev.getFrames();
                if (id == -1) {
                    // -1 means the bottom most frame
                    id = frames.size() - 1;
                }
                boolean found = false;
                for (StackFrame frame : frames) {
                    if (id == 0) {
                        enumerateVariables(frame, vals, pat);
                        for (Map.Entry<String,CspValue> entry :
                                vals.entrySet()) {
                            act.execute(name, entry);
                        }
                        found = true;
                        break;
                    }
                    --id;
                }
                if (!found) warnFrame = true;
            }
        }
        if (warnReset || warnFrame) {
            String msg = "Variables may be incomplete because ";
            if (warnReset && warnFrame) {
                msg += "some devices were not reset or have quit, or ";
                msg += "the specified stack frame is out of range.";
            } else if (warnReset) {
                msg += "some devices were not reset or have quit.";
            } else {
                msg += "the specified stack frame is out of range.";
            }
            System.err.println(msg);
        }
    }

    private String cspValueString(final CspValue var) {
        if (var instanceof CspBoolean) {
            return Boolean.toString(((CspBoolean) var).booleanValue());
        } else {
            return var.toString();
        }
    }

    private void cspGet(final Collection<String> names, final int frame,
                        final Pattern pat, final PrintWriter pw) {
        final boolean[] nothing = new boolean[] { true };
        processVariables(
            names,
            pat,
            frame,
            new BinaryAction<String,Map.Entry<String,CspValue>>() {
                public void execute(String name,
                                    Map.Entry<String,CspValue> entry) {
                    final CspValue var = entry.getValue();
                    pw.println("  " + name + '.' + entry.getKey() + ": " +
                               cspValueString(var));
                    nothing[0] = false;
                }
            });
        if (nothing[0]) {
            System.err.println("No matching variables found.");
        }
    }

    private void cspSet(final Collection<String> names, final Pattern pat,
                        final int frame, final CspInteger val,
                        final boolean isBool, final PrintWriter pw) {
        final boolean[] nothing = new boolean[] { true };
        processVariables(
            names,
            pat,
            frame,
            new BinaryAction<String,Map.Entry<String,CspValue>>() {
                public void execute(String name,
                                    Map.Entry<String,CspValue> entry) {
                    final CspValue var = entry.getValue();
                    if (isBool == (var instanceof CspBoolean)) {
                        final String old = cspValueString(var);
                        var.setValue(val);
                        pw.println("  " + name + '.' + entry.getKey() + ": " +
                                   cspValueString(var) + " (" + old + ")");
                        nothing[0] = false;
                    }
                }
            });
        if (nothing[0]) {
            System.err.println("No matching " + (isBool ? "bool" : "int") +
                               " variables found.");
        }
    }

    private void cspWhere(final Collection<String> names, final PrintWriter pw) {
        for (String name : names) {
            final CspRuntimeAbstractDevice dev =
                (CspRuntimeAbstractDevice) dsim.getDevice(name);
            pw.println(name + ':');
            final Collection<StackFrame> frames = dev.getFrames();
            StackFrame lastFrame = null;
            int i = 0;
            for (StackFrame frame : frames) {
                String funcName = frame.getFunctionName();
                if (funcName == null) funcName = "top level";

                String ppos = lastFrame == null ? dev.whereAmI
                                                : lastFrame.getCallLocation();
                if (ppos == null) ppos = "no information";

                pw.printf("  %2d: %s (%s)\n", i, funcName, ppos);

                lastFrame = frame;
                ++i;
            }
        }
    }

    private static class AlignColumn {
        private final Collection<String[]> rows;
        private final int[] size;
        public AlignColumn(final Collection<String[]> rows,
                           final int cols) {
            this.rows = rows;
            this.size = new int[cols];
        }
        public void add(final String... cols) {
            for (int i = 0; i < cols.length; ++i) {
                size[i] = Math.max(size[i], cols[i].length());
            }
            rows.add(cols);
        }
        public int getSize(int col) {
            return size[col];
        }
        public Iterator<String> getLine(final String sep) {
            final Iterator<String[]> it = rows.iterator();
            return new Iterator<String>() {
                public boolean hasNext() {
                    return it.hasNext();
                }
                public String next() throws NoSuchElementException {
                    final String[] cols = it.next();
                    final StringBuilder sb = new StringBuilder();
                    for (int i = 0; i < cols.length; ++i) {
                        if (i > 0) sb.append(sep);
                        sb.append(
                            String.format("%" + getSize(i) + "s", cols[i]));
                    }
                    return sb.toString();
                }
                public void remove() throws UnsupportedOperationException,
                                            IllegalStateException {
                    throw new UnsupportedOperationException();
                }
            };
        }
    }

    private void cspEnergy(final Collection<String> names,
                           final boolean reset, final PrintWriter pw) {
        final AlignColumn ac = new AlignColumn(new ArrayList<String[]>(), 2);
        double sum = 0.0;
        for (String name : names) {
            final CspRuntimeAbstractDevice dev =
                (CspRuntimeAbstractDevice) dsim.getDevice(name);

            final double energy = dev.getEnergy();
            ac.add(name + ":", String.format("%.3e J", energy));
            sum += energy;
            if (reset) {
                dev.resetEnergy();
            }
        }
        ac.add("Total", String.format("%.3e J", sum));

        for (Iterator<String> i = ac.getLine(" "); i.hasNext(); ) {
            pw.print(i.next());
            if (reset) pw.print(" -> 0");
            pw.println();
        }
    }

    private enum SSTOperation {
        PAUSE  { void execute(Sigscan db) { db.pause();  } },
        RESUME { void execute(Sigscan db) { db.resume(); } },
        CLOSE  { void execute(Sigscan db) { db.close();  } };

        abstract void execute(Sigscan db);
    };

    private void executeSST(SSTOperation op) {
        final Collection<Sigscan> dbs = new HashSet<Sigscan>();
        for (String name :
                new IterableIterator<String>(dsim.getDeviceNames())) {
            final AbstractDevice dev = dsim.getDevice(name);
            if (dev.getSigscan() != null) {
                dbs.add(dev.getSigscan());
            }
        }
        if (dsim.getSigscan() != null) {
            dbs.add(dsim.getSigscan());
        }

        for (Sigscan db : dbs) {
            op.execute(db);
        }
    }

    private void clearYieldCounts() {
        for (String name :
                new IterableIterator<String>(dsim.getDeviceNames())) {
            final AbstractDevice dev = dsim.getDevice(name);
            if (dev instanceof CspRuntimeAbstractDevice) {
                ((CspRuntimeAbstractDevice) dev).resetYieldCount();
            }
        }
    }

    private class FanCommand extends CmdCommand {
        private final boolean fanin;
        public FanCommand(String name, String usage, String desc,
                          String extDesc, boolean fanin) {
            super(name, usage, desc, extDesc);
            this.fanin = fanin;
        }

        private boolean parseLevels(final String[] args, final int[] result) {
            if (args != null && args.length > 0 && args[0].equals("-l")) {
                if (args.length == 1) {
                    System.err.println(getName() +
                                       ": -l requires levels argument");
                    return false;
                }

                try {
                    result[0] = validateIntArg(args, 1); 
                    if (result[0] < 0) {
                        System.err.println(getName() + ": levels must be >= 0");
                        return false;
                    }
                } catch (Exception e) {
                    return false;
                }
                result[1] = 2;
            }
            return true;
        }

        public void execute(String args[]) {
            if (!checkFile()) return;

            final int[] levelStart = { 0, 0 };
            if (!parseLevels(args, levelStart)) return;

            if (args != null && args.length > levelStart[1]) {
                processNodeArgs(args, levelStart[1], args.length,
                    new NodeIterator() {
                        public void processNode(HierName name, Node node) {
                            if (node!=null) {
                                System.out.println(getName()+" of "+name+":");
                                if (fanin) {
                                    dsim.listFanin(node, "    ", levelStart[0]);
                                } else {
                                    dsim.listFanout(node, "    ", levelStart[0]);
                                }
                            }
                        }
                    });
            } else {
                System.err.println(getName() + ": no node names given");
            }
        }
    }

    /** All of the commands that make the simulator go. */
    CmdCommand commands[] = {
        new CmdCommand("instantiate", 
            "instantiate <celltype>[:envname]", 
            "Instantiates a cell",
            "Instantiate instructs DSim to instantiate a cell of type\n"+
            "<celltype> as \"x\", with its environment envname\n"+
            "under the _env namespace.  If <celltype> is not a\n"+
            "fully-qualified cell type, a CAST file defining <celltype>\n"+
            "must first have been loaded (with 'load').\n\n"+
            "Equivalent to\n"+
            "cosimulate celltype{subcells,prs}"+
                "[:envname{prs,subcells,csp,java}]") {
            public void execute(String args[]) {
                if (args == null || args.length != 1) {
                    System.out.println("Usage: "+usage);
                    return;
                }
                checkDSim();
                // Set the environment if it was specified
                int cidx = args[0].indexOf(":");
                final String celltype;
                final String envname;
                if (cidx < 0) {
                    celltype = args[0];
                    envname = null;
                } else {
                    celltype = args[0].substring(0,cidx);
                    envname = args[0].substring(cidx+1,args[0].length());
                }

                clear();
                cosimulate(celltype + "{subcells,prs}" +
                           (envname == null ? "" :
                            ":" + envname + "{prs,subcells,csp,java}"),
                           "x");
            }
        },
        new CmdCommand("exit", "exit", "Exits the DSim commmand shell") {
            public void execute(String args[]) { 
                // TODO add any cleanup code here...
                if (dsim!=null)  {
                    dsim.interrupt();
                    dsim = null;
                }
            }
        },
        new CmdCommand("with", "with [prefix]", "Sets prefix (root) for all node arguments") {
            public void execute(String args[]) { 
                if (args==null || args.length<1) { setNodePrefix(null); }
                else { setNodePrefix(args[0]); }
            }
        },
        new CmdCommand("pend", "pend", "Gives count of pending transitions") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    System.out.println("Pending Transitions:");
                    System.out.println(dsim.pendingList());
                    System.out.println(dsim.randomCount()+" random, "+dsim.timedCount()+" timed.");
                }
            }
        },
        new FanCommand ("fanout", "fanout [-l <levels>] <node(s)>",
            "Lists targets for a node",
            "This command lists all nodes for which the specified node\n"+
            "or nodes drive.  If -l is specified, then list the fanout\n" +
            "nodes recursively <levels> times.", false),
        new FanCommand ("fanin", "fanin [-l <levels>] <node(s)>",
            "Lists guards for a node",
            "This command lists all nodes which appear in the production\n"+
            "rules driving the node(s) specified.  If -l is specified, then\n"+
            "list the fanin nodes recursively <levels> times.", true),
        new CmdCommand("param", "param <key>=[<value>] ...",
            "Set global DSim parameters",
            "Set global DSim parameters") {
            public void execute(String args[]) {
                if (args != null && checkDSim()) {
                    for (String arg : args) {
                        final int eq = arg.indexOf('=');
                        if (eq <= 0) {
                            System.err.println(
                                    "Error: invalid argument " + arg);
                            return;
                        } else {
                            final String key = arg.substring(0, eq);
                            String val = arg.substring(eq + 1);
                            if (val.equals("")) {
                                System.out.println("Unset " + key);
                                val = null;
                            } else {
                                System.out.println("Set " + key + " to " + val);
                            }
                            dsim.setParameter(key, val);
                        }
                    }
                }
            }
        },
        new CmdCommand ("sta", "sta <command> [options]", 
            "Run static timing analysis",
            "Supported commands are:\n" +
            "  on | off\n" +
            "  evaluate --tau=<tau> --input-slew=<default slew> --max-iterations=<limit>\n" +
            "           --max-walltime=<time limit> --sweep=<precision>\n" +
            "  write --output=<filel>\n" +
            "  read <file> ...\n" +
            "  simpleSwap --max-swaps=<swaps> --max-outerLoop=<loops>\n" +
            "             --spec=<spec file>\n" +
            "             --include-regex=<regex>\n" +
            "             <options to evaluate>\n" +
            "             <options to report>\n" +
            "  report --type=[flat|hier] --worst-tau=<tau> --max-violations=<limit>\n" +
            "         --verbose --output=<file>") {
            private PrintWriter getWriter(final String output) {
                final PrintWriter pw;
                if (output == null) {
                    pw = new PrintWriter(System.out);
                } else {
                    try {
                        pw = new PrintWriter(new FileWriter(output));
                    } catch (IOException e) {
                        System.out.println("Cannot write file " + output +
                                           ": " + e.getMessage());
                        return null;
                    }
                }
                return pw;
            }
            private BufferedReader getReader(final String input) {
                try {
                    return new BufferedReader(new FileReader(input));
                } catch (IOException e) {
                    System.out.println("Cannot read file " + input + ": " +
                                       e.getMessage());
                    return null;
                }
            }
            private RunStaticTiming.EvaluateOption getEvaluateAction(
                    final CommandLineArgs theArgs) {
                final float tau;
                final float slew;
                final int iterations;
                final long walltime;
                final float sweep;

                try {
                    tau = CommandLineArgsUtil.getFloatArgValue(
                            theArgs, "tau", 55.0f);
                    slew = CommandLineArgsUtil.getFloatArgValue(
                            theArgs, "input-slew", 30.0f);
                    iterations = CommandLineArgsUtil.getIntegerArgValue(
                            theArgs, "max-iterations", 1000);
                    walltime = CommandLineArgsUtil.getIntegerArgValue(
                            theArgs, "max-walltime", 0) * 1000L;
                    sweep = CommandLineArgsUtil.getFloatArgValue(
                            theArgs, "sweep", Float.NaN);
                } catch (CommandLineArgFormatException e) {
                    System.err.println("Cannot parse argument --" +
                            e.getArgName());
                    return null;
                }

                /*
                final Set<Node> observables = new HashSet<Node>();
                final StringContainerIterator it =
                    theArgs.nonParsedArgumentsIterator();
                if (it.hasNext()) {
                    final ArrayList<String> rest =
                        new ArrayList<String>();
                    while (it.hasNext()) rest.add(it.next());
                    final String[] nodes = rest.toArray(new String[0]);
                    processNodeArgs(nodes, 0, nodes.length, new NodeIterator() {
                        public void processNode(HierName name, Node node) {
                            if (node != null) observables.add(node);
                        }
                    });
                }
                */
                return new RunStaticTiming.EvaluateOption(
                        dsim, tau, slew, iterations, walltime, astaThreads,
                        sweep);
            }
            public void execute(String args[]) {
                if (args == null || args.length < 1) {
                    System.out.println("Usage: " + usage);
                    return;
                }

                final String cmd = args[0];
                final CommandLineArgs theArgs =
                    new CachingCommandLineArgs(
                        new CommandLineArgsDefImpl(
                            Arrays.copyOfRange(args, 1, args.length)));

                if (cmd.equals("on")) {
                    if (!dsim.isAstaEnabled()) {
                        sta = new RunStaticTiming();
                        dsim.enableAsta();
                    }
                } else if (cmd.equals("off")) {
                    if (dsim.isAstaEnabled()) {
                        sta = null;
                        dsim.disableAsta();
                    }
                } else if (cmd.equals("read")) {
                    for (StringContainerIterator it =
                            theArgs.nonParsedArgumentsIterator();
                         it.hasNext(); ) {
                        final String file = it.next();
                        final BufferedReader br = getReader(file);
                        if (br != null) {
                            try {
                                sta.readContext(br);
                            } catch (IOException e) {
                                System.err.println("Error reading file: " + 
                                        file + ": " + e.getMessage());
                            }
                        }
                    }
                    dsim.setAstaContext(sta.getWorstContext());
                } else if (checkFile()) {
                    if (cmd.equals("evaluate")) {
                        if (!dsim.isAstaEnabled()) {
                            System.err.println("ASTA must be enabled first.");
                            return;
                        }
                        if (dsim.errors) {
                            System.err.println("DSim instantiation failed.");
                            return;
                        }

                        final RunStaticTiming.EvaluateOption opt =
                            getEvaluateAction(theArgs);
                        if (opt != null) {
                            try {
                                sta.evaluate(opt);
                            } catch (RunStaticTiming.SolverException e) {
                                System.err.println("Cannot invoke solver: " +
                                                   e.getMessage());
                            }
                        }
                    } else if (cmd.equals("write")) {
                        final String output =
                            theArgs.getArgValue("output", null);
                        final PrintWriter pw = getWriter(output);
                        if (pw == null) return;

                        try {
                            sta.writeContext(pw);
                        } finally {
                            if (output != null) pw.close();
                        }
                    } else if (cmd.equals("report")) {
                        if (!dsim.isAstaEnabled()) {
                            System.err.println("ASTA must be enabled first.");
                            return;
                        }

                        final String type = theArgs.getArgValue("type", "flat");
                        final String output =
                            theArgs.getArgValue("output", null);
                        final float worstTau;
                        final int maxVios;

                        try {
                            worstTau = CommandLineArgsUtil.getFloatArgValue(
                                    theArgs, "worst-tau", 0.0f);
                            maxVios = CommandLineArgsUtil.getIntegerArgValue(
                                    theArgs, "max-violations", 1000);
                        } catch (CommandLineArgFormatException e) {
                            System.err.println("Cannot parse argument --" +
                                    e.getArgName());
                            return;
                        }

                        final boolean verbose = theArgs.argExists("verbose");
                        final boolean canonOnly = theArgs.argExists("canon");

                        final PrintWriter pw = getWriter(output);
                        if (pw == null) return;

                        try {
                            if (type.equals("flat")) {
                                sta.getFlatReport(worstTau, maxVios, verbose,
                                                  canonOnly, pw);
                            } else if (type.equals("hier")) {
                                sta.getHierReport(worstTau, pw);
                            } else {
                                System.out.println("Unknown report format: " +
                                                   type);
                                return;
                            }
                        } finally {
                            if (output != null) pw.close();
                        }
                    } else if (cmd.equals("swap")) {
                        if (!dsim.isAstaEnabled()) {
                            System.err.println("ASTA must be enabled first.");
                            return;
                        }
                        final String inst =
                            theArgs.getArgValue("instance", null);
                        final int which;

                        try {
                            which =
                                CommandLineArgsUtil.getIntegerArgValue(
                                    theArgs, "which", 1);
                        } catch (CommandLineArgFormatException e) {
                            System.err.println("Cannot parse argument --" +
                                    e.getArgName());
                            return;
                        }

                        sta.swap(inst, which);
                    } else if (cmd.equals("simpleSwap")) {
                        final float worstTau;
                        final int maxVios;
                        final int swaps;
                        final int outerLoop;

                        try {
                            worstTau = CommandLineArgsUtil.getFloatArgValue(
                                    theArgs, "worst-tau", 0.0f);
                            maxVios = CommandLineArgsUtil.getIntegerArgValue(
                                    theArgs, "max-violations", 1000);
                        } catch (CommandLineArgFormatException e) {
                            System.err.println("Cannot parse argument --" +
                                    e.getArgName());
                            return;
                        }

                        final RunStaticTiming.EvaluateOption opt =
                            getEvaluateAction(theArgs);
                        if (opt == null) {
                            return;
                        }

                        try {
                            swaps = CommandLineArgsUtil.getIntegerArgValue(
                                    theArgs, "max-swaps", 1);
                            outerLoop = CommandLineArgsUtil.getIntegerArgValue(
                                    theArgs, "max-outerLoop", 1);
                        } catch (CommandLineArgFormatException e) {
                            System.err.println("Cannot parse argument --" +
                                    e.getArgName());
                            return;
                        }

                        final String includeRegex =
                            theArgs.getArgValue("include-regex", null);
                        final Pattern includePat;
                        try {
                            if (includeRegex == null)
                                includePat = null;
                            else
                                includePat = Pattern.compile(includeRegex);
                        } catch (PatternSyntaxException e) {
                            System.err.println("Invalid regex pattern: " +
                                    e.getMessage());
                            return;
                        }
                        final UnaryPredicate<String> replacePredicate =
                            includePat == null ?
                                new UnaryPredicate.Constant<String>(true)
                              : new UnaryPredicate<String>() {
                                    public boolean evaluate(String s) {
                                        return includePat.matcher(s).matches();
                                    }
                                };

                        final String output =
                            theArgs.getArgValue("output", null);
                        final boolean routed = theArgs.argExists("routed");
                        final PrintWriter pw = getWriter(output);
                        if (pw == null) return;

                        final String spec = theArgs.getArgValue("spec", null);
                        final PrintWriter specWriter = getWriter(spec);
                        if (specWriter == null) return;

                        try {
                            sta.simpleSwap(worstTau, maxVios, swaps, outerLoop,
                                           opt, routed, replacePredicate,
                                           specWriter, pw);
                        } finally {
                            if (output != null) pw.close();
                            if (spec != null) specWriter.close();
                        }
                    } else {
                        System.out.println("Unknown sta command: " + cmd);
                    }
                }
            }
        },
        new RedirectCommand("cutoff", "cutoff [options] [>|>>file]",
            "Report digital cutoff loops",
            "Options are:\n" +
            "  --max-length=<N> (detect loops with <= N transitions; defaults to 4)"
        ) {
            public void execute(String args[], final PrintWriter pw) { 
                if (!checkFile()) return;

                try {
                    if (args == null) args = new String[0];
                    final CommandLineArgs theArgs =
                        new CachingCommandLineArgs(
                            new CommandLineArgsDefImpl(args));

                    final int maxLength =
                        CommandLineArgsUtil.getNonNegativeIntegerArgValue(
                                theArgs, "max-length", 4);
                    final CutoffChecker checker = new CutoffChecker(dsim);
                    checker.evaluate(maxLength, pw);
                } catch (CommandLineArgFormatException e) {
                    System.err.println("Error: can't parse " + e.getArgName());
                    return;
                } catch (InvalidCommandLineArgException e) {
                    System.err.println("Error: " + e.getMessage());
                    return;
                }
            }
        },
        new CmdCommand ("slint", "slint <command> [options]", 
            "Run Adversary Path Analysis",
            "Supported commands are(all times are in dsim units):\n" +
            "  on | off \n" +
            "  evaluate --abs-margin=<absoluteMargin> " +
            "--mult-margin=<multiplicativeMargin>\n" +
            "  readSlowSlews --input=<file>\n" +
            "  saveSlowSlews --output=<file>\n" +
            "  readFastSlews --input=<file>\n" +
            "  saveFastSlews --output=<file>\n" +
            "  setSlowSlew --slew=<slew>\n" +
            "  setFastSlew --slew=<slew>\n" +
            "  computeSlowSlews --slew=<initial slew>\n" +
            "  computeFastSlews --slew=<initial slew>\n" +
            "  report --output=<file> [--report-victims-once]\n" +
            "  reportFastSlews --output=<file>\n" + 
            "  reportSlowSlews --output=<file>\n") {
            private PrintWriter getWriter(final String output) {
                final PrintWriter pw;
                if (output == null) {
                    pw = new PrintWriter(System.out);
                } else {
                    try {
                        pw = new PrintWriter(new FileWriter(output));
                    } catch (IOException e) {
                        System.out.println("Cannot write file " + output +
                                           ": " + e.getMessage());
                        return null;
                    }
                }
                return pw;
            }

            private ObjectInputStream getObjectInputStream(String file) {
                FileInputStream fis;
                ObjectInputStream ois;
                try {
                    fis = new FileInputStream(file);
                    ois = new ObjectInputStream(fis);
                } catch (IOException e) {
                    System.out.println("Cannot read file " + file +
                                       " : " + e.getMessage());
                    return null;
                }
                return ois;
            }

            private void closeObjectOutputStream(ObjectOutputStream oos) {
                try {
                    oos.close();
                } catch (IOException e) {
                    System.out.println("Cannot close ouput stream: " 
                                       + e.getMessage());
                }                 
            }

            private void closeObjectInputStream(ObjectInputStream ois) {
                try {
                    ois.close();
                } catch (IOException e) {
                    System.out.println("Cannot close input stream: " 
                                       + e.getMessage());
                }                 
            }


            private ObjectOutputStream getObjectOutputStream(String file) {
                FileOutputStream fos;
                ObjectOutputStream oos;
                try {
                    fos = new FileOutputStream(file);
                    oos = new ObjectOutputStream(fos);
                } catch (IOException e) {
                    System.out.println("Cannot write file " + file +
                                       " : " + e.getMessage());
                    return null;
                }
                return oos;
            }

            public void execute(String args[]) {
                if (args == null || args.length < 1) {
                    System.out.println("Usage: " + usage);
                    return;
                }

                final String cmd = args[0];
                final CommandLineArgs theArgs =
                    new CachingCommandLineArgs(
                        new CommandLineArgsDefImpl(
                            Arrays.copyOfRange(args, 1, args.length)));

                if (cmd.equals("on")) {
                    if (slint == null) {
                        dsim.enableAsta();
                        dsim.enableSlint();
                        slint = new RunSlewChecks(dsim);
                    }
                } else if (cmd.equals("off")) {
                    slint = null;
                    dsim.disableSlint();
                    dsim.disableAsta();
                } else if (slint != null && checkFile()) {
                    if (cmd.equals("evaluate")) {
                        if (dsim.errors) {
                            System.err.println(
                                "DSim instantiation failed.");
                            return;
                        }
                        final double multMargin;
                        final double absMargin;
                        try {
                            absMargin = CommandLineArgsUtil.getFloatArgValue(
                                          theArgs, "abs-margin",  0.0f);
                            multMargin = CommandLineArgsUtil.getFloatArgValue(
                                           theArgs, "mult-margin", 1.0f);
                        } catch (CommandLineArgFormatException e) {
                            System.err.println("Cannot parse argument --" +
                                    e.getArgName());
                            return;
                        }
                        slint.evaluate(absMargin, multMargin);
                    } else if (cmd.equals("report")) {
                        final String output =
                            theArgs.getArgValue("output", null);
                        final boolean reportVictimsOnce  = 
                            theArgs.argExists("report-victims-once");
                        final PrintWriter pwOutputCheck = 
                            getWriter(output+".outSlews");
                        final PrintWriter pwIOCheck = 
                            getWriter(output+".IOSlews");
                        final PrintWriter pwStatCheck = 
                            getWriter(output+".statSlews");
                        final PrintWriter pwAll = getWriter(output);

                        if (pwOutputCheck == null ||
                            pwIOCheck == null ||
                            pwStatCheck == null ||
                            pwAll == null) return;
                        slint.getReport(pwOutputCheck, pwIOCheck, pwStatCheck,
                                        pwAll, reportVictimsOnce);
                        if (output != null) {
                            pwOutputCheck.close();
                            pwIOCheck.close();
                            pwStatCheck.close();
                            pwAll.close();
                        }
                    } else if (cmd.equals("reportSlowSlews")) {
                        final String output =
                            theArgs.getArgValue("output", null);
                        final PrintWriter pw =
                            getWriter(output);
                        if (pw == null) return;
                        slint.reportSlowSlews(pw);
                        if (output != null) {
                            pw.close();
                        }
                    } else if (cmd.equals("reportFastSlews")) {
                        final String output =
                            theArgs.getArgValue("output", null);
                        final PrintWriter pw =
                            getWriter(output);
                        if (pw == null) return;
                        slint.reportFastSlews(pw);
                        if (output != null) {
                            pw.close();
                        }
                    } else if(cmd.equals("setFastSlews")) {
                        final double slew;
                        try {
                            slew = CommandLineArgsUtil.getFloatArgValue(
                                       theArgs, "slew",  0.0f);
                        } catch (CommandLineArgFormatException e) {
                            System.err.println("Cannot parse argument --" +
                                    e.getArgName());
                            return;
                        }
                        slint.setFastSlews(slew);
                    } else if(cmd.equals("setSlowSlews")) {
                        final double slew;
                        try {
                            slew = CommandLineArgsUtil.getFloatArgValue(
                                       theArgs, "slew",  0.0f);
                        } catch (CommandLineArgFormatException e) {
                            System.err.println("Cannot parse argument --" +
                                    e.getArgName());
                            return;
                        }
                        slint.setSlowSlews(slew);
                    } else if(cmd.equals("readFastSlews")) {
                        String input = theArgs.getArgValue("input", null);
                        ObjectInputStream ois = getObjectInputStream(input);
                        if (ois == null) return;
                        slint.readFastSlews(ois);
                        closeObjectInputStream(ois);
                    } else if(cmd.equals("readSlowSlews")) {
                        String input = theArgs.getArgValue("input", null);
                        ObjectInputStream ois = getObjectInputStream(input);
                        if (ois == null) return;
                        slint.readSlowSlews(ois);
                        closeObjectInputStream(ois);
                    } else if(cmd.equals("saveFastSlews")) {
                        String output = theArgs.getArgValue("output", null);
                        ObjectOutputStream oos = getObjectOutputStream(output);
                        if (oos == null) return;
                        slint.saveFastSlews(oos);
                        closeObjectOutputStream(oos);
                    } else if(cmd.equals("saveSlowSlews")) {
                        String output = theArgs.getArgValue("output", null);
                        ObjectOutputStream oos = getObjectOutputStream(output);
                        if (oos == null) return;
                        slint.saveSlowSlews(oos);
                        closeObjectOutputStream(oos);
                    } else if(cmd.equals("computeSlowSlews")) {
                        final double slew;
                        try {
                            slew = CommandLineArgsUtil.getFloatArgValue(
                                       theArgs, "slew",  0.0f);
                        } catch (CommandLineArgFormatException e) {
                            System.err.println("Cannot parse argument --" +
                                    e.getArgName());
                            return;
                        }
                        slint.computeSlowSlews(slew);
                    } else if(cmd.equals("computeFastSlews")) {
                        final double slew;
                        try {
                            slew = CommandLineArgsUtil.getFloatArgValue(
                                       theArgs, "slew",  0.0f);
                        } catch (CommandLineArgFormatException e) {
                            System.err.println("Cannot parse argument --" +
                                    e.getArgName());
                            return;
                        }
                        slint.computeFastSlews(slew);
                    } else {
                        System.out.println("Unkown slint command: " + cmd);
                    }
                } else if (slint == null) {
                    System.out.println("slint must be enabled first.");
                }
            }
        },
        new CmdCommand ("rules", "rules <node(s)>", "Lists rules for a node",
            "Lists all production rules driving the specified node (or nodes)\n"+
            "as disjunctions of conjunctive clauses (DNF).\n\n"+
            "First, information about a node is reported:\n\n"+
            "    <node> (<S1>-><S2>, <W>-, <X>+, <Y>u-, <Z>u+)\n\n"+
            "where <S1> and <S2> are the current and pending states of <node>,\n"+
            "<W> and <X> are the pending counts of disjuncts that drive <node>\n"+
            "down and up, respectively, and <Y> and <Z> are the pending\n" +
            "counts of disjuncts that contain unknown conjuncts that drive\n" +
            "<node> down and up, respectively.\n\n"+
            "Then, disjuncts driving the node are listed:\n\n" +
            "    <guard> & ... -> <node> (<S0>-><S1>, <M>U, <N>F)\n\n" +
            "where <S0> and <S1> are the previous and current states of the\n"+
            "rule (0=OFF, 1=ON, U=UNKNOWN), <M> is the number of unknown\n" +
            "conjuncts, and <N> is the number of false conjuncts.") {
            public void execute(String args[]) {
                if (checkFile()) {
                    if (args!=null && args.length>0) {
                        processNodeArgs(args, 0, args.length, new NodeIterator() {
                            public void processNode(HierName name, Node node) {
                                if (node!=null) {
                                    System.out.println("Rules for " +
                                        name.getAsString('.') + ':');
                                    System.out.println(dsim.listNodeRules(node));
                                }
                            }
                        });
                    } else { System.err.println("Rules: no node names given"); }
                }
            }
        },
        new CmdCommand("tcounts", "tcounts", "Displays transition counts",
            "When enabled, the transition counts for all nodes are reported\n"+
            "by the 'watch' or 'get' commands.  Separate counts are\n"+
            "maintained for transitions 0->1 and 1->0.") {
            public void execute(String args[]) { 
                if (checkDSim()) { dsim.showTCounts(true); }
            }
        },
        new CmdCommand("notcounts", "notcounts", "Hides transition counts",
            "This command disables the output of node transition counts.") {
            public void execute(String args[]) { 
                if (checkDSim()) { dsim.showTCounts(false); }
            }
        },
        new CmdCommand("watchall", "watchall", 
                       "Watches all nodes for transitions") {
            public void execute(String args[]) { 
                if (checkDSim()) { dsim.watchAllNodes(true); }
            }
        },
        new CmdCommand("nowatchall", "nowatchall", 
                       "Stops watching all nodes for transitions") {
            public void execute(String args[]) { 
                if (checkDSim()) { dsim.watchAllNodes(false); }
            }
        },
        new CmdCommand("watch", "watch <node(s)>", 
            "Watches a node for transitions",
            "Enables transition watching on the specified node list. The\n"+
            "format of transition reports is the following:\n\n"+
            "  @TIME #TCOUNT SLEW DELAY_TYPE node:STATE (ALIAS)\n\n"+
            "where TIME is the simulation time of the transition, TCOUNT\n"+
            "is the total count of node's transitions to STATE, which\n"+
            "will be one of {0,1,U}, SLEW is the slew rate of the transition,\n"+
            "DELAY_TYPE is one of {D,E,M} corresponding to digital, estimated,\n"+
            "or measured delays respectively, and ALIAS is the particular\n" +
            "alias of the node that was specified to 'watch'.  If the same\n" +
            "node is specified more than once with different names,\n" +
            "last name takes precedence.\n\n" +
            "TCOUNT is enabled by 'tcounts' and disabled by 'notcounts'.\n" +
            "SLEW is shown when 'measured_delay' is on.\n" +
            "DELAY_TYPE is shown when 'measured_delay' or 'estimated_delay'\n"+
            "is on.") {
            public void execute(String args[]) { 
                if (checkFile()) {
                    if (args!=null && args.length>0) {
                        processNodeArgs(args, 0, args.length, new NodeIterator() {
                            public void processNode(HierName name, Node node) {
                                if (node!=null) { dsim.watchNode(node, name); }
                            }
                        });
                    } else { System.err.println("Watch: no node names given"); }
                }
            }
        },
        new CmdCommand("nowatch", "nowatch <node(s)>", "Stops watching a node") {
            public void execute(String args[]) { 
                if (checkFile()) {
                    if (args!=null && args.length>0) {
                        processNodeArgs(args, 0, args.length, new NodeIterator() {
                            public void processNode(HierName name, Node node) {
                                if (node!=null) { dsim.unwatchNode(node); }
                            }
                        });
                    } else { System.err.println("Usage: "+usage); }
                }
            }
        },
        new CmdCommand("showwatches", "showwatches", 
                       "Lists nodes being watched") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    if (args==null || args.length==0) {
                        dsim.listWatchNodes();
                    } else { System.err.println("Usage: "+usage); }
                }
            }
        },
        new CmdCommand("showbreakpts", "showbreakpts", 
                       "Lists nodes with breakpoints") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    if (args==null || args.length==0) {
                        dsim.listBreakpointNodes();
                    } else { System.err.println("Usage: "+usage); }
                }
            }
        },
        new CmdCommand("nobreakpt", "nobreakpt <node(s)>", 
                       "Clears breakpoints on nodes") {
            public void execute(String args[]) { 
                if (checkFile()) {
                    if (args!=null && args.length>0) {
                        processNodeArgs(args, 0, args.length, new NodeIterator() {
                            public void processNode(HierName name, Node node) {
                                if (node!=null) {
                                    System.out.println("Removed breakpoint for: "+name.getAspiceString());
                                    dsim.breakpointNode(node, false);
                                }
                            }
                        });
                    } else { System.err.println("NoBreakpt: no node names given"); }
                }
            }
        },
        new CmdCommand("breakpt", "breakpt <node(s)>", 
                       "Sets breakpoints on nodes") {
            public void execute(String args[]) { 
                if (checkFile()) {
                    if (args!=null && args.length>0) {
                        processNodeArgs(args, 0, args.length, new NodeIterator() {
                            public void processNode(HierName name, Node node) {
                                if (node!=null) {
                                    System.out.println("Added breakpoint for: "+name.getAspiceString());
                                    dsim.breakpointNode(node, true);
                                }
                            }
                        });
                    } else { System.err.println("Breakpt: no node names given"); }
                }
            }
        },
        new CmdCommand("trace", "trace [--filter-critical] <depth>", 
            "Turns on transition event history",
            "Allocate a ring buffer capable of recording (depth) *\n"+
            "(number of nodes) events, then record all events unless\n"+
            "--filter-critical is specified, in which case a event is only\n"+
            "recorded if it causes a rule to transition.  The log of events\n"+
            "is needed by the 'history' and 'critical' commands.") {
            public void execute(String args[]) { 
                if (checkFile()) {
                    if (args == null || args.length == 0) {
                        System.err.println("Not enough arguments specified");
                        return;
                    }

                    final CommandLineArgs theArgs =
                        new CachingCommandLineArgs(
                            new CommandLineArgsDefImpl(args));
                    final boolean filterCritical =
                        theArgs.argExists("filter-critical");
                    int depth = -1;
                    for (StringContainerIterator it =
                            theArgs.nonParsedArgumentsIterator();
                         it.hasNext(); ) {
                        final String s = it.next();
                        try {
                            depth = Integer.parseInt(s);
                        } catch (NumberFormatException e) {
                            System.err.println("Invalid depth specified: " + s);
                            return;
                        }
                        if (depth < 0) {
                            System.err.println("Depth must be non-negative");
                            return;
                        }
                        if (it.hasNext()) {
                            System.err.println("Too many depths specified");
                            return;
                        }
                    }
                    if (depth < 0) {
                        System.err.println("Depth must be specified");
                        return;
                    }
                    try {
                        dsim.setHistoryPerNode(depth, filterCritical);
                    } catch (Exception e) {
                        System.out.println("Error: "+e.getMessage());
                        if (debug) e.printStackTrace();
                    }
                }
            }
        },
        new RedirectCommand("history", "history <node(s)> [>|>>file]", 
                            "Gets the event history for a node") {
            public void execute(final String args[], final PrintWriter pw) { 
                if (checkFile()) {
                    if (args!=null && args.length>0) {
                        processNodeArgs(args, 0, args.length, new NodeIterator() {
                            public void processNode(HierName name, Node node) {
                                if (node!=null) {
                                    pw.println("History for " +
                                        name.getAsString('.') + ':');
                                    dsim.listNodeHistory(name, false, pw);
                                }
                            }
                        });
                    } else { System.err.println("History: no node names given"); }
                }
            }
        },
        new RedirectCommand("critical", "critical <node(s)> [>|>>file]", 
                            "Gets the previous enablers for a node") {
            public void execute(final String args[], final PrintWriter pw) { 
                if (checkFile()) {
                    if (args!=null && args.length>0) {
                        processNodeArgs(args, 0, args.length, new NodeIterator() {
                            public void processNode(HierName name, Node node) {
                                if (node!=null) {
                                    pw.println("Critical path for " +
                                        name.getAsString('.') + ':');
                                    dsim.listNodeEnablers(name, false, pw);
                                }
                            }
                        });
                    } else { System.err.println("Critical: no node names given"); }
                }
            }
        },
        new CmdCommand("assert", "assert <node> <val> <msg>", 
                       "Prints msg if node is not val") {
            public void execute(String args[]) { 
                if (checkFile()) {
                    if (args!=null && args.length>2) {
                        int len = args.length-1;
                        String msg = args[len--];
                        byte val = parseNodeVal(args[len]);
                        class NodeAssertIterator implements NodeIterator {
                            int val=2; String msg;
                            NodeAssertIterator(int v, String m) { val=v; msg=m; }
                            public void processNode(HierName name, Node node) {
                                if (node!=null && node.getValue()!=val) {
                                    // might be nice to do substitutions for name and val in msg...
                                    System.out.println(msg);
                                } 
                            }
                        };
                        processNodeArgs(args, 0, len, new NodeAssertIterator(val, msg));
                    } else { System.err.println("Assert: insufficient arguments"); }
                }
            }
        },
        new CmdCommand("statistics", "statistics",
                       "Reports statistics about circuit",
       "Note that the reported rule count will likely be larger than what\n" +
       "cast_query --task=prs would report, because of asserted rules (see\n" +
       "\"asserts\") and because DSim converts production rules into DNF,\n" +
       "then treats each disjunct as a rule.") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    System.out.println("Statistics: " +
                                       dsim.countNodes() + " nodes, " +
                                       dsim.countRules() + " rules.");
                    // TODO: more useful statistics, and 
                }
            }
        },
        new CmdCommand("get", "get node(s)", "Gets the value for a node") {
            public void execute(String args[]) { 
                if (checkFile()) {
                    if (args!=null && args.length>0) {
                        processNodeArgs(args, 0, args.length, new NodeIterator() {
                            public void processNode(HierName name, Node node) {
                                if (node!=null) { 
                                    System.out.println(dsim.getNodeString(node)+" ("+name+")");
                                }
                            }
                        });
                    } else { System.err.println("Get: no node names given"); }
                }
            }
        },
        new CmdCommand("set", "set node(s) val", "Sets the value for a node") {
            public String[] getCompletion(String cmd, int start, int end) {
                return null;
                //return dsim.getNodeCompletions(cmd.substring(start, end));
            }
            public void execute(String args[]) { 
                if (checkFile() && args!=null && args.length>0) {
                    int len = args.length-1;
                    byte val=-1;
                    if (len>1 && args[len-1].equals(":")) {
                        // case "set a : 0"
                        args[len-1]=args[len]; len--;
                        val = parseNodeVal(args[len]);
                    } else  {
                        int col = args[len].indexOf(":");
                        int col2 = (len<1) ? 0 : args[len-1].indexOf(":");
                        if (col>=0) {
                            // case "set a:0" or "set a :0"
                            val = parseNodeVal(args[len].substring(col+1));
                            if (col>0) {
                                // case "set a:0"
                                args[len] = args[len].substring(0, col);
                                len++;
                            }
                        } else {
                            // case "set a: 0"
                            if (col2>0) { args[len-1] = args[len-1].substring(0, col2); }
                            // case "set a 0"
                            val = parseNodeVal(args[len]);
                        }
                    }
                    if (len<1) {
                        System.out.println("Set: Insufficient args. Need a node(s) and value.");
                    } else if (val>=0) {
                        class NodeSetIterator implements NodeIterator {
                            byte val=2;
                            NodeSetIterator(byte v) { val=v; }
                            public void processNode(HierName name, Node node) {
                                if (node!=null) { node.setValueAndEnqueueDependents(val); }
                            }
                        };
                        processNodeArgs(args, 0, len, new NodeSetIterator(val));
                    } else { System.out.println("Set: Invalid value for nodes. Must be 0/1/u."); }
                }
            }
        },
        new CmdCommand("comment", "comment <text>", "Ignores text",
            "Ignores text.  Of questionable utility outside command scripts.") {
            public void execute(String args[]) { 
            }
        },
        new CmdCommand("echo", "echo <text>", "Prints text to standard output",
            "Prints text to standard output.  Of questionable utility\n"+
            "outside command scripts.") {
            public void execute(String args[]) { 
                int len = (args!=null) ? args.length : 0;
                String s="";
                for (int i=0; i<len; i++) {s=s+" "+args[i]; }
                System.out.println(s);
            }
        },
        new CmdCommand("aliases", "aliases <node(s)>", "Lists connected nodes",
            "Lists all aliases of the specified node.") {
            public void execute(String args[]) { 
                if (checkFile()) {
                    if (args!=null && args.length>0) {
                        processNodeArgs(args, 0, args.length, new NodeIterator() {
                            public void processNode(HierName name, Node node) {
                                if (node!=null) {
                                    System.out.println("Aliases of " +
                                        name.getAsString('.') + ':');
                                    dsim.listAliases(node);
                                }
                            }
                        });
                    } else { System.err.println("Aliases: no node names given"); }
                }
            }
        },
        new CmdCommand("list", "list [prefix*]", "Lists matching nodes",
            "Lists all aliases that match the specified glob pattern.") {
            public void execute(String args[]) { 
                if (checkFile()) {
                    if (args==null || args.length<1) {
                        args=new String[1]; args[0]="*";
                    }
                    for (int i=0; i<args.length; i++) {
                        if (args[i].indexOf("*")<0) { args[i]=args[i]+"*"; }
                    }
                    processNodeArgs(args, 0, args.length, new NodeIterator() {
                        public void processNode(HierName name, Node node) {
                            if (node!=null) { System.out.println(" "+name); }
                        }
                    });
                }
                //dsim.listNodes(args[0]);
            }
        },
        new CmdCommand("cycle", "cycle [-j] [count [node]]", 
            "Runs the simulator for count cycles",
            "The 'cycle' command instructs DSim to begin simulating node\n"+
            "transitions.  With no arguments, DSim simulates until its\n"+
            "event queue empties (which may be never if there are self-\n"+
            "oscillating circuits in the system).  With a single <count>\n"+
            "argument, DSim simulates for <count> time units.  With both\n"+
            "<count> and <node> arguments specified, DSim will cycle for\n"+
            "<count> cycles on <node>.  In any of these cases, DSim will\n"+
            "return to the command-line prompt if its event queue empties.\n"+
            "Use -j to prevent measure statistics from being reset before\n"+
            "returning to the prompt.") {
            private boolean resetStats = true;
            public void execute(String args[]) { 
                resetStats = true;
                if (checkDSim()) {
                    if (args != null) {
                        ArrayList<String> newArgs =
                            new ArrayList<String>(args.length);
                        for (String arg : args) {
                            if (arg.equals("-j")) resetStats = false;
                            else newArgs.add(arg);
                        }
                        if (newArgs.isEmpty()) args = null;
                        else args = newArgs.toArray(new String[0]);
                    }
                    int val=-1;
                    try {
                        if (args != null) val=validateIntArg(args, 0, true);
                    } catch (Exception e) {
                        System.out.println("Error: count must be an integer." +
                                           "  Usage: " + usage);
                        return;
                    }
                    Node.resetSlewStat();
                    executeSST(SSTOperation.RESUME);
                    clearYieldCounts();
                    DigitalScheduler.InterruptedBy interrupted =
                        DigitalScheduler.InterruptedBy.NONE;
                    if (args!=null && args.length==2) {
                        try {
                          dsim.cycle(args[1], 2*val);
                          interrupted = dsim.isInterrupted();
                        } catch (InvalidHierNameException e) {
                          System.out.println("Error: Invalid node '"+
                                             args[1]+"'.");
                        }
                    } else if (args != null && args.length == 1 &&
                               dsim.getRandom() == DSim.UNTIMED_RANDOM) {
                        System.out.println("Error: Cannot specify how many time units to simulate, since with random on, time is not incremented.");
                    } else if (args != null && args.length > 2) {
                        System.out.println("Error: Too many arguments.  " +
                                           "Usage: " + usage);
                    } else {
                        dsim.cycle(val);
                        interrupted = dsim.isInterrupted();
                    }
                    if (interrupted == DigitalScheduler.InterruptedBy.USER) {
                        System.out.println("*** interrupted by ^C ***");
                    }
                }
            }
            public void postExecute(String args[]) {
                if (utils != null) utils.printMeasureStats(resetStats);
                if (dsim.useDelayMode(DSim.MEASURED_TAU)) {
                    final float[] slewStat = Node.getSlewStat();
                    System.out.println("Slew rate over all rules: (" +
                                       slewStat[0] + ":" +
                                       slewStat[1] + ")");
                }
                executeSST(SSTOperation.PAUSE);
            }
        },
        new CmdCommand("step", "step", "Runs the simulator for one cycle",
            "Instructs DSim to simulate until the node specified by a\n"+
            "prior 'cycle <count> <node>' command has cycled once.") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    dsim.cycle(0);
                }
            }
        },
        new CmdCommand("warnall", "warnall", "Display warnings") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    dsim.setWarn(true);
                    System.out.println("DSim Warnings on.");
                }
            }
        },
        new CmdCommand("nowarnall", "nowarnall", "Ignore warnings") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    dsim.setWarn(false);
                    System.out.println("DSim Warnings off.");
                }
            }
        },
        new CmdCommand("ignoreerror", "ignoreerror", "Don't break on warnings") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    dsim.setError(false);
                    System.out.println("DSim ignoring errors.");
                }
            }
        },
        new CmdCommand("noignoreerror", "noignoreerror", "Break on warnings") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    dsim.setError(true);
                    System.out.println("DSim breaking on errors.");
                }
            }
        },
        new CmdCommand("warn", "warn <on/off>", "Display/ignore warnings") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    boolean val = getBooleanArg(args, 0, dsim.getWarn());
                    dsim.setWarn(val);
                    System.out.println("DSim Warnings "+(dsim.getWarn()?"on":"off")+".");
                }
            }
        },
        new CmdCommand("asserts", "asserts <on/off>", "Model asserted production rules",
                       "The assert{ ... } construct in new cast is compiled down to\n"+
                       "production rules.  Each e1of(N) asserts exclhi() on\n"+
                       "its data rails.  However, aliasing channels does not collapse\n"+
                       "the equivalent asserts.  The cast fragment\n"+
                       "  e1of2 a, b;\n"+
                       "  a = b;\n"+
                       "produces both the rule \"b.1 & b.0 -> ERROR+\" and the\n"+
                       "rule \"a.1 & a.0 -> ERROR+\".  Channels are aliased both\n"+
                       "explicitly and implicitly, in cell instantiation, so asserted\n"+
                       "production rules build up quickly.\n\n\n"+
                       "Whether a given instantiation includes asserted rules depends\n"+
                       "solely on what \"asserts\" was set to at the time of instantiation") {
            public void execute(String args[]) {
                if (checkDSim()) {
                    boolean val = getBooleanArg(args, 0, dsim.getAssertHandling());
                    dsim.setAssertHandling(val);
                    if (val)
                        System.out.println("Will now instantiate asserted production rules");
                    else
                        System.out.println("Will now ignore asserted production rules");
                }
            }
        },
        new CmdCommand("anomalies", "anomalies", "Returns most recent anomalous events",
                       "The 'anomalies' command outputs the most recent anomalous\n"+
                       "events encountered of each anomaly category.  Currently there\n"+
                       "are three such categories defined:\n\n"+
                       "  1. Interference, caused by a node's pull-up and pull-down\n"+
                       "     rules being simultaneously enabled.\n\n"+
                       "  2. Glitching, caused by a node being enabled in one direction\n"+
                       "     for less than its nominal transition time, followed by a\n"+
                       "     re-enabling in the opposite direction (back to its initial\n"+
                       "     state).\n\n"+
                       "  3. Instability, caused by a node being enabled in one direc-\n"+
                       "     tion for less than its nominal transition time, and then\n"+
                       "     left with no rules enabled.  The simulator cannot know\n"+
                       "     what value the node would settle to, so it reports a 'U'.\n\n"+
                       "A call to 'warnall' resets the anomalous event state.") {
            public void execute(String args[]) {
                if (!validateNumArgs(args, 0)) { return; }
                dsim.printAnomalousState();
            }
        },    
        new CmdCommand("load", "load <file>", "Loads a file to simulate",
                       "Loads a file to simulate.  A cell must be instantiated in\n"+
                       "<file> at the top-level scope in order for this command to\n"+
                       "be meaningful.  NOTE: This command is to be deprecated by\n"+
                       "'instantiate'.") {
            public void execute(String args[]) { 
                if (!validateNumArgs(args, 1)) { return; }
                loadFile(args[0]);
            }
        },
        new CmdCommand("norandom", "norandom [seed]",
                       "Use fixed delay times for all transitions",
                       "Use fixed delay times for all transitions.  An optional argument can be used\n" +
                       "to set the seed for the random function in CSP processes.\n") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    if (args != null) {
                        if (args.length > 1) {
                            System.out.println("Usage: "+usage);
                            return;
                        }
                        try {
                            long seed = validateLongArg(args, 0, true);
                            dsim.setRandomSeed(seed);
                        } catch (Exception e) { 
                            System.out.println("Usage: "+usage);
                        }
                    }
                    dsim.setRandom(DSim.NO_RANDOM);
                    System.out.println("DSim Random off.");
                }
            }
        },
        new CmdCommand("random", "random [seed]", "Use random timings") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    if (args != null) {
                        if (args.length > 1) {
                            System.out.println("Usage: "+usage);
                            return;
                        }
                        try {
                            long seed = validateLongArg(args, 0, true);
                            dsim.setRandomSeed(seed);
                        } catch (Exception e) { 
                            System.out.println("Usage: "+usage);
                        }
                    }
                    dsim.setRandom(DSim.UNTIMED_RANDOM);
                    System.out.println("DSim Random on.");
                }
            }
        },
        new CmdCommand("timed_random", "timed_random [seed]", "Use random delay times") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    if (args != null) {
                        if (args.length > 1) {
                            System.out.println("Usage: "+usage);
                            return;
                        }
                        try {
                            long seed = validateLongArg(args, 0, true);
                            dsim.setRandomSeed(seed);
                        } catch (Exception e) { 
                            System.out.println("Usage: "+usage);
                        }
                    }
                    dsim.setRandom(DSim.TIMED_RANDOM);
                    System.out.println("DSim Timed Random on.");
                }
            }
        },
        new CmdCommand("timed_delay", "timed_delay <min> <max>", "Sets the random delay range") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    try {
                        float fast = validateFloatArg(args, 0);
                        float slow = validateFloatArg(args, 1);
                        dsim.setTimedRandom(fast, slow); 
                        System.out.println("DSim timed delay on. ("+fast+", "+slow+")");
                    } catch (Exception e) {
                        System.out.println("Usage: "+usage);
                    }
                }
            }
        },
        new CmdCommand("rand", "rand <on/off>", "Use/dont-use random timings") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    boolean val = getBooleanArg(args, 0, dsim.getRandom()!=DSim.NO_RANDOM);
                    dsim.setRandom(val ? DSim.UNTIMED_RANDOM : DSim.NO_RANDOM );
                    System.out.println("DSim Random "+(dsim.getRandom()==DSim.UNTIMED_RANDOM?"on":"off")+".");
                }
            }
        },
        new CmdCommand("verbose", "verbose <on/off>", "Display/ignore info",
                       "Enables or disables extremely verbose reporting of DSim internal\n"+
                       "state.  Intended only for Java debugging purposes.") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    boolean val = getBooleanArg(args, 0, dsim.getVerbose());
                    dsim.setVerbose(val);
                    System.out.println("DSim: "+(dsim.getVerbose()?"Maximum":"Minimum")+" Verbosity.");
                }
            }
        },
  	new CmdCommand("haltonconflict", "haltonconflict <on/off>",
				"Halt on cosimulation conflict",
				"Controls whether a simulation halts or " +
				"continues on a cosimulation conflict.") {
	    public void execute(String args[]) {
	 	if (checkDSim()) {
		    boolean val = getBooleanArg(args, 0, dsim.haltOnConflict);
		    dsim.haltOnConflict = val;
		    System.out.println("DSim: " + 
			(dsim.haltOnConflict ? "halting" : "continuing") + 
			" on conflict.");
		}
	    }
	},
        new CmdCommand("haltonerror", "haltonerror <on/off>",
                       "Halt on CSP error statement",
                       "Controls whether a simulation halts or " +
                       "continues on executing a CSP error\nstatement.") {
            public void execute(String args[]) {
                 if (checkDSim()) {
                    boolean val = getBooleanArg(args, 0, dsim.haltOnError);
                    dsim.haltOnError = val;
                    System.out.println("DSim: " + 
                        (dsim.haltOnError ? "halting" : "continuing") + 
                        " on CSP error statement.");
                }
            }
        },
        new CmdCommand(
            "haltoncycle",
            "haltoncycle <node(s)> ( off | ( < | <= | == | != | >= | > ) <tpc> )",
            "Halt when transitions per cycle meets condition",
            "Configures whether a cycle command should terminate based on " +
            "the number of\n"+ 
            "transitions in the last cycle of specified nodes.") {
            public void execute(String args[]) {
                if (checkDSim()) {
                    final int end;
                    final NodeIterator action;
                    if (args == null) {
                        System.out.println("Usage: "+usage);
                        return;
                    }

                    if (utils == null) utils = new DSimUtil();
                    if (args.length >= 1 &&
                        args[args.length - 1].equals("off")) {
                        end = args.length - 1;
                        action = new NodeIterator() {
                            public void processNode(HierName name, Node node) {
                                if (node != null) utils.haltOff(node);
                            }
                        };
                    } else {
                        if (args.length < 2) {
                            System.out.println("Usage: "+usage);
                            return;
                        }

                        final BinaryPredicate p;
                        try {
                            p = NumericPredicate.getPredicate(
                                    args[args.length - 2]);
                        } catch (IllegalArgumentException e) {
                            System.err.println("Expecting \"off\" or " +
                                               "relational operator, found " +
                                               args[args.length - 2]);
                            return;
                        }

                        final int tpc;
                        try {
                            tpc = validateIntArg(args, args.length - 1);
                        } catch (Exception e) {
                            return;
                        }

                        end = args.length - 2;
                        action = new NodeIterator() {
                            public void processNode(HierName name, Node node) {
                                if (node != null) utils.haltOn(node, p, tpc);
                            }
                        };
                    }
                    processNodeArgs(args, 0, end, action);
                }
            }
        },
        new CmdCommand("reset", "reset [protocol]", 
            "Resets DSim with a standard reset protocol",
            "When protocol==0 (the default), the following reset procedure\n"+
            "is executed:\n\n"+
            "  random\n" +
            "  set _Reset:0 GND:0 Vdd:1\n" +
            "  cycle\n" +
            "  status U\n" +
            "  warnall; noignoreerror\n" +
            "  set _Reset:1\n\n" +
            "When protocol==1, the old-style _PReset/_SReset reset procedure\n"+
            "is executed:\n\n"+
            "  random\n"+
            "  set _SReset:0 _PReset:0 GND:0 Vdd:1\n" +
            "  cycle\n" +
            "  status U\n" +
            "  warnall; noignoreerror\n" +
            "  set _PReset:1\n" +
            "  cycle\n" +
            "  status U\n" +
            "  set _SReset:1\n\n" +
            "In each case, the state of 'warn', 'ignorerror', and 'rand' is\n"+
            "restored to whatever it was prior to the reset.  If castversion\n"+
            "is 1, the default protocol is 1; otherwise, 0 is the default.") {
            public void execute(String args[]) { 
                if (!checkFile()) return;
                //was:
                // resetDSim(castVersion);
                int protocol = DSimUtil.STANDARD_RESET;
                if (castVersion != null && castVersion.equals("1")) {
                    protocol = DSimUtil.SP_RESET;
                }
                if (args != null) {
                    if (args.length > 1) {
                        System.out.println("Usage: "+usage);
                        return;
                    }
                    if (args[0].equals("0")) {
                        protocol = DSimUtil.STANDARD_RESET;
                    }
                    else if (args[0].equals("1")) {
                        protocol = DSimUtil.SP_RESET;
                    }
                    else {
                        System.out.println("Valid protocols: 0,1.");
                        return;
                    }
                }
                try {
                    if (utils == null) utils = new DSimUtil();
                    utils.resetDSim(protocol);
                }
                catch (Exception e) {
                    System.out.println("Error: "+e.getMessage());
                    if (debug) {
                        e.printStackTrace();
                    }
                }
            }
        },
        
	
        new CmdCommand("mem", "mem", "Display amounts of used, free, and total memory",
                       "Displays the amount of used memory, free memory, and total memory." ) {
                public void execute( String args[] ) {
                    System.gc();
                    final long total = Runtime.getRuntime().totalMemory() / 1024;
                    final long free = Runtime.getRuntime().freeMemory() / 1024;
                    final long used = total - free;
                    System.out.println( "Used: " + used + "k" );
                    System.out.println( "Free: " + free + "k" );
                    System.out.println( "Total: " + total + "k" );
                }
            },

	new CmdCommand("clear", "clear", "Deprecated.",
                   "Clears all nodes and rules from DSim.  Allows cells to be re-\n"+
                   "instantiated without needing to parse CAST files a second time.\n" +
                   "This is now deprecated because clearing happens automatically\n" +
                   "before an instantiate or cosimulate command.") {
            public void execute(String args[]) { 
                if (!checkDSim()) return;
                System.out.println("\"clear\" is deprecated, since instantiate and cosimulate clears automatically.");
                clear();
            }
	},
        
        new CmdCommand("getchan", "getchan <channel(s)>",
                "Prints the value(s) on the passed channel(s)",
                "Command allows one to inspect the integer representation of a DI channel\n" +
                "instead of having to look at each one of the rails") {
            public void execute(String args[]) { 
                if (args == null) {
                    System.out.println("Usage: " + usage);
                } else {
                    for (String chan : args) {
                        int value = dsim.getChannel(chan);
                        switch (value) {
                          case -1: System.out.println(chan + ": neutral");
                                   break;
                          case -2: System.out.println("WARNING: channel " +
                                                      chan + " undefined");
                                   break;
                          case -3: System.out.println(chan + ": invalid");
                                   break;
                          default: System.out.println(chan + ": " + value);
                        }
                    }
                }
            }
	},

	new CmdCommand("getchan_arr", "getchan_arr", "Prints the value on the passed array",
		       "Command allows one to inspect the value on multiple channels as \n"
		       +"as an array. It assumes the LSB is zero indexed" ) {
		public void execute(String args[]) { 
		    System.out.println("NO IMPLEMENTED");
	    }
       },

        new CmdCommand("clearq", "clearq", "Deprecated.",
                   "Deprecated" ) {
            public void execute(String args[]) { 
                if (!checkDSim()) return;
                dsim.clearEventQueues();
            }
        },

	new CmdCommand("clear_tcounts", "clear_tcounts", "Clear tcounts for all nodes",
                   "Clears tcounts for all nodes in DSim.  Enables transitions during reset\n"+
		       "to be ignored.") {
            public void execute(String args[]) { 
                dsim.clear_tcounts();
            }
        },

        new CmdCommand("tcounts_total", "tcounts_total", "Display total transition counts for all nodes",
                   "Displays the total number of transistion counts for all nodes in DSim.") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    if (args != null && args.length > 0) {
                        final long[] result = { 0 };
                        processNodeArgs(args, 0, args.length, new NodeIterator() {
                            public void processNode(HierName name, Node node) {
                                if (node != null) {
                                    result[0] += node.getTCount();
                                }
                            }
                        });
                        System.out.println("Total transitions: " + result[0]);
                    } else { System.err.println("tcounts_total: no node names given"); }
                }
            }
        },
	
        new CmdCommand("status", "status <0/1/U>", "List all nodes with given value") {
            public void execute(String args[]) { 
                if (checkFile() && validateNumArgs(args, 1)) {
                    dsim.listNodes(Node.getValueForName(args[0]));
                }
            }
        },
        new CmdCommand("time", "time", "Returns the current DSim time") {
            public void execute(String args[]) { 
                if (!checkDSim()) return;
                System.out.println(
                "DSim current time (from scheduler) = "+dsim.getTime());
            }
        },
        new CmdCommand("set_time", "set_time time", "Sets the current DSim time",
                       "Sets the current DSim time and clears event queues to avoid timing\n"+
                       "conflicts.  This command is for debugging.") {
            public void execute(String args[]) {
                if (checkDSim()) {
                    int val=getIntArg(args, 0, -1); 
                    if (args!=null && args.length==1)
                        dsim.setTime(val);
                    else
                        System.out.println("bad args");
                }
                else
                    System.out.println("Error: no dsim created yet");
            }
        },
        new CmdCommand("debug", "debug <on/off>", 
            "Enables or disables debugging output",
            "When debug mode is enabled, all exception stack traces are\n"+
            "printed.  Users tend to find this really annoying, so the\n"+
            "default is off.") {
            public void execute(String args[]) { 
                if (args == null || args.length > 1 || 
                    (!args[0].equals("on") && !args[0].equals("off"))) {
                    System.out.println("Usage: "+usage);
                    return;
                }
                checkDSim();
                debug = args[0].equals("on") ? true : false;
                dsim.setDebug(debug);
            }
        },
        new CmdCommand("reset_cycle", "reset_cycle <N> <M> <node> [protocol]",
            "Repeatedly resets an instantiated cell",
            "Performs a reset stress test of the instantiated cell. Repeat-\n"+
            "edly resets the system with full-random timing, cycling for\n"+
            "some length of time between resets in whatever timing mode is\n"+
            "currently selected.\n\n"+
            "  N        - Number of reset cycles to do.\n"+
            "  M        - Number of out-of-reset cycles to run\n"+
            "             between each reset.\n"+
            "  node     - Node to use for out-of-reset cycling.\n"+
            "  protocol - Reset protocol to use (see 'reset').\n"+
            "             Defaults to 0.") {
            public void execute(String args[]) {
                if (args == null || args.length < 3 || args.length > 4) {
                    System.out.println("Usage: "+usage);
                    return;
                }
                int numTests  = getIntArg(args,0,-1);
                int numCycles = getIntArg(args,1,-10);

                int protocol = DSimUtil.STANDARD_RESET;
                if (args.length == 4) {
                    protocol = getIntArg(args,3,-1);
                }

                if (numTests == -1 || numCycles == -10 || protocol == -1) {
                    System.out.println("Usage: "+usage);
                }

                try {
                    if (utils == null) utils = new DSimUtil();
                    boolean done = 
			utils.resetStressTest(numTests,numCycles,args[2],protocol);
		    if(!done){
			System.out.println("Reset Stress Test Deadlocked");	
		    }
		    
                }
                catch (Exception e) {
                    System.out.println("Error: "+e.getMessage());
                    if (debug) {
                        e.printStackTrace();
                    }
                }
            }
        },
        new CmdCommand("measure", "measure [-v] [-c] [-clear] [<node>]",
            "Measure node transition statistics",
            "Gathers statistics of transitions on <node>.  Namely, calculates\n"+
            "the average period, and tallies the distribution of cycle times\n"+
            "contributing to that average.  Statistics will be reported when\n"+
            "returning to the command prompt following a 'cycle' command.  The\n"+
            "statistics are recalculated on each cycling.\n\n"+
            "Options:\n\n"+
            "  -v       - Verbose output mode (provides cycle time distribution)\n"+
            "  -c       - Continuous measurement mode.\n"+
            "  -clear   - Clear all node measures.") {
            public void execute(String args[]) {
                if (args == null || args.length == 0) {
                    System.out.println("Usage: "+usage);
                    return;
                }
                if (utils == null) utils = new DSimUtil();
                int i = 0;
                boolean verbose = false;
                boolean continuous = false;
                while (i != args.length) {
                    if (args[i].startsWith("-")) {
                        if (args[i].substring(1).equals("v"))
                            verbose = true;
                        else if (args[i].substring(1).equals("clear"))
                            utils.clearMeasures();
                        else if (args[i].substring(1).equals("c"))
                            continuous = true;
                        else {
                            System.out.println("Usage: "+usage);
                            return;
                        }
                    }
                    else {
                        utils.addMeasure(args[i],continuous);
                    }
                    i++;
                }
                utils.setMeasureVerbosity(verbose);
            }
        },
        new CmdCommand("cosimulate",
                "cosimulate cell_type " +
                    "[ \"{\" cosimspec_list \"}\" ] [ : envname " +
                    "[ \"{\" envspec \"}\" ] ]",
                "Instantiates a cell",
                "Instantiates cell in environment named envname, and\n" +
                "prepares cosimulation according to cosimspec_list.\n" +
                "\n" +
                "Grammar for the cosimspec_list is:\n" +
                "cosimspec_list ::= cosimspec { \"|\" cosimspec }\n" +
                "cosimspec ::= levelspec { \"-\" instspec }\n" +
                "levelspec ::= integer [ \"{\" cosimspec_list \"}\" ] " +
                    "| mode_list\n" +
                "mode_list ::= mode { \",\" mode }\n" +
                "mode ::= \"java\" | \"csp\" | \"subcells\" | \"prs\" " +
                    "| \"spice\" | \"verilog\" \".\" level\n" +
                "instspec ::= cell_id \"{\" cosimspec_list \"}\"\n" +
                "cell_id ::= instance | fqcn\n" +
                "envspec ::= envlevelspec { \"-\" envinstspec }\n" +
                "envlevelspec ::= integer [ \"{\" mode_list \"}\" ] " +
                    "| mode_list\n" +
                "envinstspec ::= cell_id \"{\" mode_list \"}\"\n" +
                "\n" +
                "See http://internal.avlsi.com/tree/sw/cad/doc/" +
                    "specs/cast/cosim.html\n" +
                "for the complete specification.") {
            public void execute(final String[] args) {
                if (args == null) {
                    System.out.println("Usage: " + usage);
                    return;
                }

                // fill in member variable dsim
                checkDSim();

                // glom all args together
                final StringBuffer sb = new StringBuffer();
                for (int i = 0; i < args.length; ++i) {
                    sb.append(args[i]);
                    sb.append(' ');
                }

                clear();
                cosimulate(sb.toString(), "x");
            }
        },
        new CmdCommand("devices",
                "devices [-c] [-l] [-p] [-r] [<device glob>]",
                "Lists all java or csp devices that match",
                "Lists all java or csp devices that match the glob.  If no glob is\n" +
                "specified, then all devices are printed.\n" +
               "    -c    don't show channel operation\n" +
               "    -l    show channel fill information, incompatible with -c\n" +
               "    -p    don't show parse position\n" +
               "    -r    interpret the glob as a regular expression\n" +
               "    -f    print full path name when showing parse position\n" +
               "A leading asterisk indicates a device may be livelocked (it did\n" +
               "not start a new iteration of an infinite loop during the last\n" +
               "cycle command).") {
            public void execute(final String[] args) {
                // check args
                String regex = null;
                boolean glob = true;
                boolean showPos = true;
                boolean showFill = false;
                boolean showChan = true;
                boolean showFull = false;
                if (args != null) {
                    for (int i = 0; i < args.length; ++i) {
                        if (args[i].equals("-c")) {
                            showChan = false;
                        } else if (args[i].equals("-l")) {
                            showFill = true;
                        } else if (args[i].equals("-p")) {
                            showPos = false;
                        } else if (args[i].equals("-r")) {
                            glob = false;
                        } else if (args[i].equals("-f")) {
                            showFull = true;
                        } else {
                            if (regex == null) {
                                regex = args[i];
                            } else {
                                System.out.println("Usage: " + usage);
                                return;
                            }
                        }
                    }
                }
                printDevices(glob, showPos, showFill, showChan, showFull,
                             regex);
            }
        },
        new RedirectCommand("csp",
                "csp [<device>] <command> [>|>>file]",
                "Print stack trace, get/set variables, get energy for CSP devices",
                "Available commands are:\n" +
                "  where\n" +
                "  get [[<frame>:]<variable>]\n" +
                "  set [<frame>:]<variable> <value>\n" +
                "  energy [reset]\n\n" +
                "<device> is a glob expression that selects devices.\n" +
                "<frame> is the stack frame number from where; defaults to the bottom frame.\n" +
                "<variable> is a glob expression that selects variables.\n" +
                "<value> can be an integer (use 0x or 0b prefix for hexadecimal or binary) or\n" +
                "\"true\" or \"false\".  The boolean literals can be used if and only if the\n" +
                "variable is declared as a bool.\n\n" +
                "Use \"where\" to get a stack trace; use \"get\" or \"set\" to get or set\n" +
                "int or bool variables; use \"energy\" to get the accumulated energy in\n" +
                "joules; use \"energy reset\" to reset the accumulated energy to 0.\n" +
                "This command can only be used after reset") {
            private Pair<Integer,String> getVariable(final String s) {
                final int idx = s.indexOf(':');
                int frame = -1;
                final String regex;
                if (idx > 0) {
                    final String sframe = s.substring(0, idx);
                    try {
                        frame = Integer.parseInt(sframe);
                    } catch (NumberFormatException e) {
                        System.out.println("Stack frame number is not an " +
                                           "integer: " + sframe);
                        return null;
                    }
                    if (frame < 0) {
                        System.out.println("Stack frame number must be " +
                                           "non-negative: " + frame);
                        return null;
                    }
                    regex = s.substring(idx + 1);
                } else {
                    regex = s;
                }
                return new Pair<Integer,String>(frame, regex);
            }
            public void execute(final String[] args, final PrintWriter pw) {
                if (args == null || args.length < 2) {
                    System.out.println("Usage: " + usage);
                    return;
                }

                try {
                    final TreeSet<String> devices =
                        new TreeSet<String>(
                            NaturalStringComparator.getInstance());
                    matchingDevices(true, args[0], devices, true);
                    if (devices.isEmpty()) {
                        System.out.println("No matching device found.");
                        return;
                    }

                    final String cmd = args[1];
                    if (cmd.equals("get")) {
                        final Pair<Integer,String> p =
                            getVariable(args.length < 3 ? "*" : args[2]);
                        if (p == null) return;
                        final int frame = p.getFirst();
                        final String regex = p.getSecond();

                        final Pattern pat =
                            Pattern.compile(StringUtil.getRegexForGlob(regex));
                        cspGet(devices, frame, pat, pw);
                    } else if (cmd.equals("set")) {
                        if (args.length != 4) {
                            System.err.println("Not enough arguments to set.");
                            return;
                        }

                        final Pair<Integer,String> p = getVariable(args[2]);
                        if (p == null) return;
                        final int frame = p.getFirst();
                        final String regex = p.getSecond();
                        final Pattern pat =
                            Pattern.compile(StringUtil.getRegexForGlob(regex));

                        boolean isBool = true;
                        CspInteger val;
                        if (args[3].equals("true")) {
                            val = CspInteger.TRUE;
                        } else if (args[3].equals("false")) {
                            val = CspInteger.FALSE;
                        } else {
                            String s;
                            final int radix;
                            if (args[3].startsWith("0b")) {
                                s = args[3].substring(2);
                                radix = 2;
                            } else if (args[3].startsWith("0x")) {
                                s = args[3].substring(2);
                                radix = 16;
                            } else {
                                s = args[3];
                                radix = 10;
                            }
                            s = s.replace("_", "");
                            try {
                                val = new CspInteger(new BigInteger(s, radix));
                                isBool = false;
                            } catch (NumberFormatException e) {
                                System.err.println(
                                        "Value is expected to be \"true\", " +
                                        "\"false\", or a number: " + args[3]);
                                return;
                            }
                        }

                        cspSet(devices, pat, frame, val, isBool, pw);
                    } else if (cmd.equals("where")) {
                        cspWhere(devices, pw);
                    } else if (cmd.equals("energy")) {
                        boolean reset = false;
                        if (args.length == 3) {
                            if (args[2].equals("reset")) {
                                reset = true;
                            } else {
                                System.err.println(
                                        "Invalid argument to energy " +
                                        "(only \"reset\" accepted): " +
                                        args[2]);
                                return;
                            }
                        } else if (args.length > 3) {
                            System.err.println("Too many arguments to energy.");
                            return;
                        }
                        cspEnergy(devices, reset, pw);
                    } else {
                        System.out.println("Unknown command: " + cmd);
                        return;
                    }
                } catch (PatternSyntaxException e) {
                    System.out.println("Invalid pattern: " + e.getMessage());
                }
            }
        },
        new CmdCommand("sst2",
                "sst2 <dir> [<timescale>]",
                "Configures directory to which sst2 logs",
                "Configures directory to which sst2 logs.  The database will be closed\n" +
                "when DSim exits or when a cosimulate or instantiate command is executed.\n\n" +
                "If <dir> starts with ~/, ~ will be expanded to the user's home directory,\n" +
                "however, the ~user syntax is not supported.  If <dir> does not exist, it\n" +
                "will be created, including any intermediate directories that do not exist.\n" +
                "An optional timescale in pS per DSim unit specifies the conversion factor\n" +
                "to real time; it defaults to 0.5 (50pS per 100 DSim units), or when\n" +
                "measured_delay is enabled, 0.01 (1pS per 100 DSim units)"
                ) {
            public void execute(final String[] args) {
                // check args
                if (args == null || (args.length != 1 && args.length != 2)) {
                    System.out.println("Usage: " + usage);
                    return;
                }

                if (args[0].equals("~")) {
                    args[0] = System.getProperty("user.home");
                } else if (args[0].startsWith("~/")) {
                    args[0] = System.getProperty("user.home") +
                              args[0].substring(1);
                } else if (args[0].startsWith("~")) {
                    System.out.println(
                            "ERROR: ~user expansion is not supported");
                    return;
                }

                // fill in member variable dsim
                checkDSim();

                double timescale =
                    dsim.useDelayMode(DSim.MEASURED_TAU) ? 0.01 : 0.5;
                if (args.length == 2) {
                    try {
                        timescale = validateFloatArg(args, 1);
                    } catch (Exception e) {
                        System.out.println("Usage: " + usage);
                    }
                }

                final File logDir = new File(args[0]);
                if (logDir.exists()) {
                    if (!logDir.isDirectory()) {
                        System.out.println("ERROR: " + args[0] +
                                           " already exists and is a file");
                        return;
                    }
                } else {
                    if (!logDir.mkdirs()) {
                        System.out.println("ERROR: cannot create directory " +
                                           args[0]);
                        return;
                    }
                }
                if (!logDir.canExecute() || !logDir.canWrite()) {
                    System.out.println("ERROR: directory " + args[0] +
                                       " has incorrect permissions" +
                                       " (must be writable and executable)");
                    return;
                }

                try {
                    dsim.openSigscan(args[0], timescale * 1e-12);
                } catch (SigscanException e) {
                    System.out.println("ERROR: " + e.getMessage());
                    System.out.println("******************************************************************************");
                    System.out.println("***** THIS SST2 ERROR STATE IS NOT RECOVERABLE UNLESS YOU EXIT FROM DSIM *****");
                    System.out.println("******************************************************************************");
                    if (debug)
                        e.printStackTrace();
                }
            }
        },
        new CmdCommand("log_nodes",
                "log_nodes [-f] [name1] .. [nameN]",
                "Enables SST2 logging of all node events",
                "Enables SST2 logging of all node events whose node names\n" +
                "match the search string (can use ? and * wildcards),\n"+
                "default is all nodes.  Use -f to treat names as literals.\n" +
                "WARNING: this is an expensive operation, unless -f is used!") {
            public void execute(final String[] args) {
                // check args
/*                if (args != null) {
                    System.out.println("Usage: " + usage);
                    return;
                }
*/
                // fill in member variable dsim
                checkDSim();

                if (dsim.getSigscan() == null) {
                    System.err.println("SST2 logging disabled.  " +
                            "Enable it with the sst2 command.");
                    return;
                }
                executeSST(SSTOperation.RESUME);
                try {
                    if (args == null) dsim.logSignals("*");
                    else {
                        boolean fixed = false;
                        if (args.length > 0 && args[0].equals("-f")) {
                            fixed = true;
                            if (args.length == 1) {
                                System.err.println(
                                    "Nodes to log must be specified with -f");
                                return;
                            }
                        }
                        if (fixed) {
                            int logged = 0;
                            for (int loop=1;loop<args.length;loop++) {
                                if (dsim.logSignalsFixed(args[loop])) logged++;
                            }
                            System.out.println(
                                    logged + " nodes added to database");
                        } else {
                            for (int loop=0;loop<args.length;loop++) {
                                dsim.logSignals(args[loop]);
                            }
                        }
                    }
                } catch (IllegalStateException e) {
                    System.out.println(e.getMessage());
                    return;
                } finally {
                    executeSST(SSTOperation.PAUSE);
                }
            }
        },
        new CmdCommand("log",
                "log <device> [ on | off | query ]",
                "Configures a device's SST2 logging",
                "Configures the logging to the SST2 database of a device's " +
                "operations.  <device> can be a glob expression.") {
            public void execute(final String[] args) {
                // check args
                if (args == null || (args.length != 1 && args.length != 2)) {
                    System.out.println("Usage: " + usage);
                    return;
                }

                final String deviceNameArg = args[0];

                // fill in member variable dsim
                checkDSim();

                if (dsim.getSigscan() == null) {
                    System.err.println("SST2 logging disabled.  " +
                            "Enable it with the sst2 command.");
                    return;
                }

                // "*" means all devices, otherwise it's a single device
                final TreeSet<String> devices =
                    new TreeSet<String>(NaturalStringComparator.getInstance());
                matchingDevices(true, deviceNameArg, devices, false);
                if (devices.isEmpty()) {
                    System.out.println("No matching device found.");
                    return;
                }

                executeSST(SSTOperation.RESUME);
                try {
                    for (String deviceName : devices) {
                        // get device
                        final AbstractDevice device =
                            dsim.getDevice(deviceName);

                        if (device == null) {
                            System.out.println("Device " + deviceName +
                                               " not found.");
                            return;
                        }

                        final String command;
                        if (args.length == 1)
                            command = "on";
                        else
                            command = args[1];

                        final DebugOpts debugOpts = device.getDebugOpts();

                        if (command.equals("query")) {
                            queryDebugOpts(deviceName, debugOpts);
                        } else {
                            final boolean log;
                            if (command.equals("on"))
                                log = true;
                            else if (command.equals("off"))
                                log = false;
                            else {
                                System.out.println("Usage: " + usage);
                                return;
                            }
                            
                            // set up the device's sigscan database
                            device.setSigscan(log ? dsim.getSigscan() : null);
                            
                            // set all SST2 logging methods to appropriate state
                            debugOpts.setLogBuses(log);
                            debugOpts.setLogMsgs(log);
                            debugOpts.setLogVars(log);
                            debugOpts.setLogTrans(log);
                            
                            // print out new status
                            queryDebugOpts(deviceName, debugOpts);
                        }
                    }
                } finally {
                    executeSST(SSTOperation.PAUSE);
                }
            }

            private void queryDebugOpts(
                    final String deviceName,
                    final DebugOpts debugOpts) {
                final boolean logBuses = debugOpts.loggingBuses();
                final boolean logMsgs = debugOpts.loggingMsgs();
                final boolean logVars = debugOpts.loggingVars();
                final boolean logTrans = debugOpts.loggingTrans();

                if (logBuses == logMsgs &&
                    logBuses == logVars &&
                    logBuses == logTrans) {
                    System.out.println(loggingString(deviceName,
                                "SST2", logBuses));
                } else {
                    System.out.println(loggingString(deviceName,
                                "Bus", logBuses));
                    System.out.println(loggingString(deviceName,
                                "Message", logMsgs));
                    System.out.println(loggingString(deviceName,
                                "Variable", logVars));
                    System.out.println(loggingString(deviceName,
                                "Transaction", logTrans));
                }
            }

            private String loggingString(
                    final String deviceName,
                    final String loggingType,
                    final boolean isEnabled) {
                return loggingType + " logging is " +
                    (isEnabled ? "on" : "off") + " for " + deviceName;
            }
        },
        new CmdCommand("log_channels",
                "log_channels <device> ( on | off | query | ? )",
                "Configures a device's logging",
                "Configures the logging to the screen of a device's " +
                "channel operations.  <device> can be a glob expression.") {
            public void execute(final String[] args) {
                // check args
                if (args == null || args.length != 2) {
                    System.out.println("Usage: " + usage);
                    return;
                }

                final String deviceNameArg = args[0];

                // fill in member variable dsim
                checkDSim();

                // "*" means all devices, otherwise it's a single device
                final TreeSet<String> devices =
                    new TreeSet<String>(NaturalStringComparator.getInstance());
                matchingDevices(true, deviceNameArg, devices, false);
                if (devices.isEmpty()) {
                    System.out.println("No matching device found.");
                    return;
                }

                for (String deviceName : devices) {
                    // get device
                    final AbstractDevice device = dsim.getDevice(deviceName);

                    if (device == null) {
                        System.out.println("Device " + deviceName +
                                           " not found.");
                        return;
                    }

                    if (args[1].equals("query")||args[1].equals("?")) {
                        System.out.println("Logging is " +
                                           (device.getDebugOpts().loggingScreen() ?
                                            "on" : "off") + 
                                           " for " + deviceName);
                        
                    } else {
                        final boolean logScreen;
                        if (args[1].equals("on"))
                            logScreen = true;
                        else if (args[1].equals("off"))
                            logScreen = false;
                        else {
                            System.out.println("Usage: " + usage);
                            return;
                        }

                        device.getDebugOpts().setLogScreen(logScreen);
                    }
                }
            }
        },
        new CmdCommand("csp_coverage", 
                       "csp_coverage ( on | off | query | list [ counts ] | \n"+
                       "                        clear ( hits | all ) | setpath PATH )",
		       "Configure code coverage options for CSP blocks",
		       "Configure code coverage options for CSP blocks. \n"+
		       "  on, off      - turn on or off insertion of coverage probes in CSP2Java\n"+
		       "  query        - print information about coverage status\n"+
		       "  list         - list locations of unreached coverage probes\n"+
                       "  clear hits   - clear all hits\n"+
                       "  clear all    - clear probe table as well (warning: only do this before any\n"+
                       "                 CSP is compiled to Java)\n"+
		       "  setpath PATH - set the name of the file in which to store the data\n") {
            public void execute(String args[]) {
		if (args == null) {
		    System.out.println("Usage: "+usage);
		    return;
		}

                // fill in member variable dsim
		// XXXX: is this enough?
                checkDSim();

		String command;
                // default command
		if (args.length==0) command="on";
		else command=args[0];
                // commands with one arguments
		if(command.equals("setpath")) {
		    String path=Monitor.defaultPath;
		    if(args.length>2) {
			System.out.println("Usage: "+usage);
			return;
		    }
		    if(args.length>1) {
			path=args[1];
		    }
		    if(dsim.emitCSPCoverageProbes) {
			saveCSPCoverageData();
		    }
		    Monitor.global.setPath(path);
		    if(dsim.emitCSPCoverageProbes) {
			loadCSPCoverageData();
		    }
		    return;
		} else if(command.equals("clear")) {
                    if(args.length>2) {
			System.out.println("Usage: "+usage);
			return;
                    }
                    String what;
                    if(args.length==2)
                        what=args[1];
                    else
                        what="hits";
                    
                    boolean savedEmitCoverage=false;
                    try {
                        savedEmitCoverage=dsim.emitCSPCoverageProbes;
                        if(!dsim.emitCSPCoverageProbes) {
                            loadCSPCoverageData();
                            dsim.emitCSPCoverageProbes=true;
                        }
                        if(what.equals("all")) {
                            Monitor.global.clearAll();
                        } else if(what.equals("hits") || what.equals("hit")) {
                            Monitor.global.clearHits();
                        } else {
                            // XXX: we could also add functionality to
                            // clear only one type at a time
                            System.out.println("Usage: "+usage);
                            return;
                        }
                    } finally {
                        if(!savedEmitCoverage) {
                            saveCSPCoverageData();
                            dsim.emitCSPCoverageProbes=false;
                        }
                    }
                    return;
		} else if(command.equals("list")) {
                    if(args.length>2) {
			System.out.println("Usage: "+usage);
			return;
                    }
                    String what;
                    if(args.length==2)
                        what=args[1];
                    else
                        what="misses";
                    
		    boolean savedEmitCoverage=dsim.emitCSPCoverageProbes;
		    if(!dsim.emitCSPCoverageProbes) {
			loadCSPCoverageData();
			dsim.emitCSPCoverageProbes=true;
		    }
                    if(what.equals("misses")) {
                        Monitor.global.listMisses(System.out);
                    } else if(what.equals("counts")) {
                        Monitor.global.listCounts(System.out);
                    } else {
                        System.out.println("Usage: "+usage);
                    }
		    dsim.emitCSPCoverageProbes=savedEmitCoverage;
                    return;
                }
                // commands with 0 arguments
		if(args.length>1) {
		    System.out.println("Usage: "+usage);
		    return;
		}
		if (command.equals("on")) {
		    if(!dsim.emitCSPCoverageProbes) {
			loadCSPCoverageData();
			dsim.emitCSPCoverageProbes=true;
		    }
		} else if(command.equals("off")) {
		    if(dsim.emitCSPCoverageProbes) {
			dsim.emitCSPCoverageProbes=false;
			saveCSPCoverageData();
		    }
		} else if(command.equals("query")) {
		    System.out.println("Coverage is "+(dsim.emitCSPCoverageProbes?"on":"off"));
		    System.out.println("Storing coverage data in "+Monitor.global.getPath());
		} else {
		    System.out.println("Usage: "+usage);
		    return;
		}
            }
        },
        new CmdCommand("accurate_wait", "accurate_wait ( on | off )",
                       "Control whether csp and WaitFactory waits are accurate",
                       "Control whether WaitFactories use AccurateWait or Wait.\n" +
                       "Affects use of WaitFactory in Java as well as csp non-deterministic " +
                       "selection, i.e. *[ ... ]") {
            public void execute(final String[] args) {
                // check args
                if (args == null || args.length != 1) {
                    System.out.println("Usage: " + usage);
                    return;
                }

                // fill in member variable dsim
                checkDSim();

                final String cmd = args[0];
                if (cmd.equals("on")) {
                    System.out.println("Enabling AccurateWait");
                    WaitFactory.setWait(AccurateWait.class);
                } else if (cmd.equals("off")) {
                    System.out.println("Disabling AccurateWait");
                    WaitFactory.setWait(Wait.class);
                } else {
                    System.out.println("Usage: " + usage);
                    return;
                }
            }
        },
        new CmdCommand("file_cache",
                "file_cache ( on | off | flush | query )",
                "Configure file cache state",
                "Configure file cache state.") {
            public void execute(final String[] args) {
                // check args
                if (args == null || args.length != 1) {
                    System.out.println("Usage: " + usage);
                    return;
                }

                // fill in member variable dsim
                checkDSim();

                final String cmd = args[0];
                if (cmd.equals("query")) {
                    System.out.println("File cache is " +
                                       (dsim.isFileCacheEnabled() ? "on"
                                                                  : "off"));
                } else if (cmd.equals("on")) {
                    dsim.enableFileCache();
                    System.out.println("Warning: changes to files will " +
                                       "NOT be detected!");
                    System.out.println("Remember to flush if you change " +
                                       "a file!");
                } else if (cmd.equals("off")) {
                    dsim.disableFileCache();
                } else if (cmd.equals("flush")) {
                    dsim.flushFileCache();
                } else {
                    System.out.println("Usage: " + usage);
                    return;
                }
            }
        },
        new CmdCommand("last_exception",
                       "last_exception",
                       "Prints the full stack trace of the last exception",
                       "Prints the full stack trace of the last exception") {
            public void execute(final String[] args) {
                System.out.println(lastStackTrace);
            }
        },
        new CmdCommand("__cosim_leftovers",
                       "__cosim_leftovers",
                       "Prints contents of slack before cosim merges",
                       "Prints contents of slack before cosim merges") {
            public void execute(final String[] args) {
                com.avlsi.tools.cosim.CoSimInfo.printCosimLeftovers();
            }
        },
        new DelayCommand("digital_delay",
                         "digital_delay [ <tau> | off ] ",
                         "Scale production rule delays ",
                         "Scale production rule delays\n" +
                         "  off   - use standard production rule delays\n" +
                         "  <tau> - calculate the delay as\n" +
                         "                         delay = delay * digital_delay\n",
                         DSim.DIGITAL_TAU),
        new DelayCommand("estimated_delay",
                         "estimated_delay [ <tau> | off ] ",
                         "Calculate delay from estimated_delay directives",
                         "Calculate delay from estimated_delay directives\n" +
                         "  off   - use standard production rule delays\n" +
                         "  <tau> - use estimated_delay directives, and calculate the delay as\n" +
                         "                         delay = 100 * estimated_delay / tau\n" +
                         "          If estimated_delay exists for a node, no other directives that\n" +
                         "          affect delay, e.g., delay_bias or extra_delay, will be considered.\n" +
                         "          If the directive does not exist for a node, standard production\n" +
                         "          rule delays are used.\n",
                         DSim.ESTIMATED_TAU),
        new CmdCommand("measured_delay",
                       "measured_delay [ dataset | off ] [ timescale ]",
                       "Calculate delay from measured_delay directives",
                       "Calculate delay from measured_delay directives\n" +
                       "  off     - do not use measured_delay directives\n" +
                       "  dataset - use measured_delay directives for the selected dataset, and\n" +
                       "            calculate the delay as\n" +
                       "                         delay = 100 * measured_delay / timescale\n" +
                       "            If measured_delay exists for a node, no other directives that\n" +
                       "            affect delay, e.g., delay_bias or extra_delay, will be\n" +
                       "            considered.  If the directive does not exist for a node,\n" +
                       "            standard production rule delays are used.\n" +
                       "  timescale - used to calculate delay as above; defaults to 1\n") {
            private int mode = DSim.MEASURED_TAU;
            public void execute(final String[] args) {
                if (args != null && args.length > 2) {
                    System.out.println("Usage: " + usage);
                    return;
                }

                checkDSim();

                if (args == null) {
                    System.out.println(this.getName() +
                        (dsim.useDelayMode(mode) ?
                            " using " + ("dataset " + dsim.getMeasureDataSet() +
                             " and timescale " +
                             Float.toString(dsim.getDelayTau(mode))) :
                            " is off"));
                    return;
                }

                if (args[0].equals("off")) {
                    dsim.disableDelayMode(mode);
                } else {
                    final int dataset;
                    try {
                        dataset = Integer.parseInt(args[0]);
                    } catch (NumberFormatException e) {
                        System.out.println("Invalid dataset specified: " + args[0]);
                        return;
                    }
                    float tau = 1;
                    if (args.length == 2) {
                        try {
                            tau = Float.parseFloat(args[1]);
                        } catch (NumberFormatException e) {
                            tau = Float.NaN;
                        }
                        if (tau <= 0 || Float.isNaN(tau)) {
                            System.out.println("Invalid timescale specified: " + args[1]);
                            return;
                        }
                    }
                    dsim.setMeasureDataSet(dataset);
                    dsim.enableDelayMode(mode, tau);
                }
            }
        },
        new CmdCommand("get_switched_cap",
                       "get_switched_cap <node(s)>",
                       "Get switched capacitance",
                       "Get switched capacitance since tcounts was last cleared.  The switched\n" +
                       "cap is calculated as the sum of capacitance on a node (as specified by\n" +
                       "the cap directive), multiplied by the transition count of the node,\n" +
                       "over specified nodes.") {
            public void execute(final String[] args) {
                if (checkDSim()) {
                    if (args != null && args.length > 0) {
                        final double[] switchedCap = { 0 };
                        final long[] noCapTrans = { 0 };
                        final long[] capTrans = { 0 };
                        final long[] nodeCount = { 0 };
                        final double[] totalCap = { 0 };
                        processNodeArgs(args, 0, args.length, new NodeIterator() {
                            public void processNode(HierName name, Node node) {
                                if (node != null) {
                                    ++nodeCount[0];
                                    if (node.getCapacitance() == 0) {
                                        noCapTrans[0] += node.getTCount();
                                    } else {
                                        capTrans[0] += node.getTCount();
                                        totalCap[0] += node.getCapacitance();
                                        switchedCap[0] += node.getTCount() *
                                                          node.getCapacitance();
                                    }
                                }
                            }
                        });
                        final PrintfFormat fmt = new PrintfFormat("%g");
                        System.out.println(
                                "Total switched capacitance:          " +
                                fmt.sprintf(switchedCap[0]) +
                                " F");
                        System.out.println(
                                "Average capacitance per node:        " +
                                (nodeCount[0] == 0 ?
                                    "NA"
                                  : fmt.sprintf(totalCap[0] / nodeCount[0]) +
                                    " F"));
                        System.out.println(
                                "Transitions with cap directives:     " + 
                                capTrans[0]);
                        System.out.println(
                                "Transitions without cap directives:  " + 
                                noCapTrans[0]);
                    } else { System.err.println("get_switched_cap: no node names given"); }
                }
            }
        },
        new CmdCommand("measured_cap",
                       "measured_cap [scale]",
                       "Enable measured_cap directives",
                       "Set the capacitance associated with each node from\n" +
                       "measured_cap directives if available.  Multiply the\n" +
                       "capacitance by the scale factor, if given.") {
            public void execute(final String[] args) {
                final float scale;
                try {
                    if (args == null) {
                        scale = 1.0f;
                    } else if (args.length == 1) {
                        scale = validateFloatArg(args, 0);
                    } else {
                        System.err.println("Too many arguments");
                        return;
                    }
                    dsim.enableMeasuredCap(scale);
                } catch (Exception e) {
                    System.err.println("Usage: " + usage);
                }
            }
        },
        new CmdCommand("report_rule_coverage",
                       "report_rule_coverage <filename>",
                       "Create report of PRS DNF rule coverage",
                       "Creates a text file that lists every PRS DNF rule\n" +
                       "which has not been exercised.  (But omits ERROR\n" +
                       "rules and rules which cannot transition because of\n" +
                       "GND and Vdd propagation.)") {
            public void execute(final String[] args) {
                if (args == null || args.length != 1) {
                    System.out.println("Usage: " + usage);
                } else if (checkFile()) {
                    try {
                        BufferedWriter w =
                            new BufferedWriter
                            (new FileWriter(args[0]));
                        dsim.reportRuleCoverage(w, true, false);
                        w.close();
                    } catch (IOException e) {
                        System.out.println(e);
                    }
                }
            }
        },
        new CmdCommand("full_report_rule_coverage",
                       "full_report_rule_coverage <filename>",
                       "Create verbose report of PRS DNF rule coverage",
                       "Creates a text file that lists every PRS DNF rule\n" +
                       "in the current instantiation, followed by the\n" +
                       "number of times it has transitioned, followed\n" +
                       "optionally by an X to indicate the rule must, by\n" +
                       "construction, always be off, or * to indicate it\n" +
                       "must always be on.") {
            public void execute(final String[] args) {
                if (args == null || args.length != 1) {
                    System.out.println("Usage: " + usage);
                } else if (checkFile()) {
                    try {
                        BufferedWriter w =
                            new BufferedWriter
                            (new FileWriter(args[0]));
                        dsim.reportRuleCoverage(w, true, true);
                        w.close();
                    } catch (IOException e) {
                        System.out.println(e);
                    }
                }
            }
        },
        new CmdCommand("report_coverage_constant_nodes",
                       "report_coverage_constant_nodes <filename> [<cell> ...]",
                       "Create list of nodes with constant value",
                       "Creates a Verilog file that forces all the nodes\n" +
                       "with constant value to that constant value.\n" +
                       "The node is output as a fully qualified canonical\n" +
                       "name with appropriate Verilog escaping.  If a node\n" +
                       "is a local node in one of the cells listed in the\n" +
                       "ignore list, then it is not output.") {
            public void execute(final String[] args) {
                if (args == null) {
                    System.out.println("Usage: " + usage);
                } else if (checkFile()) {
                    final List ignoreList = new ArrayList();
                    for (int i = 1; i < args.length; ++i)
                        ignoreList.add(args[i]);
                    try {
                        BufferedWriter w =
                            new BufferedWriter
                            (new FileWriter(args[0]));
                        dsim.reportCoverageConstantNodes(w, ignoreList, true);
                        w.close();
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
        },

        new CmdCommand("chan_filled",
                       "chan_filled [<channel glob>]",
                       "Return information on full channels",
                       "Returns the number of queued messages and slack for each channel matching\n" +
                       "the glob.  If no glob is specified, then all full channels of CSP processes\n" +
                       "are included.  This command is similar to chan_fill -F.\n" +
                       "    -r    interpret the glob as a regular expression.\n" +
                       "    -v    print out queued messages") {
            public void execute(final String[] args) {
                String regex = null;
                boolean full = false;
                boolean glob = true;
                boolean verbose = false;
                boolean fullOnly = true;
                if (args != null) {
                    for (int i = 0; i < args.length; ++i) {
                        if (args[i].equals("-r")) {
                            glob = false;
                        } else if (args[i].equals("-v")) {
                            verbose = true;
                        } else {
                            if (regex == null) {
                                regex = args[i];
                            } else {
                                System.out.println("Usage: " + usage);
                                return;
                            }
                        }
                    }
                }
                chanFill(full, glob, verbose, fullOnly, regex);
            }
        },

        new CmdCommand("chan_fill",
                       "chan_fill [-f] [-F] [-r] [-v] [<channel glob>]",
                       "Return information on how full a channel is",
                       "Returns the number of queued messages and slack for each channel matching\n" +
                       "the glob.  If no glob is specified, then all channels of CSP processes are\n" +
                       "considered.\n" +
                       "    -f    sort the output by the ratio of queued messages to capacity.\n" +
                       "    -F    only output channels that are filled to capacity.\n" +
                       "    -r    interpret the glob as a regular expression.\n" +
                       "    -v    print out queued messages") {
            public void execute(final String[] args) {
                String regex = null;
                boolean full = false;
                boolean glob = true;
                boolean verbose = false;
                boolean fullOnly = false;
                if (args != null) {
                    for (int i = 0; i < args.length; ++i) {
                        if (args[i].equals("-f")) {
                            full = true;
                        } else if (args[i].equals("-r")) {
                            glob = false;
                        } else if (args[i].equals("-v")) {
                            verbose = true;
                        } else if (args[i].equals("-F")) {
                            fullOnly = true;
                        } else {
                            if (regex == null) {
                                regex = args[i];
                            } else {
                                System.out.println("Usage: " + usage);
                                return;
                            }
                        }
                    }
                }
                chanFill(full, glob, verbose, fullOnly, regex);
            }
        },
        new CmdCommand("toggle_measured_delay",
            "toggle_measured_delay on|off [node]", 
            "Enable or disable measure delay directives",
            "For each node, enable or disable measure delays directives\n" +
            "on rules that target it.\n") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    if (args != null && args.length > 1) {
                        final boolean on;
                        if (args[0].equals("on")) {
                            on = true;
                        } else if (args[0].equals("off")) {
                            on = false;
                        } else {
                            System.out.println("Error: on or off must be" +
                                               "the first argument.");
                            return;
                        }
                        final int[] count = { 0 };
                        processNodeArgs(args, 1, args.length, new NodeIterator() {
                            public void processNode(HierName name, Node node) {
                                if (node == null) {
                                    System.out.println("Invalid node: " + name);
                                } else {
                                    for (Rule r : dsim.getTargetingRules(node)) {
                                        if (r.getMeasureStatus() != on) {
                                            r.setMeasureStatus(on);
                                            count[0]++;
                                        }
                                    }
                                }
                            }
                        });
                        System.out.println("Measured delay turned " + args[0] +
                                           " for " + count[0] + " rules.");
                    } else {
                        System.out.println("Error: Incorrect number of " +
                                           "arguments.");
                    }
                }
            }
        },
        new CmdCommand("set_delay",
            "set_delay <delay> <node[+|-]> ...",
            "Set the delay for rules driving the specified half-operators",
            "For each half-operator, set the delay (digital, or estimated if enabled) for\n" +
            "rules that drive it.  If measured_delay mode is on, and there is measured\n" +
            "data, then the new delay won't take effect unless toggle_measured_delay\n" +
            "is used to disable measure delays for the nodes.  Node can be a glob\n" +
            "expression.") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    if (args == null || args.length < 2) {
                        System.out.println("Error: Incorrect number of " +
                                           "arguments.");
                        return;
                    }

                    final int delay;
                    try {
                        delay = validateIntArg(args, 0, true);
                    } catch (Exception e) {
                        System.out.println("Error: delay must be an integer.");
                        return;
                    }
                    if (delay < 0) {
                        System.out.println(
                            "Error: delay must be non-negative.");
                        return;
                    }

                    final HashSet<String> seen = new HashSet<String>();
                    processHalfOpArgs(args, 1, args.length,
                        new HalfOpIterator() {
                            public void processHalfOp(HierName name, Node node,
                                                      Boolean dir) {
                                if (node == null) {
                                    System.out.println("Invalid node: " + name);
                                } else {
                                    for (Rule r : dsim.getTargetingRules(node))
                                    {
                                        final char pm = r.getDirection();
                                        if (dir == null ||
                                            dir.booleanValue() == (pm == '+')) {
                                            final int oldDelay =
                                                r.setDelay(delay);
                                            final String target =
                                                name.toString() + pm;
                                            if (seen.add(target)) {
                                                System.out.printf(
                                                    "%5d (%5d) %s\n",
                                                    delay, oldDelay, target);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    );
                }
            }
        },
        new CmdCommand("get_delay",
            "get_delay <node[+|-]> ...",
            "Get the delay for rules driving the specified half-operators",
            "For each half-operator, get the delay (digital, or estimated if enabled) for\n" +
            "rules that drive it.  Node can be a glob expression.  Note that this will\n" +
            "not return the measured delay.") {
            public void execute(String args[]) { 
                if (checkDSim()) {
                    if (args == null || args.length < 1) {
                        System.out.println("Error: Incorrect number of " +
                                           "arguments.");
                        return;
                    }

                    final HashSet<String> seen = new HashSet<String>();
                    processHalfOpArgs(args, 0, args.length,
                        new HalfOpIterator() {
                            public void processHalfOp(HierName name, Node node,
                                                      Boolean dir) {
                                if (node == null) {
                                    System.out.println("Invalid node: " + name);
                                } else {
                                    for (Rule r : dsim.getTargetingRules(node))
                                    {
                                        final char pm = r.getDirection();
                                        if (dir == null ||
                                            dir.booleanValue() == (pm == '+')) {
                                            final int delay = r.getDelay();
                                            final String target =
                                                name.toString() + pm;
                                            if (seen.add(target)) {
                                                System.out.printf(
                                                    "%5d %s\n", delay, target);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    );
                }
            }
        }
    };
    /** Checks if the DSim module has been initialized. **/
    boolean checkDSim() {
        if (dsim==null) { dsim=DSim.get(); }
        return dsim!=null;
    }

    /** Checks if the DSim module has loaded a file. **/
    boolean checkFile() {
        if (dsim==null) { dsim=DSim.get(); }
        final boolean result = dsim!=null && dsim.loaded;
        if (!result) {
            System.err.println("This command has no effect before a cell " +
                               "has been instantiated.");
        }
        return result;
    }

    /** load CSP coverage data from a file **/
    private void loadCSPCoverageData() {
	try {
	    System.err.print("Loading coverage data data from "+
			     Monitor.global.getPath()+"... ");
	    if(!Monitor.global.pathExists()) {
		System.err.println("no saved coverage data found");
	    } else {
		Monitor.global.load();
		System.err.println("done");
	    }
	} catch(IOException e) {
	    System.err.println("caught IO exception: "+e);
	} catch(ParseException e) {
	    System.err.println("caught parse exception: "+e);
	}
    }
    
    /** save CSP coverage data to a file **/
    private void saveCSPCoverageData() {
	try {
	    System.err.print("Saving coverage data data to "+
			     Monitor.global.getPath()+"... ");
	    Monitor.global.save();
	    System.err.println("done");
	} catch(IOException e) {
	    System.err.println("caught IO exception: "+e);
	}
    }

    /**
     * Wrapper for <code>DSim.cosimulate</code> that handles printing
     * of exceptions.  Does not signal success or failure to caller.
     **/
    private void cosimulate(final String cosimSpecString,
                            final String instanceName) {
        // if no environment is specified, we automatically initialize the
        // input ports to GND, which requires access to the CellInterface
        // object; to avoid having to parse the cell again, temporarily enable
        // file caching if not already enabled
        final boolean fileCacheEnabled = dsim.isFileCacheEnabled();
        try {
            final CoSim cosim = CoSim.getCoSim(cosimSpecString, true);
            final boolean emptyEnv = cosim.getEnvName() == null;
            if (emptyEnv && !fileCacheEnabled) dsim.enableFileCache();
            dsim.cosimulate(cosim.getCellType(), instanceName,
                            cosim.getCoSimSpecList(),
                            cosim.getEnvName(), "_env", cosim.getEnvSpec());
            if (emptyEnv) {
                if (utils == null) utils = new DSimUtil();
                utils.resetInputNodes(dsim.getCell(), instanceName);
            }
        } catch (IllegalArgumentException e) {
            saveStackTrace(e);
            // bad mode list or level or bad cosim spec
            System.out.println("ERROR: " + e.getMessage());
        } catch (DuplicateInstanceSpecException e) {
            saveStackTrace(e);
            System.out.println("ERROR: Bad InstSpec: " +
                    e.getMessage());
        } catch (ExtraInstanceSpecException e) {
            saveStackTrace(e);
            System.out.println("ERROR: " + e.getMessage());
        } catch (HierarchyDepthException e) {
            saveStackTrace(e);
            System.out.println("Depth of " + e.getInstanceName() +
                    " exceeded by " + e.getExcessDepth());
        } catch (NoSuchInstanceException e) {
            saveStackTrace(e);
            System.out.println("ERROR: " + e.getMessage());
        } catch (IOException e) {
            saveStackTrace(e);
            e.printStackTrace();
        } catch (CastSyntaxException e) {
            saveStackTrace(e);
            e.printStackTrace();
        } catch (CastSemanticException e) {
            saveStackTrace(e);
            ExceptionPrettyPrinter.printException(e);
        } catch (NoBehaviorFoundException e) {
            saveStackTrace(e);
            System.out.println(e.getMessage());
        } catch (NoSuchCellException e) {
            saveStackTrace(e);
            System.out.println("Cell type not found: " +
                    e.getCellType());
        } catch (NoSuchEnvironmentException e) {
            saveStackTrace(e);
            System.err.println("Cannot instantiate environment: " +
                               e.getEnvironmentName());
            ExceptionPrettyPrinter.printException(e);
        } finally {
            if (!fileCacheEnabled) {
                dsim.flushFileCache();
                dsim.disableFileCache();
            }
        }
    }

    /**
     * Saves stack trace in lastStackTrace variable, for use with
     * "last_exception" command.
     */
    private void saveStackTrace(Exception e) {
        lastStackTrace = ExceptionUtils.getStackTraceString(e);
    }

    private void clear() {
        if (checkDSim()) {
            executeSST(SSTOperation.CLOSE);
            if (dsim.isAstaEnabled()) sta = new RunStaticTiming();
            dsim.setSigscan(null);
            dsim.rm_instantiation();
            utils = null;
        }
    }
} 
