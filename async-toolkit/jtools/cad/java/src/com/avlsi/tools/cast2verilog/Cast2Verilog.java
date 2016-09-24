/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cast2verilog;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.lang.Integer;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import java.text.MessageFormat;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Predicate;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.impl.CastParsingOption;
import com.avlsi.cast2.util.DirectiveUtils; 
import com.avlsi.cast2.util.StandardParsingOption;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellImpl;
import com.avlsi.cell.CellUtils;
import com.avlsi.cell.CellUtils.MarkPort;
import com.avlsi.cell.NoSuchEnvironmentException;
import com.avlsi.csp.csp2java.SemanticException;
import com.avlsi.csp.csp2verilog.Csp2Verilog;
import com.avlsi.csp.util.CSPCellInfo;
import com.avlsi.csp.util.UniqueLabel;
import com.avlsi.csp.util.ProblemFilter;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.ports.ArrayType;
import com.avlsi.fast.ports.ChannelType;
import com.avlsi.fast.ports.NodeType;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.fast.ports.PortTypeInterface;
import com.avlsi.fast.ports.StructureType;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.file.common.HierName;
import com.avlsi.io.FileSearchPath;
import com.avlsi.io.NullWriter;
import com.avlsi.io.Separator;
import com.avlsi.prs.ProductionRule;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cosim.ChannelTimingInfo;
import com.avlsi.tools.cosim.CoSimParameters;
import com.avlsi.tools.cosim.CoSimHelper;
import com.avlsi.tools.cosim.JavaCoSimInfo;
import com.avlsi.tools.cosim.spec.CoSim;
import com.avlsi.tools.cosim.spec.CoSimSpec;
import com.avlsi.tools.cosim.spec.CoSimSpecList;
import com.avlsi.tools.cosim.spec.InstSpec;
import com.avlsi.tools.cosim.spec.InstSpecList;
import com.avlsi.tools.cosim.spec.Mode;
import com.avlsi.tools.cosim.spec.ModeList;
import com.avlsi.tools.cosim.spec.ModeListLevelSpec;
import com.avlsi.tools.dsim.InstanceData;
import com.avlsi.tools.dsim.ExceptionPrettyPrinter;
import com.avlsi.tools.prs2verilog.Prs2Verilog;
import com.avlsi.tools.prs2verilog.ConverterConstants;
import com.avlsi.tools.prs2verilog.verilog.VerilogUtil;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.UnionCommandLineArgs;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.container.FlatteningIterator;
import com.avlsi.util.container.IterableIterator;
import com.avlsi.util.container.MappingIterator;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.Triplet;
import com.avlsi.util.functions.UnaryFunction;
import com.avlsi.util.math.BigIntegerUtil;
import com.avlsi.util.text.NaturalStringComparator;
import com.avlsi.util.text.StringUtil;

/**
 * Generates Verilog from Cast 
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class Cast2Verilog {

    private final PrintWriter warningWriter;
    private final PrintWriter errorWriter;
    private final PrintWriter debugWriter;
    private final CoSimParameters params;
    private Set prsBehavior;
    private final Map leafDelayBias;
    private final Map leafExtraDelay;
    private static final String resetNodeName = "_RESET"; 
    private static final String delaybiasAnnotationName =
        "CAST2VERILOG_ANNOTATE_DELAYBIAS";
    private static final String extraDelayAnnotationName =
        "CAST2VERILOG_ANNOTATE_EXTRADELAY";
    private static final String moduleInstanceName =
        "CAST2VERILOG_INSTANCE";
    private static final String dutInstance = "x";
    private static final String envInstance = "_env";

    private final Map subcellNames;

    private final Set convertedCells;
    /**
     * Number of bits to use for variables 
     **/
    private final int registerBitWidth;

    /**
     * Files depended on by verilog blocks.
     **/
    private final Set validVerilogFiles = new LinkedHashSet();

    /**
     * Names of cells that has been translated by Prs2Verilog.
     **/
    private final Set prsDone = new HashSet();

    private static final Logger logger =
        Logger.getLogger("com.avlsi.tools.cast2verilog");

    private ProblemFilter probFilter = null;

    /**
     * Whether to emit SystemVerilog constructs
     **/
    private boolean enableSystemVerilog;

    /**
     * Whether to surround module definitions with ifdefs to prevent
     * redefinition.
     **/
    private boolean generateIfDef;

    /**
     * Whether to generate a full testbench that will drive the supplies
     * and reset.
     **/
    private boolean generateTb;

    /**
     * Set to true if errors were encountered during translation.
     **/
    private boolean errorExist;

    private Cadencize vcad = null;  // Verilog Cadencize
    private Cadencize cad = null;   // non-Verilog Cadencize

    private static String esc(String s) {
        return VerilogUtil.escapeIfNeeded(s);
    }

    private interface ChannelEmitter {
        ChannelType getType();
        Stream<String> getComponents();
        default int getNumBitsWide(ChannelType chanType) {
            return Cast2Verilog.getNumBitsWide(chanType);
        }
        default int getNumBitsNarrow(ChannelType chanType) {
            return Cast2Verilog.getNumBitsNarrow(chanType);
        }
        default void emitPassThru(PrintWriter out, String inChan, String outChan) {
            final List<String> inComp = new ArrayList<>();
            final List<String> outComp = new ArrayList<>();
            // inputs
            getInputPorts(inChan, Collections.emptyList(), -1, inComp);
            getInputPorts(outChan, Collections.emptyList(), -1, outComp);
            for (int i = 0; i < inComp.size(); ++i) {
                out.println("assign " + outComp.get(i) + " = " +
                            inComp.get(i) + ";");
            }

            // outputs
            inComp.clear();
            outComp.clear();
            getInputPorts(inChan, Collections.emptyList(), 1, inComp);
            getInputPorts(outChan, Collections.emptyList(), 1, outComp);
            for (int i = 0; i < inComp.size(); ++i) {
                out.println("assign " + inComp.get(i) + " = " +
                            outComp.get(i) + ";");
            }
        }
        default void emitPortDeclaration(String name,
                                         List<ArrayType> arrays,
                                         int direction,
                                         IntFunction<String> netTypeFunction,
                                         String suffix,
                                         Separator out) {}
        default void emitPortInitializer(String name,
                                         List<ArrayType> arrays,
                                         int direction,
                                         PrintWriter out) {}
        default void getInputPorts(String name,
                                   List<ArrayType> arrays,
                                   int direction,
                                   List<String> inputPorts) {}
        String getSlackWrapperName(int direction, int slack);
        default void emitSlackWrapper(final String name,
                                      int direction,
                                      String actualSuffix,
                                      Function<String,ChannelTimingInfo> ctiFunc,
                                      String reset,
                                      PrintWriter out) {
            final ChannelTimingInfo cti = ctiFunc.apply(name);
            final int slack = cti.getSlack();
            final float latency = numTransitions(cti.getLatency());
            final float cycletime = numTransitions(cti.getCycleTime());
            final int bitWidth = getNumBitsWide(getType());

            // emit timing buffer instantiation
            out.print(getSlackWrapperName(direction, slack));

            out.print(" #(.bit_width(" + (bitWidth - 1) + "), ");
            out.print(".cycle_time (" + cycletime + "), ");
            if (slack != 0) {
                out.print(".slack (" + slack + "), ");
                out.print(".forward_latency (" + latency + "), ");
                out.print(".cycle_time_in (" +
                          numTransitions(cti.getCycleTimeIn()) + "), ");
                out.print(".cycle_time_out (" +
                          numTransitions(cti.getCycleTimeOut()) + "), ");
            }
            if (direction < 0) {
                out.print(".fb_neutral (" +
                          numTransitions(cti.getDataNeutralEnableLatency()) +
                          "), ");
                out.print(".fb_valid (" +
                          numTransitions(cti.getDataValidEnableLatency()) +
                          ")");
            } else {
                out.print(".bf_latency (" +
                          numTransitions(cti.getEnableDataLatency()) + ")");
            }
            out.println(") " + esc("tb_" + name) + " (");
            final Separator sout = new Separator(out);
            sout.print("." + resetNodeName + " (" + esc(reset) + ")"); 
            final Pair<String,String> parts = mungeArray(name);
            emitChannelConnection(direction < 0 ? "L" : "R", name, null, sout);
            emitChannelConnection(direction < 0 ? "R" : "L",
                    x -> Optional.of(esc(parts.getFirst() + x + actualSuffix) +
                                     parts.getSecond()),
                    null, sout);
            out.println(");");
        }
        default void emitBodyInstantiation(String name, String actualSuffix,
                                           Separator out) {
            getComponents()
                  .forEach(s -> out.print("." + esc(name + s) + "(" +
                                          esc(name + s + actualSuffix) + ")"));
        }
        default void emitChannelConnection(String formal,
                                           Function<String,Optional<String>> actualFunc,
                                           String hier, Separator sout) {
            getComponents().forEach(s -> {
                String actualHier = actualFunc.apply(s)
                                              .map(x -> hierRef(hier, x))
                                              .orElse("/*NC*/");
                sout.print("  ." + esc(formal + s) + "(" + actualHier + ")");
            });
        }
        default void emitChannelConnection(String formal, final String actual,
                                           String hier, Separator sout) {
            emitChannelConnection(
                    formal,
                    x -> Optional.ofNullable(actual).map(y -> esc(y + x)),
                    hier, sout);
        }
        default void emitWideConverterInstantiation(
                final CellUtils.Channel wide,
                final Map<CellUtils.Channel,CellUtils.Channel> portMap,
                final String hier,
                final PrintWriter out) {
            throw new RuntimeException(
                    "emitWideConverterInstantiation not implemented");
        }
        void emitChannelDecl(String name, PrintWriter out);
        void emitNarrowConverterInstantiation(
                final CellUtils.Channel narrow,
                final Map<CellUtils.Channel,CellUtils.Channel> portMap,
                final String hier,
                final PrintWriter out);
    }

    private static class E1ofNChannelEmitter implements ChannelEmitter {
        private ChannelType chanType;
        public E1ofNChannelEmitter(ChannelType chanType) {
            this.chanType = chanType;
        }
        public ChannelType getType() {
            return chanType;
        }
        public Stream<String> getComponents() {
            return Stream.of("$data", "$enable");
        }
        public void emitPortDeclaration(String name,
                                        List<ArrayType> arrays,
                                        int direction,
                                        IntFunction<String> netTypeFunction,
                                        String suffix,
                                        Separator out) {
            final int bitWidth = getNumBitsWide(chanType);
            out.print(netTypeFunction.apply(direction) + " signed [" +
                      (bitWidth - 1) + ":0] " + 
                      esc(name + "$data" + suffix) +
                      getArrayDecl(arrays));
            out.print(netTypeFunction.apply(-direction) + " " +
                      esc(name + "$enable" + suffix) +
                      getArrayDecl(arrays));
        }
        public void emitPortInitializer(String name,
                                        List<ArrayType> arrays,
                                        int direction,
                                        PrintWriter out) {
            final String minusOne = arrays.isEmpty() ? "-1" : "'{default:-1}";
            final String one = arrays.isEmpty() ? "1" : "'{default:1}";
            if (direction > 0) {
                out.println(esc(name + "$data") + " = " + minusOne + ";");
            } else {
                out.println(esc(name + "$enable") + " = " + one + ";");
            }
        }
        public void getInputPorts(String name,
                                  List<ArrayType> arrays,
                                  int direction,
                                  List<String> inputPorts) {
            if (direction > 0) {
                inputPorts.add(esc(name + "$enable"));
            } else {
                inputPorts.add(esc(name + "$data"));
            }
        }
        public String getSlackWrapperName(int direction, int slack) {
            return "`CAST2VERILOG_" + 
                   (direction < 0 ? "INPUT" : "OUTPUT") +
                   "_TIMINGBUFFER" +
                   (slack == 0 ? "_SLACK0" : "");
        }
        public void emitWideConverterInstantiation(
                final CellUtils.Channel wide,
                final Map<CellUtils.Channel,CellUtils.Channel> portMap,
                final String hier,
                final PrintWriter out) {
            final int base = BigIntegerUtil.safeIntValue(chanType.getNumValues());
            final int width = chanType.getWidth();
            final int narrowBits = getNumBitsNarrow(chanType);
            final int wideBits = getNumBitsWide(chanType);

            final String module =
                "csp_e1of_" + (wide.isInput() ? "narrowwide" : "widenarrow");
            final String params = "#(" + base + "," +
                                         width + "," +
                                         narrowBits + "," +
                                         wideBits +
                                  ")";
            final String otherWide = portMap.get(wide).getFullName();
            out.println(esc(module) + params + " " + esc("conv_" + otherWide) + "(");

            final Separator sout = new Separator(out);

            // output connections to the wide side of the converter
            emitChannelConnection("WIDE", otherWide, hier, sout);

            // output connections to the narrow side of the converter
            for (String s : new String[] { "$data", "$enable" }) {
                sout.print("  ." + esc("NARROW" + s) + "(" +
                           wide.getChildren()
                               .stream()
                               .map(narrow -> portMap.get(narrow).getFullName())
                               .map(chan -> hierRef(hier, esc(chan + s)))
                               .collect(Collectors.joining(", ", "{", "}")) +
                           ")");
            }

            out.println(");"); 
        }
        public void emitChannelDecl(String name, PrintWriter out) {
            declareWire(name + "$data", getNumBitsWide(chanType), out);
            declareWire(name + "$enable", 0, out);
        }
        public void emitNarrowConverterInstantiation(
                final CellUtils.Channel narrow,
                final Map<CellUtils.Channel,CellUtils.Channel> portMap,
                final String hier,
                final PrintWriter out) {
            final ChannelType type = (ChannelType) narrow.getType();
            final int base = BigIntegerUtil.safeIntValue(type.getNumValues());

            final String otherNarrow = portMap.get(narrow).getFullName();
            out.print(narrow.isInput() ? "Nodes2ChannelConv2"
                                       : "Channel2NodesConv2");
            out.print(" #(.base(" + base + "), ");
            out.print(".numBits(" + getNumBitsNarrow(type) + ")) "); 
            out.println("\\conv_" + otherNarrow + " (");

            final Separator sout = new Separator(out);
            // output connections to the narrow side of the converter
            getChannelEmitter(type).
                emitChannelConnection("channel", otherNarrow, hier, sout);

            // output connections to the node side of the converter
            // the order is d[0], d[1], ...
            final Collection<CellUtils.Channel> children = narrow.getChildren();
            final Iterator<CellUtils.Channel> i = children.iterator();
            final Function<CellUtils.Channel,String> nameFunc =
                node -> hierRef(hier, esc(getActualName(portMap, node)));
            sout.print(".nodes(" +
                       IntStream.range(1, children.size())
                                .mapToObj(n -> i.next())
                                .map(nameFunc)
                                .collect(Collectors.joining(", ", "{", "}")) +
                       ")");

            // output connection to enable
            sout.print(".enable(" + nameFunc.apply(i.next()) + ")");
            out.println(");");
        }
        public int getNumBitsNarrow(final ChannelType chanType) {
            // add 1 bit for sign bit
            return Cast2Verilog.getNumBitsNarrow(chanType) + 1;
        }
        public int getNumBitsWide(final ChannelType chanType) {
            // add 1 bit for sign bit
            return Cast2Verilog.getNumBitsWide(chanType) + 1;
        }
    }

    private static class BDChannelEmitter implements ChannelEmitter {
        private ChannelType chanType;
        public BDChannelEmitter(ChannelType chanType) {
            this.chanType = chanType;
        }
        public ChannelType getType() {
            return chanType;
        }
        public Stream<String> getComponents() {
            return Stream.of("$data", "$req", "$ack");
        }
        public void emitPortDeclaration(String name,
                                        List<ArrayType> arrays,
                                        int direction,
                                        IntFunction<String> netTypeFunction,
                                        String suffix,
                                        Separator out) {
            final int bitWidth = getNumBitsWide(chanType);
            out.print(netTypeFunction.apply(direction) + " signed [" +
                      (bitWidth - 1) + ":0] " + 
                      esc(name + "$data" + suffix) +
                      getArrayDecl(arrays));
            out.print(netTypeFunction.apply(direction) + " " +
                      esc(name + "$req" + suffix) +
                      getArrayDecl(arrays));
            out.print(netTypeFunction.apply(-direction) + " " +
                      esc(name + "$ack" + suffix) +
                      getArrayDecl(arrays));
        }
        public void emitPortInitializer(String name,
                                        List<ArrayType> arrays,
                                        int direction,
                                        PrintWriter out) {
            final String zero = arrays.isEmpty() ? "0" : "'{default:0}";
            if (direction > 0) {
                out.println(esc(name + "$data") + " = " + zero + ";");
                out.println(esc(name + "$req") + " = " + zero + ";");
            } else {
                out.println(esc(name + "$ack") + " = " + zero + ";");
            }
        }
        public void getInputPorts(String name,
                                  List<ArrayType> arrays,
                                  int direction,
                                  List<String> inputPorts) {
            if (direction > 0) {
                inputPorts.add(esc(name + "$ack"));
            } else {
                inputPorts.add(esc(name + "$data"));
                inputPorts.add(esc(name + "$req"));
            }
        }
        public String getSlackWrapperName(int direction, int slack) {
            return "`CAST2VERILOG_BD_" + 
                   (direction < 0 ? "INPUT" : "OUTPUT") +
                   "_TIMINGBUFFER" +
                   (slack == 0 ? "_SLACK0" : "");
        }
        public void emitChannelDecl(String name, PrintWriter out) {
            declareWire(name + "$data", getNumBitsWide(chanType), out);
            declareWire(name + "$req", 0, out);
            declareWire(name + "$ack", 0, out);
        }
        public void emitNarrowConverterInstantiation(
                final CellUtils.Channel narrow,
                final Map<CellUtils.Channel,CellUtils.Channel> portMap,
                final String hier,
                final PrintWriter out) {
            final ChannelType type = (ChannelType) narrow.getType();

            final String otherNarrow = portMap.get(narrow).getFullName();
            out.print(narrow.isInput() ? "BDNodes2ChannelConv2"
                                       : "BDChannel2NodesConv2");
            out.print(" #(.numBits(" + getNumBitsNarrow(type) + ")) "); 
            out.println("\\conv_" + otherNarrow + " (");

            final Separator sout = new Separator(out);
            // output connections to the narrow side of the converter
            getChannelEmitter(type).
                emitChannelConnection("channel", otherNarrow, hier, sout);

            // output connections to the node side of the converter
            // the order is d[0], d[1], ...
            final Collection<CellUtils.Channel> children = narrow.getChildren();
            final Iterator<CellUtils.Channel> i = children.iterator();
            final Function<CellUtils.Channel,String> nameFunc =
                node -> hierRef(hier, esc(getActualName(portMap, node)));
            sout.print(".req(" + nameFunc.apply(i.next()) + ")");
            sout.print(".ack(" + nameFunc.apply(i.next()) + ")");
            sout.print(".nodes(" +
                       CollectionUtils.stream(i)
                                .map(n -> i.next())
                                .map(nameFunc)
                                .collect(Collectors.joining(", ", "{", "}")) +
                       ")");
            out.println(");");
        }
    }

    private static ChannelEmitter getChannelEmitter(final ChannelType chanType) {
        final String type = chanType.getTypeName();
        if (type.startsWith("standard.channel.bd")) {
            return new BDChannelEmitter(chanType);
        } else if (type.startsWith("standard.channel.e1of")) {
            return new E1ofNChannelEmitter(chanType);
        } else {
            throw new RuntimeException("No support for channel type: " + type);
        }
    }

    private static class IgnoreWiring extends Cadencize.DefaultCallback {
        public IgnoreWiring(final int considerVerilog) {
            super(considerVerilog);
        }
        public boolean used(CellInterface cell, HierName portCanon,
                            boolean wiring, boolean used) {
            return used;
        }
    }

    private Cadencize getCadencize(boolean verilog) {
        if (verilog) {
            // exclusion properties required for netgraph operations in
            // Prs2Verilog.emitVerilogBlock
            if (vcad == null)
                vcad = new Cadencize(true,
                                     new IgnoreWiring(Cadencize.VERILOG_ALL));
            return vcad;
        } else {
            if (cad == null)
                cad = new Cadencize(false,
                                    new IgnoreWiring(Cadencize.VERILOG_NONE));
            return cad;
        }
    }

    public boolean checkError() {
        return errorExist;
    }

    private static class GroupedChannel {
        private final CellInterface cell;
        private final Map<String,String> dirs;
        private final Map<String,String> groups;
        private final MultiMap/*<String,String>*/ input, output;
        public GroupedChannel(final CellInterface cell,
                              final Map<String,String> dirs) {
            this.cell = cell;
            this.dirs = dirs;
            this.groups = new HashMap/*<String,String>*/();
            this.input = new MultiMap/*<String,String>*/(
                    new LinkedHashMap(), MultiMap.ARRAY_LIST_FACTORY);
            this.output = new MultiMap/*<String,String>*/(
                    new LinkedHashMap(), MultiMap.ARRAY_LIST_FACTORY);
        }
        public String getGroupName(final String channel, final int direction) {
            final String groupName = dirs.remove(channel);
            if (groupName != null) {
                final String group;
                int idx = groupName.lastIndexOf('.');
                if (idx < 0) {
                    group = groupName;
                    logger.warning("In " + cell.getFullyQualifiedType() + ", " +
                                   "the group directive of " + channel + " " +
                                   "does not contain a dot.");
                } else {
                    group = groupName.substring(0, idx);
                }
                ((direction == PortDefinition.IN) ? input : output)
                    .put(group, channel);
                groups.put(channel, groupName);
                return group;
            } else {
                return null;
            }
        }
        public String lookupGroupName(final String channel) {
            final String group = groups.get(channel);
            return group == null ? null : group + "$$" + channel;
        }
        public boolean checkError() {
            boolean hasError = false;
            if (!dirs.isEmpty()) {
                logger.warning(cell.getFullyQualifiedType() + " contains " +
                               "group directives that didn't take effect: " +
                               dirs);
            }
            for (Iterator i = input.keySet().iterator(); i.hasNext(); ) {
                final String group = (String) i.next();
                final Collection bad = output.get(group);
                if (bad != null) {
                    logger.severe("In " + cell.getFullyQualifiedType() + ", " +
                                  "group " + group + " is associated with " +
                                  "both input and output channels.  Input " +
                                  "channels: " + input.get(group) + "; " +
                                  "output channels: " + bad);
                    hasError = true;
                }
            }
            return hasError;
        }
        private String v(String group) {
            return "\\" + group + "$$v ";
        }
        private String e(String group) {
            return "\\" + group + "$$e ";
        }
        private String enable(String chan) {
            return "\\" + chan + "$enable ";
        }
        private String data(String chan) {
            return "\\" + chan + "$data ";
        }
        private void writePort(final PrintWriter out, final MultiMap map,
                               final String valid, final String enable,
                               final String delim) {
            for (Iterator i = map.keySet().iterator(); i.hasNext(); ) {
                final String group = (String) i.next();
                out.println(delim);
                out.println(valid + " " + v(group) + delim);
                out.print(enable + " " + e(group));
            }
        }
        public void outputPorts(final PrintWriter out) {
            writePort(out, input, "input", "output reg", ",");
            writePort(out, output, "output reg", "input", ",");
        }
        public void outputDecl(final PrintWriter out,
                               final List<VerilogPort> verilogPorts) {
            writePort(out, input, "wire", "wire", ";");
            writePort(out, output, "wire", "wire", ";");
            out.println(';');
            for (VerilogPort port : verilogPorts) {
                if (port.getDirection() == VerilogPort.OUTPUT &&
                    port.getGroup() != null) {
                    out.println("wire " +
                                (port.getSignedness() ? "signed " : "") +
                                port.getVector() +
                                " \\" + port.getName() + " ;");
                }
            }
            for (Iterator i = input.keySet().iterator(); i.hasNext(); ) {
                final String group = (String) i.next();
                final Collection<String> chans = input.get(group);
                out.println("`CAST2VERILOG_CELEMENT " +
                            "#(.width(" + chans.size() + ")) " + 
                            "\\" + group + "$$c  (");
                out.println(".out (" + v(group) + "),");
                out.print(".in ({");
                for (Iterator j = chans.iterator(); j.hasNext(); ) {
                    final String chan = (String) j.next();
                    out.print(data(chan) + " >= 0");
                    if (j.hasNext()) out.print(",\n      ");
                }
                out.println("})");
                out.println(");");
            }

            for (Iterator i = output.keySet().iterator(); i.hasNext(); ) {
                final String group = (String) i.next();
                final Collection<String> chans = output.get(group);
                out.println("`CAST2VERILOG_CELEMENT " +
                            "#(.width(" + chans.size() + ")) " + 
                            "\\" + group + "$$c  (");
                out.println(".out (" + e(group) + "),");
                out.print(".in ({");
                for (Iterator j = chans.iterator(); j.hasNext(); ) {
                    final String chan = (String) j.next();
                    out.print(enable(chan));
                    if (j.hasNext()) out.print(",\n      ");
                }
                out.println("})");
                out.println(");");
            }

            for (VerilogPort port : verilogPorts) {
                if (port.getGroup() != null) {
                    if (port.getDirection() == VerilogPort.OUTPUT) {
                        out.println("assign " +
                                    data(port.getGroup().getSecond()) +
                                    " = " + v(port.getGroup().getFirst()) +
                                    " ? \\" + port.getName() + "  : -1;");
                    } else {
                        out.println("assign " +
                                    enable(port.getGroup().getSecond()) +
                                    " = " + e(port.getGroup().getFirst()) +
                                    ";");
                    }
                }
            }
        }
        private void writePort(final PrintWriter out, final MultiMap map,
                               final boolean output) {
            for (Iterator i = map.keySet().iterator(); i.hasNext(); ) {
                final String group = (String) i.next();
                for (Iterator j = map.get(group).iterator(); j.hasNext(); ) {
                    final String chan = (String) j.next();
                    out.println(",");
                    out.print(".\\" + lookupGroupName(chan) + "  (");
                    if (output) {
                        out.print("\\" + lookupGroupName(chan) + " ");
                    } else {
                        out.print(data(chan));
                    }
                    out.print(")");
                }
                out.println(",");
                out.println("." + v(group) + " (" + v(group) + "),");
                out.print("." + e(group) + " (" + e(group) + ")");
            }
        }
        public void outputInstantiation(final PrintWriter out,
                                        final List<VerilogPort> verilogPorts) {
            boolean first = true;
            for (VerilogPort port : verilogPorts) {
                if (port.getGroup() != null) continue;
                if (first) first = false;
                else out.println(",");
                out.print(".\\" + port.getName() + "  (\\" + port.getName() +
                          " )");
            }
            writePort(out, input, false);
            writePort(out, output, true);
        }
    }

    public Cast2Verilog(PrintWriter warningWriter,
                   PrintWriter errorWriter,
                   PrintWriter debugWriter,
                   final CoSimParameters params,
                   final int registerBitWidth,
                   final boolean enableSystemVerilog,
                   final boolean generateIfDef,
                   final boolean generateTb) {
        this.subcellNames = new HashMap();
        this.convertedCells = new HashSet /*<String>*/ (); 
        this.warningWriter = warningWriter;
        this.errorWriter = errorWriter;
        this.debugWriter = debugWriter;
        this.params = params;
        this.prsBehavior = null;
        this.registerBitWidth = registerBitWidth; 
        this.leafDelayBias = new HashMap();
        this.leafExtraDelay = new HashMap();
        this.enableSystemVerilog = enableSystemVerilog;
        this.generateIfDef = generateIfDef;
        this.generateTb = generateTb;
        this.errorExist = false;
    }

    /**
     * Find the largest blocks that can be processed completely by prs2verilog,
     * and store the instance names in <code>prsBehavior</code>.
     **/
    private boolean cellBehavior(final CellInterface cell,
                                 final HierName prefix) {
        if (cell.isNode() || cell.isChannel()) return true;
        final String instance = prefix == null ? null : prefix.getAsString('.');
        final int beh = instance == null ? CoSimParameters.DIGITAL
                                         : params.lookupBehavior(instance);
        if (beh == CoSimParameters.DIGITAL) {
            boolean prs = true;
            if (cell.containsSubcells()) { // mid-level cells
                for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                    final Pair p = (Pair) i.next();
                    final HierName subinst = (HierName) p.getFirst();
                    final CellInterface subcell = (CellInterface) p.getSecond();
                    prs &=
                        cellBehavior(subcell,
                                     HierName.append(prefix, subinst));
                }
            }
            if (prs) {
                getSubcellName(cell.getFullyQualifiedType(), null);
                prsBehavior.add(prefix);
            }
            return prs;
        } else if (beh == CoSimParameters.VERILOG &&
                   params.lookupVerilogLevel(instance) == null) {
            throw new RuntimeException("The \"verilog\" behavior is specified for " + cell.getFullyQualifiedType() + "/" + instance + ", but it is not supported.  You must specify a named block to use, e.g., \"verilog.rtl\".");
        }
        return false;
    }

    private void populateLeafDelaybias(final CellInterface cell,
                                       final HierName prefix,
                                       final InstanceData instData,
                                       final String verilogInst,
                                       final Cadencize cad,
                                       boolean wrapperAdded) {
        final String instance = prefix == null ? null : prefix.getAsString('.');
        final int beh = instance == null ? CoSimParameters.DIGITAL
                                         : params.lookupBehavior(instance);
        String newVerilogInst = verilogInst;
        if (beh == CoSimParameters.DIGITAL) {
            if (!wrapperAdded && prsBehavior.contains(prefix)) {
                newVerilogInst =
                    (verilogInst == null ? "" : verilogInst + ".") + "_";
                wrapperAdded = true;
            }
            if (cell.containsSubcells()) { // mid-level cells
                for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                    final Pair p = (Pair) i.next();
                    final CellInterface subcell = (CellInterface) p.getSecond();
                    if (subcell.isNode() || subcell.isChannel()) continue;

                    final HierName subinst = (HierName) p.getFirst();
                    final InstanceData newInstData =
                        instData.translate(cell, subcell, subinst, cad);
                    populateLeafDelaybias(subcell,
                                          HierName.append(prefix, subinst),
                                          newInstData,
                                          (newVerilogInst == null ?
                                              "" : newVerilogInst + ".") +
                                              VerilogUtil.escapeIfNeeded(
                                                  subinst.getAsString('.')),
                                          cad, wrapperAdded);
                }
            } else if (cell.hasRealProductionRule()) { // leaf cell
                final float total = instData.getDelayBias(null);
                // we must divide by the cell level delaybias in a leaf cell
                // has aleady been taken into account by CellDelay
                final float cellDelay =
                    ((Float) DirectiveUtils.getTopLevelDirective(
                        cell, DirectiveConstants.DELAYBIAS)).floatValue();

                // avoid NaNs in case delaybias = 0 in a cell
                final float curr = cellDelay == 0 ? 0 : total / cellDelay;

                if (curr != 1)
                    leafDelayBias.put(newVerilogInst, new Float(curr));

                final AliasedSet locals = cad.convert(cell).getLocalNodes();
                Map extraDelay = Collections.EMPTY_MAP;
                for (Iterator i = cell.getProductionRuleSet()
                                      .getProductionRules(); i.hasNext(); ) {
                    final ProductionRule prs = (ProductionRule) i.next();
                    final HierName target = prs.getTarget();
                    final HierName canon =
                        (HierName) locals.getCanonicalKey(target);
                    if (extraDelay.containsKey(canon)) continue;

                    final float up = instData.getExtraDelay(true, canon);
                    final float dn = instData.getExtraDelay(false, canon);
                    if (up != 0 || dn != 0) {
                        if (extraDelay == Collections.EMPTY_MAP)
                            extraDelay = new HashMap();
                        extraDelay.put(canon, new float[] { up, dn });
                    }
                }
                if (extraDelay != Collections.EMPTY_MAP)
                    leafExtraDelay.put(newVerilogInst, extraDelay);
            }
        }
    }

    /**
     * Return the appropriate module name for a cell with subcells behavior.
     **/
    private String getSubcellName(final String type, final Map subcells) {
        UniqueLabel label = (UniqueLabel) subcellNames.get(type);
        if (label == null) {
            label = new UniqueLabel(new HashMap());
            subcellNames.put(type, label);
        }
        final int n = label.getLabel(subcells);
        return type + (n == 0 ? "" : "$" + n);
    }
    
    /**
     * Return the behavior associated with an instance.
     **/
    private Mode getBehavior(final HierName instance) {
        final Mode m;
        if (instance == null) m = Mode.SUBCELLS;
        else if (prsBehavior.contains(instance)) m = Mode.PRS;
        else {
            final String inst = instance.getAsString('.');
            switch (params.lookupBehavior(inst)) {
              case CoSimParameters.DIGITAL: m = Mode.SUBCELLS; break;
              case CoSimParameters.JAVA: m = Mode.JAVA; break;
              case CoSimParameters.CSP: m = Mode.CSP; break;
              case CoSimParameters.VERILOG:
                m = new Mode.VerilogMode(params.lookupVerilogLevel(inst));
                break;
              default:
                throw new RuntimeException("Csp2Verilog does not support cosimulation: " + inst + " " + params.lookupBehavior(inst));
            }
        }
        return m;
    }

    private void logmem(final String heading) {
        logger.info(heading + " memory" +
                    " total = " + Runtime.getRuntime().totalMemory() +
                    " max = " + Runtime.getRuntime().maxMemory() +
                    " free = " + Runtime.getRuntime().freeMemory());
    }

    private void convert(final /*@ non_null @*/ CellInterface cell,
                         final HierName prefix,
                         final /*@ non_null @*/ PrintWriter out,
                         final PrintWriter behOut,
                         final CommandLineArgs theArgs)
        throws SemanticException {
        prsBehavior = new HashSet();
        cellBehavior(cell, prefix);

        cad = getCadencize(false);
        final InstanceData instData = new InstanceData();
        instData.updateDelayBias(cell);
        logmem("cadencize before");
        final AliasedSet localNodes = cad.convert(cell).getLocalNodes();
        logmem("cadencize after");
        instData.updateExtraDelay(cell, localNodes);
        if (!theArgs.argExists("ignore-asta-extra-delay")) {
            instData.updateAstaExtraDelay(cell, localNodes);
        }
        populateLeafDelaybias(cell, prefix, instData, null, cad, false);

        final String topName = convert(cell, prefix, out, behOut, true, true,
                                       false, theArgs);
        if (probFilter != null && probFilter.hasError()) {
            warningWriter.println("Fatal CSP errors found.\n");
            warningWriter.flush();
            System.exit(1);
        }

        annotateDelaybias(out, topName);
        annotateExtraDelay(out, topName);
    }

    private void ifdef(final String name, final PrintWriter out) {
        // Even with the ifdefs, in general, it is still not safe to
        // concatenate the output of multiple runs of cast2verilog, because the
        // name of a cell with the subcells behavior does not encode the
        // behaviors of the subcells it contains.  It is easy to fix this, for
        // example, by adding the MD5 hash of the names of the subcells to the
        // end of the name of a cell containing subcells, but the name would
        // be very ugly.
        if (generateIfDef) {
            out.println("`ifndef " + VerilogUtil.escapeIfNeeded(name));
            out.println("`define " + VerilogUtil.escapeIfNeeded(name) + " 1");
        }
    }

    private void endif(final PrintWriter out) {
        if (generateIfDef) {
            out.println("`endif");
        }
    }

    private void getTbDirective(final CellInterface cell,
                                final AliasedSet aliases,
                                final AliasedSet localAliases,
                                final HierName prefix,
                                final String directive,
                                final Map<HierName,String> result) {
        final Set<HierName> nodes = (Set<HierName>)
            DirectiveUtils.getExplicitTrues(
                DirectiveUtils.getTopLevelDirective(cell,
                    directive, DirectiveConstants.NODE_TYPE));
        nodes.stream()
             .filter(h -> aliases.getCanonicalKey(HierName.append(prefix, h)) != null)
             .map(h -> (HierName) localAliases.getCanonicalKey(h))
             .forEach(h -> result.put(h, directive));
    }

    private void getTbDirective(final CellInterface cell,
                                final AliasedSet aliases,
                                final AliasedSet localAliases,
                                final HierName prefix,
                                final Map<HierName,String> result) {
        getTbDirective(cell, aliases, localAliases, prefix, DirectiveConstants.GROUND_NET, result);
        getTbDirective(cell, aliases, localAliases, prefix, DirectiveConstants.POWER_NET, result);
        getTbDirective(cell, aliases, localAliases, prefix, DirectiveConstants.RESET_NET, result);
    }

    // Convert cell to verilog and emit to file 
    private String convert(final /*@ non_null @*/ CellInterface cell,
                           final HierName prefix,
                           final /*@ non_null @*/ PrintWriter out,
                           final PrintWriter behOut,
                           final boolean emitSlackWrappers,
                           final boolean topLevel,
                           final boolean narrowPort,
                           final CommandLineArgs theArgs)
        throws SemanticException {

        final String result;
        final String type = cell.getFullyQualifiedType();
        final Mode beh = getBehavior(prefix);

        final String logstr =
            "cell = " + type +
            " prefix = " + (prefix == null ? "null" : prefix.getAsString('.')) +
            " behavior = " + (beh == null ? "null" : beh.toString());
        logger.info("begin convert " + logstr);

        final Map<HierName,String> tbNets = new TreeMap<HierName,String>();
        if (topLevel && generateTb) {
            final AliasedSet aliases =
                getCadencize(false).convert(cell).getLocalNodes();
            getTbDirective(cell, aliases, aliases, null, tbNets);
            if (tbNets.isEmpty()) {
                // TODO: need to look at the environment as well
                final HierName hDut = HierName.makeHierName(dutInstance);
                final CellInterface dut = cell.getSubcell(hDut);
                if (dut != null) {
                    final AliasedSet localAliases =
                        getCadencize(false).convert(dut).getLocalNodes();
                    // use TESTBENCH aliases to make sure the node in the
                    // directive is a port, but store canonical name in the
                    // context of the DUT -- the implied port name will be the
                    // local net name in TESTBENCH
                    getTbDirective(dut, aliases, localAliases, hDut, tbNets);
                }
            }
        }

        if (beh == Mode.SUBCELLS) {
            // map from instance name to converted module name
            final Map<HierName,String> subcells =
                new TreeMap<HierName,String>(
                    NaturalStringComparator.getInstance());

            // map from instance name to whether its port should be narrow
            final Set/*<HierName>*/ narrowInstances =
                new HashSet/*<HierName>*/();

            final boolean narrowSubcell;
            if (prefix == null) {
                // this is the top-level cell contain the DUT and the
                // environment; if either requsted narrow ports, use narrow
                // ports here as well
                boolean narrowRequsted = false;
                for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                    final Pair p = (Pair) i.next();
                    final CellInterface subcell = (CellInterface) p.getSecond();
                    if (subcell.isNode() || subcell.isChannel()) continue;
                    narrowRequsted |=
                        params.isNarrowSubcell(
                            ((HierName) p.getFirst()).getAsString('.'));
                }
                narrowSubcell = narrowRequsted;
            } else {
                narrowSubcell = params.isNarrowSubcell(prefix.getAsString('.'));
            }

            if (narrowPort || (narrowSubcell && topLevel))
                narrowInstances.add(null);

            for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair/*<HierName,CellInterface>*/ pair = (Pair) i.next();
                final CellInterface subcell = (CellInterface) pair.getSecond();

                // ignore node and channel subcells
                if (subcell.isNode() || subcell.isChannel()) continue;

                final HierName instance = (HierName) pair.getFirst();
                final HierName complete = HierName.append(prefix, instance);

                final Mode subbeh = getBehavior(complete);

                // this subcell is a wiring cell; inline aliases, and don't
                // generate anything
                if (subbeh == Mode.PRS && !subcell.hasRealProductionRule())
                    continue;

                logger.info("found subcell " + instance + " of type " +
                            subcell.getFullyQualifiedType());

                // CSP and Java behaviors always have wide ports
                if (subbeh != Mode.CSP && subbeh != Mode.JAVA && narrowSubcell)
                    narrowInstances.add(instance);

                // get the subcell's converted module name
                final String converted =
                    convert(subcell, complete, out, behOut, emitSlackWrappers,
                            false, narrowSubcell, theArgs);

                subcells.put(instance, converted);
            }

            // determine if this subcell has been processed already
            final String suffix = narrowPort ? "$narrow" : "";
            result = topLevel && generateTb ? "TESTBENCH"
                                            : getSubcellName(type + suffix, subcells);
            if (convertedCells.add(result)) {
                ifdef(result, out);
                emitModuleForCellWithSubcells(cell, prefix, result, subcells,
                                              topLevel, tbNets, narrowInstances,
                                              out);
                endif(out);
            }
        } else if (beh == Mode.CSP) {
            result = type + "$csp";
            behOut.println(cell.getFullyQualifiedType() + " " + prefix +
                           " csp");
            if (convertedCells.add(result)) {
                ifdef(result, out);
                emitCspLeaf(cell, result, out, emitSlackWrappers);
                endif(out);
            }
        } else if (beh == Mode.JAVA) {
            result = type + "$java";
            // write out header for module
            if (convertedCells.add(result)) {
                ifdef(result, out);

                final Map<String,String> dirs = (Map<String,String>)
                    DirectiveUtils.getJavaDirective(
                        cell,
                        DirectiveConstants.GROUP,
                        DirectiveConstants.WIDE_CHANNEL_TYPE);
                final List<VerilogPort> verilogPorts =
                    new ArrayList<VerilogPort>();

                final GroupedChannel groupedChannel =
                    new GroupedChannel(cell, new HashMap<String,String>(dirs));

                out.println("module \\" + result + "$body (");
                emitPortList(cell, null, null, null, null, null,
                             out, true, true, false, verilogPorts,
                             groupedChannel);
                groupedChannel.outputPorts(out);
                out.println(");");

                errorExist |= groupedChannel.checkError();

                final JavaCoSimInfo jcsi = cell.getJavaCoSimInfo();
                for (Iterator i = jcsi.getClassNames().iterator();
                     i.hasNext(); ) {
                    final String name = (String) i.next();
                    out.println("initial $instantiate(\"" + name + "\");");
                }
                out.println("endmodule      // java block");

                out.println();

                out.println("module \\" + result + "$wrap (");
                emitPortList(cell, null, null, null, null, null,
                             out, true, true, false);
                out.print(")");
                groupedChannel.outputDecl(out, verilogPorts);
                out.println("\\" + result + "$body  body(");
                groupedChannel.outputInstantiation(out, verilogPorts);
                out.println(");");
                out.println("endmodule");
                out.println();

                emitSlackWrapper(cell, result, result + "$wrap",
                                 BlockInterface.JAVA, out);

                endif(out);
            }
        } else if (beh instanceof Mode.VerilogMode || beh == Mode.PRS) {
            final CellImpl wrapper = getWrapperCell(cell);
            result =
                CellUtils.hashMetaParameters(wrapper.getFullyQualifiedType());
            behOut.println(cell.getFullyQualifiedType() + " " + prefix +
                           " digital");
            if (convertedCells.add(result)) {
                try {
                    Prs2Verilog.emitVerilogBlock(
                        wrapper, getCadencize(true),
                        beh == Mode.PRS ? null
                                        : ((Mode.VerilogMode) beh).getLevel(),
                        new NoRepeatVisitor(out, prsDone),
                        new UnionCommandLineArgs(theArgs,
                            new CommandLineArgsDefImpl(
                                new String[] { "--timescale" })),
                        validVerilogFiles);
                } catch (IOException e) {
                    throw new SemanticException(e);
                }
            }
        } else
            throw new AssertionError("No suitable behavior found in " +
                                     cell.getFullyQualifiedType());

        logger.info("end convert " + logstr);
        return result;
    }

    /**
     * Add object <code>o</code> into the collection <code>c</code> if
     * <code>c</code> is not <code>null</code>.  Otherwise, do nothing.
     **/
    private static void addNonNull(Collection c, Object o) {
        if (c != null) c.add(o);
    }

    private static class VerilogPort {
        public static final String INPUT = "input";
        public static final String OUTPUT = "output";
        public static final String INOUT = "inout";
        public static final String WIRE = "wire";
        public static final String REG = "reg";
        private String direction;
        private boolean signedness;
        private Pair<String,String> group;
        private String netType;
        private String vector;
        private String name;
        public VerilogPort() { }
        public void setDirection(String direction) {
            this.direction = direction;
        }
        public void setSignedness(boolean signedness) {
            this.signedness = signedness;
        }
        public void setGroup(Pair<String,String> group) { this.group = group; }
        public void setNetType(String netType) { this.netType = netType; }
        public void setVector(String vector) { this.vector = vector; }
        public void setName(String name) { this.name = name; }
        public String getName() { return name; }
        public String getVector() { return vector; }
        public Pair<String,String> getGroup() { return group; }
        public String getDirection() { return direction; }
        public boolean getSignedness() { return signedness; }
    }

    private void walkPortList(final CSPCellInfo cellInfo,
                              final boolean narrowPorts,
                              final MarkPort marker) {
        for (PortDefinition port :
                new IterableIterator<PortDefinition>(
                    new MappingIterator<PortDefinition,PortDefinition>(
                            cellInfo.getPortDefinitions(),
                            port -> new PortDefinition(
                                        port.getName(),
                                        mungeArrays(port.getType(),
                                                    narrowPorts),
                                        port.getDirection())))) {
            marker.mark(Collections.singleton(port).iterator());
        }
    }

    /**
     * Fills in (data nodes, enable nodes, arrayInputNode, arrayOutputNodes).
     **/
    private void emitPortList(CellInterface cell,
                              List/*<String>*/ dataNodes,
                              List/*<String>*/ enableNodes,
                              List/*<Pair<String,Integer>>*/ arrayInputNodes,
                              List/*<Pair<String,Integer>>*/
                                  arrayOutputNodes,
                              List/*<String>*/ inputPorts,
                              PrintWriter out,
                              boolean isLeaf) {
        emitPortList(cell, dataNodes, enableNodes, arrayInputNodes,
                     arrayOutputNodes, inputPorts, out, isLeaf, false, false);
    }

    private void emitPortList(CellInterface cell,
                              List/*<String>*/ dataNodes,
                              List/*<String>*/ enableNodes,
                              List/*<Pair<String,Integer>>*/ arrayInputNodes,
                              List/*<Pair<String,Integer>>*/
                                  arrayOutputNodes,
                              List/*<String>*/ inputPorts,
                              PrintWriter out,
                              boolean isLeaf,
                              boolean inJava,
                              boolean narrowPorts) {
        emitPortList(cell, dataNodes, enableNodes, arrayInputNodes,
                     arrayOutputNodes, inputPorts, out, isLeaf, inJava,
                     narrowPorts, null, null);
    }

    private void emitPortList(CellInterface cell,
                              List/*<String>*/ dataNodes,
                              List/*<String>*/ enableNodes,
                              List/*<Pair<String,Integer>>*/ arrayInputNodes,
                              List/*<Pair<String,Integer>>*/
                                  arrayOutputNodes,
                              List/*<String>*/ inputPorts,
                              PrintWriter out,
                              boolean isLeaf,
                              boolean inJava,
                              boolean narrowPorts,
                              List<VerilogPort> verilogPorts,
                              GroupedChannel groupedChannels) {
        // and the real ports
        final boolean[] first = new boolean[] { true };
        final CSPCellInfo cellInfo = cell.getCSPInfo();
        for (Iterator i = cellInfo.getPortDefinitions(); i.hasNext(); ) {
            final PortDefinition portDefinition = (PortDefinition) i.next();
            emitPorts(cell,
                      portDefinition.getName(),
                      portDefinition.getType(),
                      portDefinition.getDirection(),
                      dataNodes, enableNodes,
                      arrayInputNodes, arrayOutputNodes, inputPorts, false, 
                      first, out, isLeaf, inJava, narrowPorts,
                      verilogPorts, groupedChannels);
        }
    }


    private void emitPorts(final CellInterface cell,
                           final String name,
                           final PortTypeInterface portType,
                           final int direction,
                           final List/*<String>*/ dataNodes,
                           final List/*<String>*/ enableNodes,
                           final List/*<Pair<String,Integer>>*/
                               arrayInputNodes,
                           final List/*<Pair<String,Integer>>*/
                               arrayOutputNodes,
                           final List/*<String>*/ inputPorts,
                           final boolean isArray,
                           final boolean[] first, 
                           PrintWriter out,
                           boolean isLeaf,
                           boolean inJava,
                           boolean narrowPorts,
                           final List<VerilogPort> verilogPorts,
                           final GroupedChannel groupedChannels) {
        if (portType instanceof com.avlsi.fast.ports.ArrayType) {
            final com.avlsi.fast.ports.ArrayType arrayType =
                (com.avlsi.fast.ports.ArrayType) portType;

            for (int i = arrayType.getMinIndex();
                 i <= arrayType.getMaxIndex(); ++i) {
                emitPorts(cell, name + '[' + i + ']',
                          arrayType.getArrayedType(),
                          direction, dataNodes, enableNodes,
                          arrayInputNodes, arrayOutputNodes, inputPorts,
                          true, first, out, isLeaf, inJava, narrowPorts,
                          verilogPorts, groupedChannels);
            }
        } else if (portType instanceof ChannelType) {
            final ChannelType chanType = (ChannelType) portType;
            if (chanType.isArrayed() && narrowPorts) {
                for (int i = 0; i < chanType.getWidth(); ++i) {
                    emitPorts(cell, name + '[' + i + ']',
                              new ChannelType(chanType.iterator(),
                                              chanType.getTypeName(),
                                              chanType.getNumValues()),
                              direction, dataNodes, enableNodes,
                              arrayInputNodes, arrayOutputNodes, inputPorts,
                              isArray, first, out, isLeaf, inJava, narrowPorts,
                              verilogPorts, groupedChannels);
                }
            } else {
                final ChannelEmitter emitter = getChannelEmitter(chanType);

                final VerilogPort vdata = new VerilogPort();
                final VerilogPort venable = new VerilogPort();

                final String prettyName =
                    StringUtil.replaceSubstring(name, "][", ",");
                final String data = '\\' + prettyName + "$data ";
                vdata.setName(prettyName + "$data");
                final String enable = '\\' + prettyName + "$enable ";
                venable.setName(prettyName + "$enable");

                // If we are part of an array, emit the outputs as
                // wires because the regs will be internal.
                // If we are not a leaf cell, emit the outputs as
                // wires as the regs will be declared in a lower
                // level cell
                final boolean outWire =
                    ((isArray || !isLeaf) && !inJava) ||
                    (inJava && groupedChannels == null);
                final String outputType = outWire ? "output" : "output reg";
                final String forwardDirection;
                final String reverseDirection;

                if (direction == PortDefinition.OUT) {
                    if (isArray) {
                        addNonNull(arrayInputNodes,
                                new Pair/*<String,Integer>*/(
                                    enable, new Integer(1)));
                        addNonNull(arrayOutputNodes,
                                new Pair/*<String,Integer>*/(
                                    data, new Integer(-1)));
                    } else {
                        addNonNull(dataNodes, data);
                        addNonNull(inputPorts, enable);
                    }

                    forwardDirection = outputType;
                    vdata.setDirection(VerilogPort.OUTPUT);
                    vdata.setNetType(outWire ? VerilogPort.WIRE
                                             : VerilogPort.REG);
                    reverseDirection = "input";
                    venable.setDirection(VerilogPort.INPUT);
                    venable.setNetType(VerilogPort.WIRE);
                } else {
                    assert direction == PortDefinition.IN;
                    if (isArray) {
                        addNonNull(arrayInputNodes,
                                new Pair/*<String,Integer>*/(
                                    data, new Integer(-1)));
                        addNonNull(arrayOutputNodes,
                                new Pair/*<String,Integer>*/(
                                    enable, new Integer(1)));
                    } else {
                        addNonNull(enableNodes, enable);
                        addNonNull(inputPorts, data);
                    }

                    forwardDirection = "input";
                    vdata.setDirection(VerilogPort.INPUT);
                    vdata.setNetType(VerilogPort.WIRE);
                    reverseDirection = outputType;
                    venable.setDirection(VerilogPort.OUTPUT);
                    venable.setNetType(outWire ? VerilogPort.WIRE
                                               : VerilogPort.REG);
                }
                if (!first[0])
                    out.println(',');
                /* Always print out wide channels */
                final String bitWidth =
                    "[" + getNumBitsWide(chanType) + ":0]";
                final String fullGroup, groupName;
                if (groupedChannels == null) {
                    fullGroup = null;
                    groupName = null;
                } else {
                    groupName = groupedChannels.getGroupName(prettyName,
                                                             direction);
                    fullGroup = groupedChannels.lookupGroupName(prettyName);
                }
                if (fullGroup == null) {
                    out.println(forwardDirection + " signed " + bitWidth + " " +
                                data + ',');
                    out.print(reverseDirection + ' ' + enable);
                    addNonNull(verilogPorts, vdata);
                    addNonNull(verilogPorts, venable);
                } else {
                    out.print(forwardDirection + " signed " + bitWidth + " " +
                              "\\" + fullGroup + ' ');
                    vdata.setName(fullGroup);
                    vdata.setGroup(new Pair<String,String>(groupName,
                                                           prettyName));
                    addNonNull(verilogPorts, vdata);
                }
                vdata.setSignedness(true);
                vdata.setVector(bitWidth);
                first[0] = false;
            }
        } else if (portType instanceof com.avlsi.fast.ports.NodeType) {
            // XXX: what to do about initial value?
            // Ignore for now
            final VerilogPort vnode = new VerilogPort();
            final NodeType nodeType = (NodeType) portType;
            if (!first[0])
                out.println(',');

            final String comma = StringUtil.replaceSubstring(name, "][", ",");
            final String prettyName = '\\' + comma + ' ';
            vnode.setName(comma);

            final String dir;
            vnode.setNetType(VerilogPort.WIRE);
            if (direction == PortDefinition.OUT) {
                final boolean outWire = ((isArray || !isLeaf) && !inJava) ||
                                        (inJava && groupedChannels == null);
                dir = outWire ? "output" : "output reg";
                vnode.setDirection(VerilogPort.OUTPUT);
                if (!outWire) vnode.setNetType(VerilogPort.REG);
            } else {
                if (direction == PortDefinition.IN) {
                    dir = "input";
                    vnode.setDirection(VerilogPort.INPUT);
                } else {
                    dir = "inout";
                    vnode.setDirection(VerilogPort.INOUT);
                }
                addNonNull(inputPorts, prettyName);
            }

            if (isArray) {
                final Collection which = 
                    direction == PortDefinition.OUT ?
                        arrayOutputNodes
                      : (direction == PortDefinition.IN ? arrayInputNodes
                                                        : null);

                // if neither array input ports or array output ports are
                // requested, then it makes no difference which collection to
                // add the port to, so suppress the warning in that case
                if (direction == PortDefinition.INOUT &&
                    (arrayOutputNodes != null || arrayInputNodes != null))
                    logger.severe("In " + cell.getFullyQualifiedType() +
                                  ", port " + prettyName +
                                  "may not be handled correctly, because it " +
                                  "is declared as inout, and is part of an " +
                                  "array.");

                addNonNull(which,
                           new Pair/*<String,Integer>*/(prettyName, null));
            }

            if (nodeType.isArrayed()) {
                final String width = "[" + (nodeType.getWidth() - 1) + ":0]";
                vnode.setVector(width);
                out.print(dir + " " + width + " " + prettyName);
            } else {
                out.print(dir + " " + prettyName);
            }

            first[0] = false;
            addNonNull(verilogPorts, vnode);
        } else {
            assert portType instanceof com.avlsi.fast.ports.StructureType;
            final com.avlsi.fast.ports.StructureType structureType =
                (com.avlsi.fast.ports.StructureType) portType;

            if (isLeaf && !inJava &&
                (CellUtils.isDftChannel(cell, structureType.getTag()) ||
                 CellUtils.isSramSerialChannel(structureType.getTag()))) {
                return;
            }

            for (Iterator i = structureType.iterator(); i.hasNext(); ) {
                final PortDefinition portDefinition =
                    (PortDefinition) i.next();

                emitPorts(cell,
                          name + '.' + portDefinition.getName(),
                          portDefinition.getType(),
                          PortDefinition.updateDirection(
                              direction,
                              portDefinition.getDirection()),
                          dataNodes, enableNodes,
                          arrayInputNodes, arrayOutputNodes, inputPorts,
                          isArray, first, out, isLeaf, inJava, narrowPorts,
                          verilogPorts, groupedChannels);
            }
        }
    }

    private void repeatN(final String format, final int N,
                         final PrintWriter out) {
        repeatN(new MessageFormat(format), N, out);
    }

    private void repeatN(final MessageFormat format, final int N,
                         final PrintWriter out) {
        repeatN(format, N, "\n", out);
        out.println();
    }

    private void repeatN(final String format, final int N,
                         final String sep, final PrintWriter out) {
        repeatN(new MessageFormat(format), N, sep, out);
    }
    private void repeatN(final MessageFormat format, final int N,
                         final String sep, final PrintWriter out) {
        for (int i = 0; i < N; ++i) {
            out.print(format.format(new Object[] { String.valueOf(i) }));
            if (i < N - 1) out.print(sep);
        }
    }

    /**
     * Returns a port definition with all arrays pushed down to the
     * leaves and structures brought up to the top.
     **/
    private PortTypeInterface mungeArrays(
            final PortTypeInterface portType,
            final boolean narrowPorts) {
        return mungeArrays(portType, null, narrowPorts);
    }

    private PortTypeInterface mungeArrays(
            final PortTypeInterface portType,
            final ArrayType arrays,
            final boolean narrowPorts) {
        if (portType instanceof ArrayType) {
            final ArrayType arrayType = (ArrayType) portType;

            return mungeArrays(arrayType.getArrayedType(),
                               (ArrayType)
                                   substituteArray(arrays,
                                       new ArrayType(
                                           null, arrayType.getMinIndex(),
                                           arrayType.getMaxIndex())),
                               narrowPorts);
        } else if (portType instanceof ChannelType) {
            final ChannelType channelType = (ChannelType) portType;
            if (narrowPorts && channelType.isArrayed()) {
                return mungeArrays(
                        new ArrayType(channelType.getSingular(),
                                      0,
                                      channelType.getWidth() - 1),
                        arrays, narrowPorts);
            } else {
                return substituteArray(arrays, portType);
            }
        } else if (portType instanceof NodeType) {
            return substituteArray(arrays, portType);
        } else {
            assert portType instanceof StructureType;
            final StructureType structureType =
                (StructureType) portType;
            final StructureType newStructureType =
                new StructureType();

            for (Iterator i = structureType.iterator(); i.hasNext(); ) {
                final PortDefinition portDefinition =
                    (PortDefinition) i.next();
                newStructureType.add(
                        new PortDefinition(
                            portDefinition.getName(),
                            mungeArrays(portDefinition.getType(), arrays,
                                        narrowPorts),
                            portDefinition.getDirection()));
            }

            return newStructureType;
        }
    }

    /**
     * Substitutes arrayType in for the blank space denoted by
     * <code>null</code> in <code>arrays</code>.  <code>arrays</code>
     * is an <code>ArrayType</code> of <code>ArrayTypes</code>,
     * with the innermost arrayed type as <code>null</code>.
     **/
    private PortTypeInterface substituteArray(
            final ArrayType arrays,
            final PortTypeInterface type) {
        if (arrays == null) {
            return type;
        } else {
            return new ArrayType(
                    substituteArray(
                        (ArrayType)
                            arrays.getArrayedType(),
                        type),
                    arrays.getMinIndex(),
                    arrays.getMaxIndex());
        }
    }

    /**
     * Moves all array accesses to the end of the string <code>s</code>.
     **/
    private static Pair<String,String> mungeArray(String s) {
        // Recover ]['s that were deleted earlier
        s = StringUtil.replaceSubstring(s, ",", "][");
        final StringBuffer nonArrayPart = new StringBuffer(s.length());
        final StringBuffer arrayPart = new StringBuffer(s.length());
        final Pattern p = Pattern.compile("\\[\\d+\\]");
        final Matcher m = p.matcher(s);
        int start = 0;
        while (m.find(start)) {
            nonArrayPart.append(s.substring(start, m.start()));
            arrayPart.append(s.substring(m.start(), m.end()));
            start = m.end();
        }
        nonArrayPart.append(s.substring(start, s.length()));
        return new Pair<>(nonArrayPart.toString(), arrayPart.toString());
    }

    private void emitHelperFunctions(PrintWriter out) {
        final String size = "[" + (registerBitWidth - 1) + ":0]";
        ifdef("CAST2VERILOG_RUNTIME_UTIL", out);
        out.println("module util;");

        out.println("function signed " + size + " sign_extend(");
        out.println("   input signed " + size + " val);");
        out.println("sign_extend = val;");
        out.println("endfunction");
        out.println();

        out.println("function signed " + size + " pow(");
        out.println("   input signed " + size + " x,");
        out.println("   input signed " + size + " y);");
        out.println("pow = y >= 0 ? x ** y : (x == 0 ? 0 : 1 / (x ** -y));");
        out.println("endfunction");
        out.println();

        // a trivial implementation of log2
        out.println("function signed " + size + " log2(");
        out.println("   input signed " + size + " x);");
        out.println("bit " + size + " temp;");
        out.println("integer i;");
        out.println("begin");
        out.println("if (x < 1) temp = -x;");
        out.println("else temp = x - 1;");
        out.println("log2 = 0;");
        out.println("i = 0;");
        out.println("while (temp !== 0) begin");
        out.println("i = i + 1;");
        out.println("case (temp[0])");
        out.println("1'b1: log2 = i;");
        out.println("1'bx: begin temp = 0; log2 = 1'bx; end");
        out.println("1'bz: begin temp = 0; log2 = 1'bz; end");
        out.println("endcase");
        out.println("temp = temp >> 1;");
        out.println("end");
        out.println("end");
        out.println("endfunction");
        out.println();

        out.println("function signed " + size + " log4(");
        out.println("   input signed " + size + " x);");
        out.println("begin");
        out.println("log4 = (log2(x) + 1) / 2;");
        out.println("end");
        out.println("endfunction");
        out.println();

        out.println("function signed " + size + " divide(");
        out.println("   input signed " + size + " numer,");
        out.println("   input signed " + size + " denom);");
        // Verilog divide rounds to 0, just like csp, so we don't need
        // to compensate for that.
        out.println("divide = denom != 0 ? numer / denom : 0;");
        out.println("endfunction");
        out.println();

        out.println("function signed " + size + " remainder(");
        out.println("   input signed " + size + " numer,");
        out.println("   input signed " + size + " denom);");
        // Verilog divide rounds to 0, just like csp, so we don't need
        // to compensate for that.
        out.println("remainder = denom != 0 ? numer % denom : numer;");
        out.println("endfunction");
        out.println();

        out.println("function signed " + size + " shift_left(");
        out.println("   input signed " + size + " left,");
        out.println("   input signed " + size + " right);");
        out.println("shift_left = right >= 0 ? left <<< right : left >>> -right;");
        out.println("endfunction");
        out.println();

        out.println("function signed " + size + " shift_right(");
        out.println("   input signed " + size + " left,");
        out.println("   input signed " + size + " right);");
        out.println("shift_right = right >= 0 ? left >>> right : left <<< -right;");
        out.println("endfunction");
        out.println();

        out.println("function " + size + " posmod(");
        out.println("    input signed " + size + " numer,");
        out.println("    input signed " + size + " denom);");
        out.println("posmod = $unsigned((numer % denom + denom) % denom);");
        out.println("endfunction");
        out.println();

        // XXX: handle negatives and bad ranges
        out.println("function signed " + size + " bit_extract2(");
        out.println("    input signed " + size + " val,");
        out.println("    input signed " + size + " max_bit);");
        out.println("bit_extract2 = bit_extract3(val, max_bit, max_bit);");
        out.println("endfunction");
        out.println();

        out.println("function signed " + size + " bit_extract3(");
        out.println("    input signed " + size + " val,");
        out.println("    input signed " + size + " max_bit,");
        out.println("    input signed " + size + " min_bit);");
        out.println("bit_extract3 = " +
                    "(val & ((1 << (max_bit + 1)) - 1)) >> min_bit;");
        out.println("endfunction");
        out.println();

        // XXX: handle negatives and bad ranges
        final Collection kinds = CollectionUtils.addAll(
            new HashSet(),
            new String[] { "", "add", "subtract", "multiply", "divide",
                           "remainder", "and", "or", "xor", "leftshift",
                           "rightshift" }
        );
        for (Iterator i = kinds.iterator(); i.hasNext(); ) {
            final String kind = (String) i.next();
            final String name3 = "bit_insert_" + kind + "3";
            final String name4 = "bit_insert_" + kind + "4";
            out.println("task " + name3 + "(");
            out.println("    inout signed " + size + " old_val,");
            out.println("    input signed " + size + " new_val,");
            out.println("    input signed " + size + " max_bit);");
            out.println(name4 + "(old_val, new_val, max_bit, max_bit);");
            out.println("endtask");
            out.println();
        }

        for (Iterator i = kinds.iterator(); i.hasNext(); ) {
            final String kind = (String) i.next();
            final String name4 = "bit_insert_" + kind + "4";
            out.println("task " + name4 + "(");
            out.println("    inout signed " + size + " old_val,");
            out.println("    input signed " + size + " new_val,");
            out.println("    input signed " + size + " max_bit,");
            out.println("    input signed " + size + " min_bit);");
            out.println("bit signed " + size + " mask;");
            out.println("bit signed " + size + " curr;");
            out.println("begin");
            if (kind.equals("")) {
                out.println("curr = new_val;");
            } else {
                out.println("curr = bit_extract3(old_val, max_bit, min_bit);");
                out.println("assign_" + kind + "(curr, new_val);");
            }
            out.println("mask = (1 << (max_bit + 1)) - (1 << min_bit);");
            out.println("old_val = (old_val & ~mask) " +
                        "| ((curr << min_bit) & mask);");
            out.println("end");
            out.println("endtask");
            out.println();
        }


        // Random number generator
        // XXX: handle negatives and bad ranges
        out.println("function " + size + " random(");
        out.println("    input signed " + size + " num_bits);");
        out.println("integer words, i, remaining_bits, msb_bits;");
        out.println("begin");
        // set # of 32-bit words in num_bits
        out.println("words = num_bits / 32;");
        // initialize entire integer to 0
        out.println("random = 0;");
        out.println("for (i = 0; i < words; i=i+1) begin");
        out.println("    random[i*32 +:32] = $random;");
        out.println("end");
        // set remaining bits: j = num_bits - 32*32bitchunks
        out.println("remaining_bits = num_bits - words*32;");
        out.println("// Use shift and bitwise or to avoid verilog's");
        out.println("// restrictions on part selects");
        out.println("msb_bits = {$random} % (1<<remaining_bits);");
        out.println("random = random | (msb_bits << (words*32));");
        out.println("end");
        out.println("endfunction");
        out.println();

        // task to print out a value in hexdecimal; output starts with - if
        // value is negative
        out.println("task automatic display_hex;");
        out.println("input signed val;");
        out.println("bit signed " + size + " val;");
        out.println("if (val < 0) $write(\"-0x%0h\", -val);");
        out.println("else $write(\"0x%0h\", val);");
        out.println("endtask");
        out.println();

        // csp_string(x, 16) doesn't quite print out what we want
        out.println("function [`CSP_STRING_WHOLE] hex_string(");
        out.println("  input signed " + size + " val);");
        out.println("bit signed [`CSP_STRING_ASCII] temp;");
        out.println("integer len;");
        out.println("begin");
        out.println("    if (val < 0) $sformat(temp, \"-0x%0h\", -val);");
        out.println("    else $sformat(temp, \"0x%0h\", val);");
        out.println("    len = 0;");
        out.println("    while ((len + 1) <= `CSP_STRING_MAX && temp[(len + 1) * 8 -: 8] > 0) begin");
        out.println("        len = len + 1;");
        out.println("    end ");
        out.println("    hex_string[`CSP_STRING_LENGTH] = len;");
        out.println("    hex_string[`CSP_STRING_ASCII] = temp;");
        out.println("end");
        out.println("endfunction");
        out.println();

        out.println("task assign_concat;");
        out.println("inout [`CSP_STRING_WHOLE] variable;");
        out.println("input [`CSP_STRING_WHOLE] val;");
        out.println("`CSP_STRING variable;");
        out.println("`CSP_STRING val;");
        out.println("variable = csp_string.concat(variable, val);");
        out.println("endtask");
        out.println();

        out.println("task assign_add;");
        out.println("inout signed variable;");
        out.println("input signed val;");
        out.println("bit signed " + size + " variable;");
        out.println("bit signed " + size + " val;");
        out.println("variable = variable + val;");
        out.println("endtask");
        out.println();

        out.println("task assign_subtract;");
        out.println("inout signed variable;");
        out.println("input signed val;");
        out.println("bit signed " + size + " variable;");
        out.println("bit signed " + size + " val;");
        out.println("variable = variable - val;");
        out.println("endtask");
        out.println();

        out.println("task assign_multiply;");
        out.println("inout signed variable;");
        out.println("input signed val;");
        out.println("bit signed " + size + " variable;");
        out.println("bit signed " + size + " val;");
        out.println("variable = variable * val;");
        out.println("endtask");
        out.println();

        out.println("task assign_divide;");
        out.println("inout signed variable;");
        out.println("input signed val;");
        out.println("bit signed " + size + " variable;");
        out.println("bit signed " + size + " val;");
        out.println("variable = divide(variable, val);");
        out.println("endtask");
        out.println();

        out.println("task assign_remainder;");
        out.println("inout signed variable;");
        out.println("input signed val;");
        out.println("bit signed " + size + " variable;");
        out.println("bit signed " + size + " val;");
        out.println("variable = remainder(variable, val);");
        out.println("endtask");
        out.println();

        out.println("task assign_and;");
        out.println("inout signed variable;");
        out.println("input signed val;");
        out.println("bit signed " + size + " variable;");
        out.println("bit signed " + size + " val;");
        out.println("variable = variable & val;");
        out.println("endtask");
        out.println();

        out.println("task assign_or;");
        out.println("inout signed variable;");
        out.println("input signed val;");
        out.println("bit signed " + size + " variable;");
        out.println("bit signed " + size + " val;");
        out.println("variable = variable | val;");
        out.println("endtask");
        out.println();

        out.println("task assign_xor;");
        out.println("inout signed variable;");
        out.println("input signed val;");
        out.println("bit signed " + size + " variable;");
        out.println("bit signed " + size + " val;");
        out.println("variable = variable ^ val;");
        out.println("endtask");
        out.println();

        out.println("task assign_leftshift;");
        out.println("inout signed variable;");
        out.println("input signed val;");
        out.println("bit signed " + size + " variable;");
        out.println("bit signed " + size + " val;");
        out.println("variable = shift_left(variable, val);");
        out.println("endtask");
        out.println();

        out.println("task assign_rightshift;");
        out.println("inout signed variable;");
        out.println("input signed val;");
        out.println("bit signed " + size + " variable;");
        out.println("bit signed " + size + " val;");
        out.println("variable = shift_right(variable, val);");
        out.println("endtask");
        out.println();

        out.println("function [`CSP_STRING_WHOLE] csp_string(" +
                    "input signed " + size + " value, " +
                    "input signed " + size + " base);");
        out.println("bit [36*8:1] digits;");
        out.println("bit signed " + size + " pos;");
        out.println("bit signed [`CSP_STRING_ASCII] temp;");
        out.println("bit [5:0] b, rem;");
        out.println("bit [7:0] neg;");
        out.println("integer len;");
        out.println("begin");
        out.println("    digits = \"zyxwvutsrqponmlkjihgfedcba9876543210\";");
        out.println("    b = base;");
        out.println("    if (base < 2 || base > 36) begin");
        out.println("        $display(\"%t:%m: Invalid base %d for string conversion\", $time, base);");
        out.println("        b = 10;");
        out.println("    end");
        out.println("    if (value < 0) begin");
        out.println("        pos = -value;");
        out.println("        neg = \"-\";");
        out.println("    end");
        out.println("    else begin");
        out.println("        pos = value;");
        out.println("        neg = \"\";");
        out.println("    end");
        out.println("    if (pos == 0) $sformat(temp, \"0\");");
        out.println("    else if (base == 2) $sformat(temp, \"%0s%0b\", neg, pos);");
        out.println("    else if (base == 8) $sformat(temp, \"%0s%0o\", neg, pos);");
        out.println("    else if (base == 10) $sformat(temp, \"%0s%0d\", neg, pos);");
        out.println("    else if (base == 16) $sformat(temp, \"%0s%0h\", neg, pos);");
        out.println("    else begin");
        out.println("        temp = 0;");
        out.println("        len = 1;");
        out.println("        while (pos > 0) begin");
        out.println("            rem = pos % b;");
        out.println("            pos = pos / b;");
        out.println("            temp[len * 8 -: 8] = digits[(rem + 1) * 8 -: 8];");
        out.println("            len = len + 1;");
        out.println("        end");
        out.println("        if (neg > 0) temp[len * 8 -: 8] = \"-\";");
        out.println("    end");
        out.println("    len = 0;");
        out.println("    while ((len + 1) <= `CSP_STRING_MAX && temp[(len + 1) * 8 -: 8] > 0) begin");
        out.println("        len = len + 1;");
        out.println("    end");
        out.println("    csp_string[`CSP_STRING_LENGTH] = len;");
        out.println("    csp_string[`CSP_STRING_ASCII] = temp;");
        out.println("end");
        out.println("endfunction");
        out.println();

        out.println("function [`CSP_STRING_WHOLE] posmod_warning(" +
                    "input signed " + size + " message, " +
                    "input signed " + size + " maxValue);");
        out.println("begin");
        out.println("posmod_warning = csp_string.inits(\"implicit non-power-of-2 posmod is deprecated: \");");
        out.println("posmod_warning = csp_string.concat(posmod_warning, csp_string(message, 10));");
        out.println("posmod_warning = csp_string.concat(posmod_warning, csp_string.inits(\" outside [0..\"));");
        out.println("posmod_warning = csp_string.concat(posmod_warning, csp_string(maxValue, 10));");
        out.println("posmod_warning = csp_string.concat(posmod_warning, csp_string.inits(\"]\"));");
        out.println("end");
        out.println("endfunction");

        out.println("endmodule");
        endif(out);
        out.println();
        out.println();
    }

    /**
     * Prints usage message and exits.
     **/
    private static void usage() {
        System.out.print(
            "Usage: cast2verilog\n" +
            "    --cast-path=<CAST path>\n" +
            "    --cell=<cosim-spec>\n" +
            "    [--output-file=<file>] (defaults to <cell>.v)\n" +
            "    [--register-width=<width>] (defaults to 129)\n" +
            "    [--library=<CDL file>] (gate matching for PRS behavior)\n" +
            "    [--ignore-inline] (do not process inline keyword)\n" +
            "    [--enable-system-verilog] (use SystemVerilog features)\n" +
            "    [--ifdef] (surround module definitions with ifdef)\n" +
            "    [--file-list=<file>] (Verilog block dependencies)\n" +
            "    [--behavior-report=<file>] (report CSP and PRS instances)\n" +
            "    [--generate-testbench] (generate simple testbench)\n" +
            "    [--version] (print version information)\n");
        System.exit(1);
    }

    /**
     * Converts CSP to Verilog.  By default, Verilog is written to cell_name.v.
     *
     * @param args  Arguments.  
     **/
    public static void main(String[] args) throws Exception {

        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl(args);
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles(parsedArgs); 
        final CommandLineArgs theArgs = 
            new CachingCommandLineArgs(argsWithConfigs);

        if (theArgs.argExists("version")) {
            System.out.println(
                com.avlsi.util.debug.VersionInfo.getVersionString(
                    Cast2Verilog.class));
        }

        final String logging = theArgs.getArgValue("logging", "SEVERE");
        Level loggingLevel;
        try {
            loggingLevel = Level.parse(logging);
        } catch (IllegalArgumentException e) {
            loggingLevel = Level.OFF;
            System.out.println("Valid levels are OFF, SEVERE, WARNING, INFO," +
                               " CONFIG, FINE, FINER, FINEST, ALL\n" +
                               "\"" + logging + "\" is invalid");
        }
        logger.setLevel(loggingLevel);

        final String castCellName = theArgs.getArgValue("cell", null);
        if (castCellName == null)
            usage();
        final CoSim cosim = CoSim.getCoSim(castCellName, true);
        final String cellName = cosim.getCellType();
        final String envName = cosim.getEnvName();

        final FileSearchPath castFSP =
            new FileSearchPath(theArgs.getArgValue("cast-path", "."));
        final StandardParsingOption spo = new StandardParsingOption(theArgs) {
            public boolean processInline(final CellInterface cell) {
                return !theArgs.argExists("ignore-inline");
            }
        };

        final CastFileParser cfp = new CastFileParser(castFSP, "2", spo);

        final CellInterface resetDriver;
        final String resetDriverFqcn =
            theArgs.getArgValue("reset-driver", null);
        final HierName resetDriverPort = HierName.makeHierName("RESET");
        if (resetDriverFqcn == null) {
            resetDriver = null;
        } else {
            resetDriver = cfp.getFullyQualifiedCellPretty(resetDriverFqcn, 2);
            final CellInterface port = resetDriver.getSubcell(resetDriverPort);
            if (!resetDriver.containsJava() || port == null || !port.isNode()) {
                System.err.println(
                    "To be a valid reset driver, " + resetDriverFqcn +
                    " should have a node output port called " +
                    resetDriverPort + ", and contains a java block");
                System.exit(1);
            }
        }

        final CellInterface cell = cfp.getFullyQualifiedCellPretty(cellName, 2);

        final CoSimParameters params = new CoSimParameters();
        CoSimHelper.setCoSimParams(dutInstance, cell, cosim.getCoSimSpecList(), params,
                                   null, false);

        final HierName instanceName;
        final CellInterface cellEnv;
        if (envName == null) {
            instanceName = HierName.makeHierName(dutInstance);
            cellEnv = cell;
        } else {
            try {
                final CellInterface envCell = cell.getEnvironment(envName);
                CoSimHelper.setCoSimParams(envInstance, envCell,
                             new CoSimSpecList(
                                 new CoSimSpec[] { cosim.getEnvSpec() }),
                             params, null, false);
            } catch (NoSuchEnvironmentException e) {
                System.err.println("Cannot find environment " + envName +
                                   " in " + cellName);
                ExceptionPrettyPrinter.printException(e, System.err);
                System.exit(2);
            }
            instanceName = null;
            final CellImpl impl =
                CellUtils.getEnvWithCell(cell, envName,
                                         cell.getType() + "_" + envName,
                                         envInstance, dutInstance);

            // bring the implied ports up to the cell enclosing the original
            // cell and the environment
            for (Iterator i = cell.getPortSubcellPairs(); i.hasNext(); ){
                final Pair p = (Pair) i.next();
                final HierName inst = (HierName) p.getFirst();
                if (cell.isImpliedPort(inst.getAsString('.'))) {
                    impl.addSubcellPair(inst, (CellInterface) p.getSecond(),
                                        true);
                }
            }

            // create port definitions for the implied ports, and setup
            // connections
            final HierName instance = HierName.makeHierName(dutInstance);
            for (Iterator i = cell.getPortDefinitions(); i.hasNext(); ) {
                final PortDefinition def = (PortDefinition) i.next();
                if (cell.isImpliedPort(def.getName())) {
                    impl.addPortDefinition(def);
                    impl.addImpliedPortMapping(
                        def.getName(),
                        cell.getParentImpliedPort(def.getName()));
                    final Map ports =
                        CellUtils.markPorts(Collections.singletonList(def)
                                                       .iterator());
                    for (Iterator j = ports.entrySet().iterator();
                         j.hasNext(); ) {
                        final Map.Entry entry = (Map.Entry) j.next();
                        final String s = (String) entry.getKey();
                        final HierName port;
                        try {
                            port = HierName.makeHierName(s, '.');
                        } catch (InvalidHierNameException e) {
                            throw new AssertionError("Cannot construct HierName: " + s);
                        }
                        impl.addConnection(port, HierName.append(instance, port));
                    }
                }
            }

            // instantiate a reset driver if requested; it should be a cell
            // with only an output node as port
            if (resetDriver != null) {
                final String resetName = getResetName(cell, null);
                if (resetName != null) {
                    final HierName resetInst =
                        HierName.makeHierName("reset_driver");
                    impl.addSubcellPair(resetInst, resetDriver, false);
                    impl.addConnection(
                            HierName.append(resetInst, resetDriverPort),
                            HierName.append(instance, 
                                HierName.makeHierName(resetName, '.')));

                    // set the instance to the "java" behavior
                    CoSimHelper.setCoSimParams(
                            resetInst.toString(), resetDriver,
                            new CoSimSpecList(
                                new CoSimSpec[] {
                                   new CoSimSpec(
                                       new ModeListLevelSpec(
                                           new ModeList(
                                               new Mode[] {
                                                   Mode.JAVA })),
                                       new InstSpecList(
                                           new InstSpec[0])) }),
                            params, null, false);
                }
            }

            cellEnv = impl;
        }

        final PrintWriter systemErrWriter =
            new PrintWriter(new OutputStreamWriter(System.err));

        final String outputFileName =
            theArgs.getArgValue("output-file", cellEnv.getFullyQualifiedType() + ".v");
        logger.warning("output file is " + outputFileName); // TODO: set to INFO
        final PrintWriter out =
            new PrintWriter(
                new BufferedWriter(
                    new FileWriter(new File(outputFileName))));
        out.println("// " +
                    com.avlsi.util.debug.VersionInfo.getVersionString(
                        Cast2Verilog.class));
        out.println("// " + Calendar.getInstance().getTime());
        out.println("// " + StringUtil.join(args, ' '));

        final String behFileName =
            theArgs.getArgValue("behavior-report", null);
        final PrintWriter behOut =
            new PrintWriter(
                behFileName == null ? NullWriter.getInstance()
                                    : new BufferedWriter(
                                          new FileWriter(behFileName)));

        final String registerWidth =
            theArgs.getArgValue("register-width", "129");
        final boolean enableSystemVerilog =
            theArgs.argExists("enable-system-verilog");
        final boolean generateIfDef = theArgs.argExists("ifdef");
        final boolean generateTb = theArgs.argExists("generate-testbench");
        final Cast2Verilog c2v = new Cast2Verilog(systemErrWriter, 
             systemErrWriter, systemErrWriter, params,
             Integer.parseInt(registerWidth), enableSystemVerilog,
             generateIfDef, generateTb);
        c2v.emitHelperFunctions(out);
        c2v.convert(cellEnv, instanceName, out, behOut, theArgs);
        if (c2v.checkError()) {
            System.err.println("Errors found during translation; output file " +
                               "may be incorrect.");
            System.exit(1);
        }

        final String filelist = theArgs.getArgValue("file-list", null);
        if (filelist != null && !c2v.validVerilogFiles.isEmpty()) {
            final FileWriter w = new FileWriter(filelist);
            for (Iterator i = c2v.validVerilogFiles.iterator(); i.hasNext(); ) {
                w.write((String) i.next() + "\n");
            }
            w.close();
        }

        out.close();
        behOut.close();
    }

    private final CellImpl createWrapperCell(final CellInterface verilog) {
        final String module = "prs2verilog." + verilog.getModuleName();
        final String type = verilog.getType();
        final CellImpl cell =
            new CellImpl(type, module, CellImpl.SYNTHETIC_CELL);

        // We must choose a name that does not conflict with any ports.  _ is
        // the anonymous name, and translated by the CAST parser, so it must
        // not cause a conflict in a synthetic cell.
        final HierName instance = HierName.makeHierName("_");
        cell.addSubcellPair(instance, verilog, false);
        for (Iterator i = verilog.getPortSubcellPairs(); i.hasNext(); ){
            final Pair p = (Pair) i.next();
            cell.addSubcellPair((HierName) p.getFirst(),
                                (CellInterface) p.getSecond(), true);
        }
        cell.setHasCompleteSubcellsBlock();

        // connect ports of the new cell with the ports of the cell containing
        // the verilog block
        final Map ports = CellUtils.markPorts(verilog);
        for (Iterator i = ports.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final String s = (String) entry.getKey();
            final HierName port;
            try {
                port = HierName.makeHierName(s, '.');
            } catch (InvalidHierNameException e) {
                throw new AssertionError("Cannot construct HierName: " + s);
            }
            cell.addConnection(port, HierName.append(instance, port));
        }

        // copy the appropriate port definition for the new cell
        for (Iterator i = verilog.getPortDefinitions(); i.hasNext(); ) {
            final PortDefinition def = (PortDefinition) i.next();
            cell.addPortDefinition(def);
            if (verilog.isImpliedPort(def.getName())) {
                cell.addImpliedPortMapping(
                    def.getName(),
                    verilog.getParentImpliedPort(def.getName()));
            }
        }
        
        return cell;
    }

    /**
     * A cache of wrapper cells.
     **/
    private final Map/*<String,CellInterface>*/ wrapperCache =
        new HashMap/*<String,CellInterface>*/();

    /**
     * Return the wrapper cell for the give cell.
     **/
    private final CellImpl getWrapperCell(final CellInterface verilog) {
        CellImpl cell =
            (CellImpl) wrapperCache.get(verilog.getFullyQualifiedType());
        if (cell == null) {
            cell = createWrapperCell(verilog);
            wrapperCache.put(verilog.getFullyQualifiedType(), cell);
        }

        return cell;
    }

    private final static String getActualName(final Map portLocalMap,
                                              final CellUtils.Channel chan) {
        final CellUtils.Channel local =
            (CellUtils.Channel) portLocalMap.get(chan);
        if (local == null) {
            return null;
        } else {
            final CellUtils.Channel parent = local.getParent();
            if (parent != null && parent.getType() instanceof NodeType) {
                return parent.getFullName() + " [" + local.getIndex() + "]";
            } else {
                return local.getFullName();
            }
        }
    }

    private final String getFormalName(final AliasedMap ports,
                                       final CellUtils.Channel chan) {
        if (ports == null) {
            return chan.getName();
        } else {
            HierName h = null;
            try {
                h = HierName.makeHierName(chan.getName(), '.');
            } catch (InvalidHierNameException e) {
                throw new RuntimeException(e);
            }
            if (((Boolean) ports.getValue(h)).booleanValue()) {
                return ports.getCanonicalKey(h).toString();
            } else {
                return null;
            }
        }
    }

    private final void emitModuleForCellWithSubcells(CellInterface cell, 
            final HierName prefix,
            final String moduleName,
            final Map<HierName,String> subcells, 
            final boolean topLevel,
            final Map<HierName,String> tbNets,
            final Set/*<HierName>*/ narrowInstances,
            PrintWriter out) {

        // write out header for module
        out.println("module \\" + moduleName + " (");
        new EmitFlatPortDeclarations(new Separator(out),
                dir -> (dir > 0 ? "output" : "input"),
                h -> tbNets.containsKey(HierName.makeHierNameUnchecked(h, '.')),
                "",
                narrowInstances.contains(null))
            .mark(cell.getCSPInfo().getPortDefinitions());
        out.println(");");

        final List<String> resets = new ArrayList<>();
        for (Map.Entry<HierName,String> tbNet : tbNets.entrySet()) {
            final String net =
                VerilogUtil.escapeIfNeeded(tbNet.getKey().getAsString('.'));
            if (tbNet.getValue().equals(DirectiveConstants.GROUND_NET)) {
                out.println("supply0 " + net + ";");
            } else if (tbNet.getValue().equals(DirectiveConstants.POWER_NET)) {
                out.println("supply1 " + net + ";");
            } else if (tbNet.getValue().equals(DirectiveConstants.RESET_NET)) {
                out.println("wire " + net + ";");
                resets.add(net);
            }
        }
        if (!resets.isEmpty()) {
            out.println("`CAST2VERILOG_RESET #(.RESETS(" + resets.size() + ")) " +
                        VerilogUtil.escapeIfNeeded("cast2verilog$reset") +
                        "(.reset_n(" +
                        resets.stream().collect(Collectors.joining(", ", "{", "}")) +
                        "));");
        }

        final Map flattenNodes = new HashMap();
        for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            final CellInterface subcell = (CellInterface) p.getSecond();
            if (subcell.isNode() || subcell.isChannel()) continue;

            final HierName instanceName = (HierName) p.getFirst();
            final HierName complete = HierName.append(prefix, instanceName);
            final Mode m = getBehavior(complete);
            if (m instanceof Mode.VerilogMode || m == Mode.PRS) {
                flattenNodes.put(instanceName,
                                 m == Mode.PRS ? Boolean.TRUE : Boolean.FALSE);
            }
        }

        final Set<CellUtils.Channel> nodeNarrowConverters =
            new TreeSet<CellUtils.Channel>();
        final Set<CellUtils.Channel> narrowWideConverters =
            new TreeSet<CellUtils.Channel>();

        // First, process all port connections, identify split src and snk 
        // ports, and create mapping between narrow/wide channels and local 
        // variables/ports. Emit declaration of new local variables as needed
        final Map portToLocalMap =
            processPortConnections(cell, flattenNodes, narrowInstances,
                                   nodeNarrowConverters, narrowWideConverters,
                                   out); 

        // Unconnected wide nodes that have already been declared
        final Set unconnectedWideNodes = new HashSet();

        // Write out subcell instantiations with port lists
        // These will always have wide local names according to above maps
        for (Map.Entry<HierName,String> entry : subcells.entrySet()) {
            final HierName instanceName = entry.getKey();
            final String convertedName = entry.getValue();
            final CellInterface subcell = cell.getSubcell(instanceName);

            if (flattenNodes.get(instanceName) == Boolean.TRUE &&
                !subcell.hasRealProductionRule()) {
                continue;
            }
            final boolean verilog = flattenNodes.containsKey(instanceName);

            final AliasedMap portAliases;
            final CellInterface wrapper;
            if (verilog) {
                wrapper = getWrapperCell(subcell);
                portAliases = getCadencize(true).convert(wrapper).getPortNodes();
            } else {
                wrapper = subcell;
                portAliases = null;
            }

            // calculate the port connection for the subcell module
            // instantiation
            final HashSet aliasedPorts = new HashSet();
            final TreeSet<Triplet<PortTypeInterface,String,List<String>>> actualPorts =
                new TreeSet<>(
                    new Comparator<Triplet<PortTypeInterface,String,List<String>>>() {
                        public int compare(Triplet<PortTypeInterface,String,List<String>> t1,
                                           Triplet<PortTypeInterface,String,List<String>> t2) {
                            return NaturalStringComparator.compareString(
                                        t1.getSecond(), t2.getSecond());
                        }
                    });

            for (Iterator j = CellUtils.getPortChannels(wrapper,
                    getCadencize(true), verilog,
                    narrowInstances.contains(instanceName) ? 1 : 2).iterator();
                 j.hasNext(); ) {
                final CellUtils.Channel subchan = (CellUtils.Channel) j.next();
                final String formal = getFormalName(portAliases, subchan);
                if (formal == null) continue;

                final List<String> actuals = new ArrayList<>();
                final PortTypeInterface subtype = subchan.getType();
                if (portAliases == null && subtype instanceof NodeType &&
                    ((NodeType) subtype).isArrayed()) {
                    for (int k = ((NodeType) subtype).getWidth() - 1; k >= 0; --k)
                    {
                        String name = subchan.getName() + "[" + k + "]";
                        name = StringUtil.replaceSubstring(name, "][", ",");
                        final CellUtils.Channel chan =
                          new CellUtils.Channel(instanceName, name,
                                                subchan.getParent(),
                                                subchan.getType(),
                                                subchan.getIndex(),
                                                subchan.getDirection());
                        String actual = getActualName(portToLocalMap, chan);
                        if (actual == null) {
                            // the node is unconnected; use its name as is,
                            // since it must be unique.
                            actual = subchan.getFullName() + " [" + k + "]";

                            // the node must have not been declared; do so here
                            emitChannelDecl(
                                new CellUtils.Channel(
                                    instanceName, subchan.getName(), subchan,
                                    subchan.getType(), k,
                                    subchan.getDirection()),
                                unconnectedWideNodes, out);
                        }
                        actuals.add(actual);
                    }
                } else {
                    final CellUtils.Channel chan =
                      new CellUtils.Channel(instanceName, subchan.getName(),
                                         subchan.getParent(), subchan.getType(),
                                         subchan.getIndex(),
                                         subchan.getDirection());
                    actuals.add(getActualName(portToLocalMap, chan));
                }

                // only output a connection for the first alias to a port,
                // others are handled earlier in assign statements
                if (portAliases != null && !aliasedPorts.add(formal)) continue;

                actualPorts.add(new Triplet<>(subtype, formal, actuals));
            }

            // emit the subcell module instantiation
            out.println(VerilogUtil.escapeIfNeeded(convertedName) + " " +
                        VerilogUtil.escapeIfNeeded(instanceName.toString()) +
                        " (");
            final Separator sout = new Separator(out);
            for (Triplet<PortTypeInterface,String,List<String>> triple : actualPorts) {
                final PortTypeInterface type = triple.getFirst();
                final String formal = triple.getSecond();
                final List<String> actuals = triple.getThird();
                if (type instanceof NodeType) {
                    emitNodeConnection(cell, instanceName, formal, actuals,
                                       sout);
                } else {
                    assert actuals.size() == 1;
                    getChannelEmitter((ChannelType) type)
                        .emitChannelConnection(formal, actuals.get(0), null, sout);
                }
            }
            out.println(");");
        }

        if (topLevel && !leafDelayBias.isEmpty()) {
            out.println("`ifdef USE_CAST2VERILOG_DELAYBIAS");
            out.println(delaybiasAnnotationName + " CAST2VERILOG_DELAYBIAS();");
            out.println("`endif");
        }

        if (topLevel && !leafExtraDelay.isEmpty()) {
            out.println("`ifdef USE_CAST2VERILOG_EXTRADELAY");
            out.println(extraDelayAnnotationName +
                        " CAST2VERILOG_EXTRADELAY();");
            out.println("`endif");
        }

        final String converterModule = narrowWideConverters.size() > 0 ||
                                       nodeNarrowConverters.size() > 0
              ? VerilogUtil.escapeIfNeeded(moduleName + "$converters")
              : null;

        if (converterModule != null) {
            out.println(converterModule + " cast2verilog$converters();");
        }

        out.println("endmodule        // module with subcells");
        out.println();

        // Emit converter instantiation module if needed
        if (converterModule != null) {
            out.println("module " + converterModule + ";");

            // Wide to narrow converters
            for (final CellUtils.Channel wide : narrowWideConverters) {
                getChannelEmitter((ChannelType) wide.getType())
                    .emitWideConverterInstantiation(
                            wide,
                            portToLocalMap,
                            moduleName,
                            out);
            }

            // Narrow to node converters
            for (final CellUtils.Channel narrow : nodeNarrowConverters) {
                getChannelEmitter((ChannelType) narrow.getType())
                    .emitNarrowConverterInstantiation(narrow,
                            portToLocalMap, moduleName, out);
            }
            out.println("endmodule        // converter module");
        }
        out.println();
    }

    private void addLocalName(final CellUtils.Channel source,
                              final CellUtils.Channel sink,
                              final Map<CellUtils.Channel,CellUtils.Channel> portMap,
                              final Set alreadyDeclared,
                              final Map flattenNodes,
                              final Collection needAssign,
                              final PrintWriter out) {
        final CellUtils.Channel local;
        if (source.getInstance() == null || sink.getInstance() != null) {
            local = source;
        } else {
            local = sink;
        }

        if ((local.getInstance() != null || local.getParent() != null) &&
            alreadyDeclared.add(local)) {
            // XXX: with the port handling below, this may cause some extrenous
            // (but harmless) wires to be declared; to avoid this, declaration
            // should happen after the names have been fixed
            emitChannelDecl(local, alreadyDeclared, out);
        }

        if (!portMap.containsKey(source)) portMap.put(source, local);
        else if (local == sink) {
            final CellUtils.Channel prev = portMap.get(source);
            if (prev.getInstance() == null) {
                needAssign.add(new Pair(prev, local));
            } else {
                portMap.put(source, local);
            }
        }
        if (portMap.containsKey(sink)) {
            final CellUtils.Channel prev = portMap.get(sink);
            if (!prev.equals(local)) {
                final CellUtils.Channel old = simplifyMap(portMap, prev);
                final CellUtils.Channel curr = simplifyMap(portMap, local);
                final CellUtils.Channel from, to;
                if ((old.getInstance() == null) !=
                    (curr.getInstance() == null)) {
                    if (old.getInstance() == null) {
                        from = curr; to = old;
                    } else {
                        from = old; to = curr;
                    }
                } else {
                    if (curr.compareTo(old) < 0) {
                        from = curr; to = old;
                    } else {
                        from = old; to = curr;
                    }
                }
                portMap.put(from, to);
            }
        } else {
            portMap.put(sink, local);
        }
    }

    private static void declareWire(final String name, final int width,
                                    final PrintWriter out) {
        out.println("wire" + (width > 0 ? " [" + (width - 1) + ":0] " : " ") +
                    VerilogUtil.escapeIfNeeded(name) + ";");
    }

    private void emitChannelDecl(final CellUtils.Channel channel,
                                 final Set alreadyDeclared,
                                 final PrintWriter out) {
        final PortTypeInterface type = channel.getType();
        final String fullName = channel.getFullName();
        if (type instanceof NodeType) {
            final CellUtils.Channel parent = channel.getParent();
            if (parent != null && parent.getType() instanceof NodeType) {
                if (channel.getInstance() != null &&
                    alreadyDeclared.add(parent))
                    declareWire(parent.getFullName(),
                                ((NodeType) parent.getType()).getWidth(), out);
            } else {
                declareWire(fullName, 0, out);
            }
        } else {
            final ChannelType chan = (ChannelType) type;
            getChannelEmitter(chan).emitChannelDecl(fullName, out);
        }
    }

    private void declConverterVars(final Collection converters,
                                   final Set alreadyDeclared,
                                   final Map flattenNodes,
                                   final Map<CellUtils.Channel,CellUtils.Channel> portMap,
                                   final PrintWriter out) {
        for (Iterator i = converters.iterator(); i.hasNext();) { 
            final CellUtils.Channel parent = (CellUtils.Channel) i.next();
            if (!portMap.containsKey(parent)) {
                addLocalName(parent, parent, portMap, alreadyDeclared,
                             flattenNodes, null, out);
            }
            for (Iterator j = parent.getChildren().iterator(); j.hasNext(); ) {
                final CellUtils.Channel child = (CellUtils.Channel) j.next();
                if (!portMap.containsKey(child)) {
                    addLocalName(child, child, portMap, alreadyDeclared,
                                 flattenNodes, null, out);
                }
            }
        }
    }

    private static boolean wideNodeChild(final CellUtils.Channel channel) {
        return channel.getParent() != null &&
               channel.getParent().getType() instanceof NodeType &&
               channel.getParent().getParent() == null;
    }

    /**
     * Return the string representation for a possibly wide node typed channel.
     * The returned string will be properly escaped.
     **/
    private static String nodeVerilogName(final CellUtils.Channel channel) {
        assert channel.getType() instanceof NodeType;

        if (wideNodeChild(channel)) {
            final CellUtils.Channel parent = channel.getParent();
            return VerilogUtil.escapeIfNeeded(parent.getFullName()) +
                   "[" + channel.getIndex() + "]";
        } else {
            return VerilogUtil.escapeIfNeeded(channel.getFullName());
        }
    }

    private CellUtils.Channel simplifyMap(
            final Map<CellUtils.Channel,CellUtils.Channel> portMap,
            final CellUtils.Channel key) {
        final CellUtils.Channel value = portMap.get(key);
        if (value == null || value == key) {
            return key;
        } else {
            final CellUtils.Channel last = simplifyMap(portMap, value);
            portMap.put(key, last);
            return last;
        }
    }

    private Map<CellUtils.Channel,CellUtils.Channel> processPortConnections(
            final CellInterface cell,
            final Map flattenNodes,
            final Set narrowInstances,
            final Set<CellUtils.Channel> nodeNarrowConverters,
            final Set<CellUtils.Channel> narrowWideConverters,
            final PrintWriter out) {
        final Map<CellUtils.Channel,CellUtils.Channel> portMap = new HashMap<>();

        final Collection nodeConnections = new ArrayList();
        final Collection narrowConnections = new ArrayList();
        final Collection wideConnections = new ArrayList();

        CellUtils.getChannelConnection(cell,
                nodeConnections, narrowConnections, wideConnections,
                new AliasedSet(), new AliasedSet(), new AliasedSet(),
                nodeNarrowConverters, narrowWideConverters, flattenNodes,
                false, false, getCadencize(false), getCadencize(true));

        // convert a wide connection to narrow connections if either the source
        // or the sink instance is supposed to be narrow
        for (Iterator i = wideConnections.iterator(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            final CellUtils.Channel source = (CellUtils.Channel) p.getFirst();
            final CellUtils.Channel sink = (CellUtils.Channel) p.getSecond();
            if (narrowInstances.contains(source.getInstance()) ||
                narrowInstances.contains(sink.getInstance())) {
                for (Iterator srcIter = source.getChildren().iterator(),
                              snkIter = sink.getChildren().iterator();
                     srcIter.hasNext() && snkIter.hasNext(); ) {
                    narrowConnections.add(new Pair(srcIter.next(),
                                                   snkIter.next()));
                }

                // add additional converters that might be necessary because
                // one end of a connection is wide, but the other end is
                // narrow.
                if (!narrowInstances.contains(source.getInstance()))
                    narrowWideConverters.add(source);
                if (!narrowInstances.contains(sink.getInstance()))
                    narrowWideConverters.add(sink);

                i.remove();
            }
        }

        // remove narrow to wide converters for narrow instances as they are
        // not necessary
        for (Iterator i = narrowWideConverters.iterator(); i.hasNext(); ) {
            final CellUtils.Channel chan = (CellUtils.Channel) i.next();
            if (narrowInstances.contains(chan.getInstance())) i.remove();
        }

        final Set alreadyDeclared = new HashSet();

        // Signals that need to be connected via assign statements
        final Collection needAssign = new HashSet();

        for (Iterator i =
                new FlatteningIterator(
                    Arrays.asList(
                        new Iterator[] {
                            nodeConnections.iterator(),
                            narrowConnections.iterator(),
                            wideConnections.iterator() }).iterator());
                i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            final CellUtils.Channel source = (CellUtils.Channel) p.getFirst();
            final CellUtils.Channel sink = (CellUtils.Channel) p.getSecond();
            addLocalName(source, sink, portMap, alreadyDeclared, flattenNodes,
                         needAssign, out);
            if (source.getInstance() == null && sink.getInstance() == null)
                needAssign.add(p);
        }

        // Make sure all variables used in instantiating converters have
        // appropriate mapping in portMap
        declConverterVars(narrowWideConverters, alreadyDeclared, flattenNodes,
                          portMap, out);
        declConverterVars(nodeNarrowConverters, alreadyDeclared, flattenNodes,
                          portMap, out);

        // Compute the transitive closure of the name mapping to find the
        // ultimate name to use
        for (Map.Entry<CellUtils.Channel,CellUtils.Channel> entry :
                portMap.entrySet()) {
            entry.setValue(simplifyMap(portMap, entry.getValue()));
        }

        for (Iterator i = needAssign.iterator(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            final CellUtils.Channel source = (CellUtils.Channel) p.getFirst();
            final CellUtils.Channel sink = (CellUtils.Channel) p.getSecond();
            if (source.getType() instanceof NodeType) {
                final String sourceName = nodeVerilogName(source);
                final String sinkName = nodeVerilogName(sink);
                out.println("assign " + sinkName + " = " + sourceName + ";");
            } else {
                getChannelEmitter((ChannelType) source.getType()).
                    emitPassThru(out, source.getFullName(), sink.getFullName());
            }
        }

        return portMap;
    }

    private int getBase(ChannelType chanType) {
        return CellUtils.extractN(chanType.getTypeName());
    }

    /***
       Find the number of bits we need to model each narrow channel element 
       of a possibly wide channel. 
    ***/
    private static int getNumBitsNarrow(ChannelType chanType) {
        final BigInteger numValues = chanType.getNumValues();
        final int bitWidth = BigIntegerUtil.log2(numValues);
        return bitWidth;
    }

    /***
       Find the number of bits we need model the data in possibly wide channels
     **/
    private static int getNumBitsWide(ChannelType chanType) {
        final BigInteger narrowNumValues = chanType.getNumValues();
        final int width = chanType.getWidth();
        final BigInteger numValues = narrowNumValues.pow(width);
        final int bitWidth = BigIntegerUtil.log2(numValues);
        return bitWidth;
    }

    private static String hierRef(final String hier, final String local) {
        return hier == null ? local : esc(hier) + "." + local;
    }

    private void emitNodeConnection(final CellInterface parent,
                                    final HierName instance,
                                    final String formal,
                                    final List<String> actuals,
                                    final Separator sout) {
        final String preamble = "  ." + esc(formal) + "(";
        final String actual;
        if (actuals.size() == 1) {
            actual = actuals.stream()
                            .map(act -> act == null ? "/*NC*/" : esc(act))
                            .findFirst()
                            .get();
        } else {
            final String spaces =
                ",\n" + StringUtil.repeatString(" ", preamble.length() + 1);
            actual = actuals.stream()
                            .map(act -> act == null ? "1'dx" : esc(act))
                            .collect(Collectors.joining(spaces, "{", "}"));
        }
        sout.print(preamble + actual + ")");

        boolean unconnected = actuals.stream().anyMatch(x -> x == null);
        if (unconnected) {
            logger.warning("In " + parent.getFullyQualifiedType() +
                           ", port " + formal + " of instance " +
                           instance + " is unconnected.");
        }
    }

    /**
     * Convert DSim units to number of transitions.
     **/
    private static float numTransitions(final float dsimUnits) {
        return dsimUnits / 100f;
    }

    private static String getArrayDecl(List<ArrayType> arrays) {
        final int size = arrays.size();
        if (size == 0) {
            return "";
        } else {
            return arrays.stream()
                         .map(ary -> ary.getMaxIndex() + ":" +
                                     ary.getMinIndex())
                         .collect(Collectors.joining("][", "[", "]"));
        }
    }

    private static int getArrayElements(List<ArrayType> arrays) {
        return arrays.stream()
                     .mapToInt(ary -> ary.getMaxIndex() - ary.getMinIndex() + 1)
                     .reduce(1, (a, b) -> a * b);
    }

    private class IgnoreArrayPorts extends MarkPort {
        protected final List<ArrayType> arrays = new ArrayList<>();
        protected void mark(final ArrayType arrayType, final String name,
                            final int direction, final boolean inArray) {
            PortTypeInterface baseType = arrayType;
            while (baseType instanceof ArrayType) {
                final ArrayType aType = (ArrayType) baseType;
                arrays.add(aType);
                baseType = aType.getArrayedType();
            }
            mark(baseType, name, direction, false);
        }
    }

    private class EmitPortDeclarations extends IgnoreArrayPorts {
        private final Separator out;
        private final IntFunction<String> netTypeFunction;
        private final String suffix;
        public EmitPortDeclarations(final Separator out,
                                    final IntFunction<String> netTypeFunction,
                                    final String suffix) {
            this.out = out;
            this.netTypeFunction = netTypeFunction;
            this.suffix = suffix;
        }
        protected void mark(final ChannelType channelType, final String name,
                            final int direction, final boolean inArray) {
            getChannelEmitter(channelType)
                .emitPortDeclaration(name, arrays, direction, netTypeFunction,
                                     suffix, out);
            arrays.clear();
        }
        protected void mark(final NodeType nodeType, final String name,
                            final int direction, final boolean inArray) {
            final String width;
            if (nodeType.isArrayed()) {
                width = "[" + (nodeType.getWidth() - 1) + ":0] ";
            } else {
                width = "";
            }
            out.print(netTypeFunction.apply(direction) + " " + width +
                      VerilogUtil.escapeIfNeeded(name) +
                      getArrayDecl(arrays));
            arrays.clear();
        }
    }

    private class PossiblyNarrowMarkPort extends MarkPort {
        private final boolean narrowPorts;
        public PossiblyNarrowMarkPort(final boolean narrowPorts) {
            this.narrowPorts = narrowPorts;
        }
        protected void mark(final ChannelType channelType, final String name,
                            final int direction, final boolean inArray) {
            final boolean isArray = channelType.isArrayed() && narrowPorts;
            final String newName =
                arrayName(channelType, name, inArray, isArray);
            if (isArray) {
                mark(channelType, newName, direction, channelType.getWidth());
            } else {
                mark(channelType, newName, direction);
            }
        }
    }

    private class EmitFlatPortDeclarations extends PossiblyNarrowMarkPort {
        private final Separator out;
        private final IntFunction<String> netTypeFunction;
        private final String suffix;
        private final Predicate<String> omit;
        public EmitFlatPortDeclarations(final Separator out,
                                        final IntFunction<String> netTypeFunction,
                                        final String suffix,
                                        final boolean narrowPorts) {
            this(out, netTypeFunction, x -> false, suffix, narrowPorts);
        }
        public EmitFlatPortDeclarations(final Separator out,
                                        final IntFunction<String> netTypeFunction,
                                        final Predicate<String> omit,
                                        final String suffix,
                                        final boolean narrowPorts) {
            super(narrowPorts);
            this.out = out;
            this.netTypeFunction = netTypeFunction;
            this.omit = omit;
            this.suffix = suffix;
        }
        protected void mark(final ChannelType channelType, final String name,
                            final int direction) {
            getChannelEmitter(channelType)
                .emitPortDeclaration(name, Collections.emptyList(), direction,
                                     netTypeFunction, suffix, out);
        }
        protected void mark(final NodeType nodeType, final String name,
                            final int direction, final boolean inArray) {
            final String width;
            if (nodeType.isArrayed()) {
                width = "[" + (nodeType.getWidth() - 1) + ":0] ";
            } else {
                width = "";
            }
            final String newName = arrayName(nodeType, name, inArray, false);
            if (!omit.test(newName)) {
                out.print(netTypeFunction.apply(direction) + " " + width +
                          VerilogUtil.escapeIfNeeded(newName));
            }
        }
    }

    private class EmitNodeArrayAssign extends MarkPort {
        private final PrintWriter out;
        public EmitNodeArrayAssign(final PrintWriter out) {
            this.out = out;
        }
        protected void mark(final ChannelType channelType, final String name,
                            final int direction) {
        }
        protected void mark(final NodeType nodeType, final String name,
                            final int direction, final boolean inArray) {
            if (inArray) {
                final String newName = arrayName(nodeType, name, inArray, false);
                final Pair<String,String> parts = mungeArray(newName);
                final String indexed =
                    VerilogUtil.escapeIfNeeded(parts.getFirst()) +
                    parts.getSecond();
                final String nonindexed = VerilogUtil.escapeIfNeeded(newName);

                final String lhs, rhs;
                if (direction > 0) {
                    lhs = nonindexed;
                    rhs = indexed;
                } else {
                    lhs = indexed;
                    rhs = nonindexed;
                }

                out.println("assign " + lhs + " = " + rhs + ";");
            }
        }
    }

    private class EmitBodyInstantiation extends IgnoreArrayPorts {
        private final Separator out;
        private final String actualSuffix;
        public EmitBodyInstantiation(final Separator out,
                                     final String actualSuffix) {
            this.out = out;
            this.actualSuffix = actualSuffix;
        }
        protected void mark(final ChannelType channelType, final String name,
                            final int direction, final boolean inArray) {
            getChannelEmitter(channelType)
                .emitBodyInstantiation(name, actualSuffix, out);
            arrays.clear();
        }
        protected void mark(final NodeType nodeType, final String name,
                            final int direction, final boolean inArray) {
            out.print("." + VerilogUtil.escapeIfNeeded(name) + "(" +
                      VerilogUtil.escapeIfNeeded(name) + ")");
            arrays.clear();
        }
    }

    private class EmitPortInitializers extends IgnoreArrayPorts {
        private final PrintWriter out;
        public EmitPortInitializers(final PrintWriter out) {
            this.out = out;
        }
        protected void mark(final ChannelType channelType, final String name,
                            final int direction, final boolean inArray) {
            getChannelEmitter(channelType)
                .emitPortInitializer(name, arrays, direction, out);
            arrays.clear();
        }
        protected void mark(final NodeType nodeType, final String name,
                            final int direction, final boolean inArray) {
            arrays.clear();
        }
    }

    private class CollectInputPorts extends IgnoreArrayPorts {
        private final List<String> inputPorts;
        public CollectInputPorts(List<String> inputPorts) {
            this.inputPorts = inputPorts;
        }
        protected void mark(final ChannelType channelType, final String name,
                            final int direction, final boolean inArray) {
            getChannelEmitter(channelType)
                .getInputPorts(name, arrays, direction, inputPorts);
            arrays.clear();
        }
        protected void mark(final NodeType nodeType, final String name,
                            final int direction, final boolean inArray) {
            inputPorts.add(VerilogUtil.escapeIfNeeded(name));
            arrays.clear();
        }
    }

    private class EmitSlackWrappers extends PossiblyNarrowMarkPort {
        private final PrintWriter out;
        private final String actualSuffix;
        private final String reset;
        private final Function<String,ChannelTimingInfo> ctiFunc;
        public EmitSlackWrappers(final PrintWriter out, final String actualSuffix,
                                 final String reset,
                                 final Function<String,ChannelTimingInfo> ctiFunc) {
            super(false);
            this.out = out;
            this.actualSuffix = actualSuffix;
            this.reset = reset;
            this.ctiFunc = ctiFunc;
        }
        protected void mark(final ChannelType channelType, final String name,
                            final int direction) {
            getChannelEmitter(channelType)
                .emitSlackWrapper(name, direction, actualSuffix, ctiFunc,
                                  reset, out);
        }
    }

    /**
     * Emit a cell to be implemented in CSP.
     **/
    private void emitCspLeaf(final CellInterface cell,
                             final String moduleName,
                             final PrintWriter out,
                             final boolean emitSlackWrappers)
        throws SemanticException {

        final String bodyName = moduleName + (emitSlackWrappers ? "$body" : "");
        out.println("module " + VerilogUtil.escapeIfNeeded(bodyName) + "(");

        final Separator sout = new Separator(out);
        walkPortList(
                cell.getCSPInfo(), false,
                new EmitPortDeclarations(
                    sout, dir -> (dir > 0 ? "output reg" : "input"), ""));
        out.println(");");
        emitTimeScale(out);

        final String resetName = getResetName(cell);

        out.println("string " + moduleInstanceName +
                    (emitSlackWrappers ? "" : " = $psprintf(\"%m\")") +
                    ";");

        // initialize port variables
        out.println("always @(negedge " + resetName + " )");
        out.println("begin : init_ports");
        out.println("disable main;");
        walkPortList(cell.getCSPInfo(), false, new EmitPortInitializers(out));
        out.println("end");
        out.println();

        if (probFilter == null)
            probFilter = Csp2Verilog.getProblemFilter(warningWriter);

        final ArrayList<String> inputPorts = new ArrayList<>();
        walkPortList(cell.getCSPInfo(), false, new CollectInputPorts(inputPorts));
        new Csp2Verilog(warningWriter, errorWriter, debugWriter, resetName,
                        registerBitWidth, probFilter, enableSystemVerilog)
        .convert(cell, bodyName, inputPorts, out);

        out.println("endmodule     // CSP body ");
        out.println();
        out.println();

        if (emitSlackWrappers)
            emitSlackWrapper(cell, moduleName, bodyName, BlockInterface.CSP,
                             out);
    }

    private void emitPassThru(final PortDefinition l, final PortDefinition r,
                              final PrintWriter out) {
        final Collection<PortDefinition> lports =
            new ArrayList<PortDefinition>();
        new CellUtils.FlattenPortDefinitions(lports).mark(
                Collections.singleton(l).iterator());

        final Collection<PortDefinition> rports =
            new ArrayList<PortDefinition>();
        new CellUtils.FlattenPortDefinitions(rports).mark(
                Collections.singleton(r).iterator());

        assert lports.size() == rports.size() :
            "Left and right ports not compatible: " + l + " " + r;

        final Iterator<PortDefinition> liter = lports.iterator();
        final Iterator<PortDefinition> riter = rports.iterator();
        while (liter.hasNext() && riter.hasNext()) {
            final PortDefinition lport = liter.next();
            final PortDefinition rport = riter.next();
            final String input, output;
            if (lport.getDirection() == PortDefinition.IN) {
                input = lport.getName();
                output = rport.getName();
            } else {
                input = rport.getName();
                output = lport.getName();
            }

            if (lport.getType() instanceof ChannelType &&
                rport.getType() instanceof ChannelType) {
                getChannelEmitter((ChannelType) lport.getType()).
                    emitPassThru(out, input, output);
            } else {
                out.println("assign \\" + output + " = \\" + input + " ;");
            }
        }
    }

    /**
     * Emits wrapper that includes all timing buffers. $body is surrounded 
     * by timing buffers, 1 per (possibly wide) channel 
     **/
    private void emitSlackWrapper(final CellInterface cell, 
            final String moduleName,
            final String bodyName,
            final String block,
            PrintWriter out) {

        // emit module name and port list declaration
        out.println("module " + VerilogUtil.escapeIfNeeded(moduleName) + "(");
        new EmitFlatPortDeclarations(new Separator(out),
                dir -> (dir > 0 ? "output" : "input"), "", false)
            .mark(cell.getCSPInfo().getPortDefinitions());
        out.println(");");

        // Collect channels require special handling
        final Collection<PortDefinition> inSramSerial =
            new ArrayList<PortDefinition>();
        final Collection<PortDefinition> outSramSerial =
            new ArrayList<PortDefinition>();
        final Collection<PortDefinition> inChanDft =
            new ArrayList<PortDefinition>();
        final Collection<PortDefinition> outChanDft =
            new ArrayList<PortDefinition>();

        /* Iterate over flattened possibly wide channels */
        Iterator portDefs = cell.getCSPInfo().getPortDefinitions();
        final Collection partialFlatPorts = new ArrayList();
        (new CellUtils.FlattenPortDefinitions(partialFlatPorts) {
            protected void mark(final StructureType structureType,
                                final String name, final int direction) {
                final String tag = structureType.getTag();
                final int dir = translate(direction);
                if (CellUtils.isDftChannel(cell, tag)) {
                    (dir == PortDefinition.IN ? inChanDft : outChanDft)
                        .add(new PortDefinition(name, structureType, dir));
                } else if (CellUtils.isSramSerialChannel(tag)) {
                    (dir == PortDefinition.IN ? inSramSerial : outSramSerial)
                        .add(new PortDefinition(name, structureType, dir));
                } else {
                    super.mark(structureType, name, direction);
                }
            }   
        }).mark(portDefs);

        final String bodyInst = "body";
        final String bundleSuffix = "_wrap";
        walkPortList(
                cell.getCSPInfo(), false,
                new EmitPortDeclarations(new Separator(out, ";\n"),
                                         dir -> "wire",
                                         bundleSuffix));
        out.println(";");

        new EmitNodeArrayAssign(out)
            .mark(cell.getCSPInfo().getPortDefinitions());

        out.println("initial " + VerilogUtil.escapeIfNeeded(bodyInst) + "." + 
                    moduleInstanceName + " = $psprintf(\"%m\");");

        new EmitSlackWrappers(out, bundleSuffix, getResetName(cell),
                x -> DirectiveUtils.getTiming(cell, block, x, 0))
            .mark(cell.getCSPInfo().getPortDefinitions());

        // Handle special case for ChanDft
        final Iterator<PortDefinition> chanDftIn = inChanDft.iterator();
        final Iterator<PortDefinition> chanDftOut = outChanDft.iterator();
        while (chanDftIn.hasNext() && chanDftOut.hasNext()) {
            emitPassThru(chanDftIn.next(), chanDftOut.next(), out);
        }

        if (chanDftIn.hasNext() != chanDftOut.hasNext()) {
            logger.severe("In " + cell.getFullyQualifiedType() + ", " +
                          "ChanDft ports do not appear in pairs");
        }

        // Handle special case for SramSerialChannel
        final Iterator<PortDefinition> sramSerialIn = inSramSerial.iterator();
        final Iterator<PortDefinition> sramSerialOut = outSramSerial.iterator();
        while (sramSerialIn.hasNext() && sramSerialOut.hasNext()) {
            emitPassThru(sramSerialIn.next(), sramSerialOut.next(), out);
        }

        if (sramSerialIn.hasNext() != sramSerialOut.hasNext()) {
            logger.severe("In " + cell.getFullyQualifiedType() + ", " +
                          "SramSerialChannel ports do not appear in pairs");
        }
       
        // instance name = "body" for now
        out.println(VerilogUtil.escapeIfNeeded(bodyName) + " " +
                    VerilogUtil.escapeIfNeeded(bodyInst) + "(");
        walkPortList(
                cell.getCSPInfo(), false,
                new EmitBodyInstantiation(new Separator(out), bundleSuffix));
        out.println(");");

        out.println("endmodule       // slack wrapper");
        out.println("");
    }

    /**
     * Return the reset node for a cell.  The reset node is an implied node
     * that connects to <code>_RESET</code> in the parent.  Mike Davies
     * promises this condition is satisfied for any cell that needs to be
     * implemented in CSP.
     *
     * @param cell cell to process
     * @return name of the reset node, escaped if necessary
     **/
    private static String getResetName(final CellInterface cell) {
        final String name = getResetName(cell, null);
        if (name == null) {
            throw new RuntimeException("Cannot find the reset node in " +
                                       cell.getFullyQualifiedType());
        } else {
            return name;
        }
    }

    /**
     * Return the reset node for a cell.  The reset node is an implied node
     * that connects to <code>_RESET</code> in the parent.  If that node does
     * not exists and <code>portMap</code> is not null, look for a node called
     * <code>_RESET</code> in the current cell, and if it exists, return the
     * proper name associated with that node.
     *
     * @param cell cell to process
     * @param portMap mapping from source and sinks to local names generated by
     * {@link processPortConnections}
     * @return name of the reset node, escaped if necessary
     **/
    private static String getResetName(final CellInterface cell,
                                       final Map portMap) {
        boolean found = false;
        for (Iterator i = cell.getPortDefinitions(); i.hasNext(); ) {
            final PortDefinition port = (PortDefinition) i.next();
            if (resetNodeName.equals(cell.getParentImpliedPort(port.getName())))
            {
                return VerilogUtil.escapeIfNeeded(port.getName());
            }
            if (resetNodeName.equals(port.getName())) found = true;
        }

        if (portMap == null) {
            if (found) return VerilogUtil.escapeIfNeeded(resetNodeName);
        } else {
            // _RESET may not be in the map directly, because the map only
            // contains sources and sinks, but one of its aliases should be in
            // the map, if _RESET is in fact connected correctly
            final HierName hierReset = HierName.makeHierName(resetNodeName);
            final Iterator i = cell.getConnectedNodes(hierReset);
            if (i != null) {
                while (i.hasNext()) {
                    final String alias = ((HierName) i.next()).getAsString('.');
                    final CellUtils.Channel candidate =
                        new CellUtils.Channel(null, alias, null, new NodeType(),
                                              -1, -1);
                    final CellUtils.Channel reset =
                        (CellUtils.Channel) portMap.get(candidate);
                    if (reset != null)
                        return nodeVerilogName(reset);
                }
            }
        }

        return null;
    }

    private void annotateDelaybias(final PrintWriter out,
                                   final String topModule) {
        // don't output anything if there are no instance delaybiases
        if (leafDelayBias.isEmpty()) return;

        final String esc = VerilogUtil.escapeIfNeeded(topModule);
        out.println("`ifdef USE_CAST2VERILOG_DELAYBIAS");
        out.println("module " + delaybiasAnnotationName + ";");
        out.println("defparam");
        for (Iterator i = leafDelayBias.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            out.println("    " + esc + "." + entry.getKey() + "." +
                        VerilogUtil.escapeIfNeeded(
                            ConverterConstants.getDelayBiasString())
                        + " = " + entry.getValue() + (i.hasNext() ? "," : ";"));
        }
        out.println("endmodule");
        out.println("`endif");
    }

    private void annotateExtraDelay(final String prefix,
                                    final HierName net,
                                    final boolean up,
                                    final float extraDelay,
                                    final PrintWriter out,
                                    final boolean[] first) {
        if (!first[0]) out.println(",");
        out.print(prefix +
                  VerilogUtil.escapeIfNeeded(
                      ConverterConstants.getExtraDelayString(
                          net, up)) + " = " +
                  numTransitions(extraDelay));
        first[0] = false;
    }

    private void annotateExtraDelay(final PrintWriter out,
                                    final String topModule) {
        // don't output anything if there are no extra delays
        if (leafExtraDelay.isEmpty()) return;

        final String esc = VerilogUtil.escapeIfNeeded(topModule);
        out.println("`ifdef USE_CAST2VERILOG_EXTRADELAY");
        out.println("module " + extraDelayAnnotationName + ";");
        out.println("defparam");
        final boolean[] first = new boolean[] { true };
        for (Iterator i = leafExtraDelay.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final String partialLhs = "    " + esc + "." + entry.getKey() + ".";
            for (Iterator j = ((Map) entry.getValue()).entrySet().iterator();
                 j.hasNext(); ) {
                final Map.Entry netValue = (Map.Entry) j.next();
                final HierName net = (HierName) netValue.getKey();
                final float[] extraDelay = (float[]) netValue.getValue();
                if (extraDelay[0] != 0)
                    annotateExtraDelay(partialLhs, net, true, extraDelay[0],
                                       out, first);
                if (extraDelay[1] != 0)
                    annotateExtraDelay(partialLhs, net, false, extraDelay[1],
                                       out, first);
            }
        }
        if (!first[0]) out.println(";");
        out.println("endmodule");
        out.println("`endif");
    }

    private void emitTimeScale(final PrintWriter out) {
        out.println("`" + ConverterConstants.getTimeScaleMacroString());
    }
}
