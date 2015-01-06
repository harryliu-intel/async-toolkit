/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.synthesis;

import java.util.*;
import java.text.DateFormat;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;

import com.avlsi.util.container.MultiSet;
import com.avlsi.util.cmdlineargs.CommandLineArg;

import com.avlsi.util.container.SortingIterator;
import com.avlsi.cast.CastCacheManager;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cast2.util.StandardParsingOption;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.impl.CastEmitter;
import com.avlsi.cast2.directive.impl.DirectiveEmitter;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.fast.ports.ChannelType;
import com.avlsi.fast.ports.NodeType;
import com.avlsi.fast.ports.StructureType;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.io.FileSearchPath;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.dsim.ExceptionPrettyPrinter;
import com.avlsi.tools.slacker.Cast;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.PedanticCommandLineArgs;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.text.NaturalStringComparator;
import com.avlsi.util.text.NumberFormatter;
import com.avlsi.util.text.StringUtil;
import com.avlsi.util.container.Pair;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CadenceNameInterface;
import com.avlsi.file.cdl.util.rename.GDS2NameInterface;
import com.avlsi.file.cdl.util.rename.IdentityNameInterface;
import java.util.Comparator;
import java.lang.Float;

/** for generating Liberty ".lib" files **/
public class GenerateLib {

    /** number of inputs **/
    final int Inputs;

    /** task name: image, dual and task **/
    final String Task;

    /** location of timing files **/
    final String timingDir;

    /** module name **/
    final String module;

    /** prefixes for variants of gates **/
    final String [] mldPrefixes = new String [] {"","V_"};
    final String [] qdiPrefixes = new String [] {"","C_"};
    final String [] dominoPrefixes = new String [] {"","C_"};

    /** use virtual clocks for real lib? **/
    final boolean virtualClocks;

    /** CAST and name translation options **/
    CastFileParser castParser;
    CDLNameInterface nameInterface;
    final boolean macroLib; // characterize a custom async cell

    int CellCount = 0;

    /** default PVT applies to hard-macro, Proteus sets these from timing file **/
    String voltage = "0.81";
    String temperature = "0";
    String process_name = "ss";

    String translate = "cast";

    boolean noheader;
    boolean nofooter;
    final String header_template =
      "/*\n"+
      " * Copyright (c) COPYRIGHT_YEAR Fulcrum Microsystems, Inc. All Rights Reserved.\n"+
      " *\n"+
      " * CONFIDENTIAL AND PROPRIETARY DATA OF Fulcrum Microsystems, Inc.\n"+
      " *\n"+
      " * This file contains valuable trade secrets and proprietary information\n"+
      " * of Fulcrum Microsystems, Inc., and is protected by U.S. and\n"+
      " * international laws and/or treaties.\n"+
      " *\n"+
      " * The copyright notice(s) in this file does not indicate actual or intended\n"+
      " * publication of this file.\n"+
      " *\n"+
      " * created DATE_CREATED\n"+
      " *\n"+
      " * process (PROCESS_NAME), voltage (VOLTAGE), temp (TEMPERATURE)\n"+
      " *\n"+
      " * corner: VOLTAGE_TEMPERATURE_PROCESS_NAME\n"+
      " */\n"+
      "library(LIBRARY_NAME) {\n"+
      "\n"+
      "  /* general attributes */\n"+
      "\n"+
      "  delay_model : table_lookup;\n"+
      "  in_place_swap_mode : match_footprint;\n"+
      "  library_features(report_delay_calculation);\n"+
      "  bus_naming_style : \"%s_______%d\";\n"+
      "\n"+
      "  /* documentation attributes */\n"+
      "\n"+
      "  revision : 1.0;\n"+
      "  date : \"DATE_CREATED\";\n"+
      "  comment : \"Copyright (c) COPYRIGHT_YEAR Fulcrum Microsystems, Inc. All Rights Reserved.\"\n"+
      "\n"+
      "  /* unit attributes */\n"+
      "\n"+
      "  time_unit : \"1ns\";\n"+
      "  voltage_unit : \"1V\";\n"+
      "  current_unit : \"1mA\";\n"+
      "  capacitive_load_unit (1,pf);\n"+
      "  pulling_resistance_unit : \"1kohm\";\n"+
      "  leakage_power_unit : \"1pW\";\n"+
      "\n"+
      "  /* operation conditions */\n"+
      "\n"+
      "  nom_process     : 1;\n"+
      "  nom_temperature : TEMPERATURE;\n"+
      "  nom_voltage     : VOLTAGE;\n"+
      "  operating_conditions(typical) {\n"+
      "    process     : 1;\n"+
      "    temperature : TEMPERATURE;\n"+
      "    voltage     : VOLTAGE;\n"+
      "    tree_type   : balanced_tree\n"+
      "  }\n"+
      "  default_operating_conditions : typical;\n"+
      "\n"+
      "  /* threshold definitions */\n"+
      "\n"+
      "  slew_lower_threshold_pct_fall : 33.3;\n"+
      "  slew_upper_threshold_pct_fall : 66.7;\n"+
      "  slew_lower_threshold_pct_rise : 33.3;\n"+
      "  slew_upper_threshold_pct_rise : 66.7;\n"+
      "  input_threshold_pct_fall      : 50.0;\n"+
      "  input_threshold_pct_rise      : 50.0;\n"+
      "  output_threshold_pct_fall     : 50.0;\n"+
      "  output_threshold_pct_rise     : 50.0;\n"+
      "\n"+
      "  /* default attributes */\n"+
      "\n"+
      "  default_leakage_power_density : 0.0;\n"+
      "  slew_derate_from_library      : 1.0;\n"+
      "  default_cell_leakage_power    : 0.0;\n"+
      "  default_fanout_load           : 1.0;\n"+
      "  default_output_pin_cap        : 0.0;\n"+
      "  default_inout_pin_cap         : 0.0;\n"+
      "  default_input_pin_cap         : 0.0;\n"+
      "  default_max_transition        : 0.1;\n"+
      "  default_max_capacitance       : 0.25;\n"+
      "  default_max_fanout            : 64.0;\n";

    private class FloatComparator implements Comparator {
        public int compare(Object o1, Object o2) {
            Float a = new Float((String) o1);
            Float b = new Float((String) o2);
            if (a - b > 0.0)
                return 1;
            if (a -b < 0.0)
                return -1;
            return 0;
        }
    }
    
    /** print lib file header **/
    private void emitHeader(CellTiming timing, String castName) {
        if (noheader) return;
        Date today = new Date();
        String date_created = today.toString();
        final String[] datesplit = date_created.split(" ");
        String copyright_year = datesplit[datesplit.length-1];
        String header = header_template.replaceAll("VOLTAGE",voltage);
        String library_name = "\"" + castName + "\"";
        header = header.replaceAll("TEMPERATURE",temperature);
        header = header.replaceAll("DATE_CREATED", date_created);
        header = header.replaceAll("COPYRIGHT_YEAR", copyright_year);
        header = header.replaceAll("PROCESS_NAME", process_name);
        header = header.replaceAll("LIBRARY_NAME", library_name);
        System.out.println(header);
        final Map templates = timing.getTemplates(false);
        for (Iterator i = templates.keySet().iterator(); i.hasNext(); ) {
            String Template = i.next().toString();
            System.out.println("  lu_table_template("+Template+") {");
            System.out.print(templates.get(Template));
            System.out.println("  }\n");
        }
        final Map powerTemplates = timing.getTemplates(true);
        for (Iterator i = powerTemplates.keySet().iterator(); i.hasNext(); ) {
            String Template = i.next().toString();
            System.out.println("  power_lut_template("+Template+") {");
            System.out.print(powerTemplates.get(Template));
            System.out.println("  }\n");
        }

        System.out.println("  define(proteus_cell_type,cell,string);");
        System.out.println("  define(proteus_channel,cell,string);");
        System.out.println("  define(proteus_channel,pin,string);");
        System.out.println("  define(proteus_ignore_timing,pin,boolean);");
        System.out.println("  define(proteus_scan_io,pin,boolean);");
        System.out.println("  define(proteus_scan_control,pin,boolean);");
        System.out.println("  define(proteus_budget_tau,cell,string);");
        System.out.println("  define(proteus_budget_riseDelay,timing,string);");
        System.out.println("  define(proteus_budget_fallDelay,timing,string);");

        System.out.println();
        System.out.println("  define(slacker_alignment,cell,string);");
        System.out.println("  define(slacker_time,cell,string);");
        System.out.println("  define(slacker_free_slack,cell,string);");
        System.out.println("  define(slacker_ntpc,cell,string);");
        System.out.println("  define(slacker_leaf,cell,boolean);");
        System.out.println("  define(slacker_ignore,cell,string);");
        System.out.println("  define(slacker_initial_tokens,cell,string);");
        System.out.println("  define(slacker_handshakes,cell,string);");
        System.out.println();
    }

    /** Print lib file footer **/
    private void emitFooter() {
        if (nofooter) return;
        System.out.println("}");
    }

    /** Generate liberty file for a list of logic gates **/
    public GenerateLib(int Inputs, int Strengths, MultiSet gates, String task,
                       boolean noheader, boolean nofooter,
                       String timingDir, String module,
                       boolean image, boolean virtualClocks, boolean macroLib,
                       CastFileParser castParser) {
        this.Inputs = Inputs;
        this.Task = task;
        this.noheader = noheader;
        this.nofooter = nofooter;
        this.timingDir = timingDir;
        this.module = module;
        this.virtualClocks = virtualClocks;
        this.macroLib = macroLib;
        this.castParser = castParser;
        this.nameInterface = new IdentityNameInterface();
        if (image) {
            // enumerate non-canonical image gates too
            for (Iterator i = gates.iterator(); i.hasNext(); ) {
                long T = ((Long) i.next()).longValue();
                MultiSet noncanonical = Karnaugh.getInvertedEquivalentGates(T);
                //forward latency is null here
                generateLibrary(noncanonical,Strengths,true,null);
            }
        }
        //forward latency is null
        else generateLibrary(gates,Strengths,false,null);
    }
     /** Generate liberty file for a list of logic gates with forward latency "tau" defined**/
    public GenerateLib(int Inputs, int Strengths, MultiSet gates, String task,
                       boolean noheader, boolean nofooter,
                       String timingDir, String module,
                       boolean image, boolean virtualClocks, boolean macroLib,
                       CastFileParser castParser,String tau) {
        this.Inputs = Inputs;
        this.Task = task;
        this.noheader = noheader;
        this.nofooter = nofooter;
        this.timingDir = timingDir;
        this.module = module;
        this.virtualClocks = virtualClocks;
        this.macroLib = macroLib;
        this.castParser = castParser;
        this.nameInterface = new IdentityNameInterface();

        if (image) {
            // enumerate non-canonical image gates too
            for (Iterator i = gates.iterator(); i.hasNext(); ) {
                long T = ((Long) i.next()).longValue();
                MultiSet noncanonical = Karnaugh.getInvertedEquivalentGates(T);
                generateLibrary(noncanonical,Strengths,true,tau);
            }
        }
        else generateLibrary(gates,Strengths,false,tau);
    }

    /** check if this type needs a virtual clock **/
    private boolean needsVirtualClock(String baseName) {
        if (baseName.equals("TIE") ||
            baseName.equals("INV") ||
            baseName.equals("INVINV") ||
            baseName.startsWith("NOR") ||
            baseName.startsWith("AND") ||
            baseName.startsWith("CTREE") ||
            baseName.startsWith("RESET_INVINV") ||
            baseName.startsWith("RESET_CTREE")) return false;
        return true;
    }
                
    /** Generate liberty file for a single special cell **/
    public GenerateLib(int Strengths, String baseName, String task,
                       boolean noheader, boolean nofooter,
                       String timingDir, String module,
                       boolean image, boolean virtualClocks, boolean macroLib,
                       CastFileParser castParser, String translate){
        this.Inputs = 1;
        this.Task = task;
        this.noheader = noheader;
        this.nofooter = nofooter;
        this.timingDir = timingDir;
        this.module = module;
        this.virtualClocks = virtualClocks;
        this.castParser = castParser;
        this.macroLib = macroLib;
        setNameInterface(translate);
        String footName = null;

        for (int i = 0; i < Strengths; i++) {
            String castName = module + "." + baseName +
                (Strengths>1 ? "." + i : "");
            if (image) {
                String cellName = baseName + (Strengths>1 ? "_X" + i : "");
                //forward latency is null(the last argument)
                emitImageLib(castName,0,cellName,baseName,null,"",null);
            } else {
                String f = null;
                String cellName = castName;
                if      (baseName.equals("INV"))    f = "!a";
                else if (baseName.equals("INVINV")) f = "a";
                boolean useVirtualClocks = true; // assume true if unknown
                if (module.equals("synthesis.qdi.special")) {
                    useVirtualClocks = needsVirtualClock(baseName);
                } else if (module.equals("synthesis.mld.special")) {
                    if (!baseName.startsWith("CTRL") &&
                        !baseName.startsWith("TOK_CTRL"))
                        useVirtualClocks = false;
                } else if (module.equals("")) {
                    useVirtualClocks = true;
                    cellName = baseName;
                    castName = baseName;
                    footName = renameCell(baseName);
                }
                cellName = renameCell(castName); // optional translation
                emitRealLib(castName, cellName,
                            Strengths>1 ? baseName : footName, f,
                            useVirtualClocks,false,null);
            }
        }
    }

    /** Generate liberty file for a single special cell when the forward latency "tau" is defined**/
    public GenerateLib(int Strengths, String baseName, String task,
                       boolean noheader, boolean nofooter,
                       String timingDir, String module,
                       boolean image, boolean virtualClocks, boolean macroLib,
                       CastFileParser castParser, String translate, String tau){
        this.Inputs = 1;
        this.Task = task;
        this.noheader = noheader;
        this.nofooter = nofooter;
        this.timingDir = timingDir;
        this.module = module;
        this.virtualClocks = virtualClocks;
        this.castParser = castParser;
        this.macroLib = macroLib;
        setNameInterface(translate);
        String footName = null;

        for (int i = 0; i < Strengths; i++) {
            String castName = module + "." + baseName +
                (Strengths>1 ? "." + i : "");
            if (image && !this.macroLib) {
                String cellName = baseName + (Strengths>1 ? "_X" + i : "");
                emitImageLib(castName,0,cellName,baseName,null,"",tau);
            } else {
                String f = null;
                String cellName = castName;
                if      (baseName.equals("INV"))    f = "!a";
                else if (baseName.equals("INVINV")) f = "a";
                boolean useVirtualClocks = true; // assume true if unknown
                if (module.equals("synthesis.qdi.special")) {
                    useVirtualClocks = needsVirtualClock(baseName);
                } else if (module.equals("synthesis.mld.special")) {
                    if (!baseName.startsWith("CTRL") &&
                        !baseName.startsWith("TOK_CTRL"))
                        useVirtualClocks = false;
                } else if (module.equals("")) {
                    useVirtualClocks = true;
                    cellName = baseName;
                    castName = baseName;
                    footName = renameCell(baseName);
                }
                cellName = renameCell(castName); // optional translation
                emitRealLib(castName, cellName,
                            Strengths>1 ? baseName : footName, f,
                            useVirtualClocks,image,tau);
            }
        }
    }

    /** set the current namespace translator **/
    private void setNameInterface(String translate) {
        if (translate!=null && translate.equals("cadence"))
            nameInterface = new CadenceNameInterface (true);
        else if (translate!=null && translate.equals("gds2"))
            nameInterface = new GDS2NameInterface ();
        else nameInterface = new IdentityNameInterface();
    }

    /** rename the cell **/
    private String renameCell(String cellName) {
        try {
            cellName = nameInterface.renameCell(cellName);
        } catch(Exception e) {
            System.err.println("Error: Illegal name translation from "+ cellName);
        }
        return cellName;
    }

    /** generate logic libraries for given gate list **/
    private void generateLibrary(MultiSet gates, int Strengths, boolean image,String tau) {
        Iterator iter = gates.iterator();
        while (iter.hasNext()) {
            long T = ((Long) iter.next()).longValue();
            long BT = Karnaugh.bestOverInversions(T);
            int invMask = Karnaugh.getInversionMask(T);
            String baseName = "LOGIC" + Inputs + "_" + Karnaugh.getGateString(T);
            String trueFunc = Karnaugh.getFuncString(T,true,false,true);
            for (int i = 0; i < Strengths; i++) {
                if (image) {
                    // figure out names
                    String canonicalName = module + ".LOGIC" + Inputs + "_" +
                        Karnaugh.getGateString(BT) +
                        (Strengths>1 ? "." + i : "");
                    String cellName = baseName +
                        (Strengths>1 ? "_X" + i : "");

                    // emit comment for non/canonical gates
                    String comment;
                    if (BT==T) comment="/* Canonical Gate */";
                    else comment="/* Non-Canonical form of " +
                        Karnaugh.getGateString(BT) +
                        " with inversion mask " +
                        NumberFormatter.toHexString(invMask,0) +
                        " */";

                    // emit image library for this gate
                    emitImageLib(canonicalName, invMask, cellName, baseName,
                                 trueFunc, comment,tau);
                }
                else {
                    String [] prefixes;
                    if      (Task.equals("mld"))    prefixes = mldPrefixes;
                    else if (Task.equals("qdi"))    prefixes = qdiPrefixes;
                    else if (Task.equals("domino")) prefixes = dominoPrefixes;
                    else                            prefixes = new String [] {""};
                    for (int j=0; j<prefixes.length; j++) {
                        // figure out names
                        String prefix = prefixes[j];
                        if (prefix.equals("C_") /* && Inputs>3 */) continue;
                        String castName = module + "." +
                            prefix + baseName + (Strengths>1 ? "." + i : "");
                        String cellName = castName;

                        // emit real library for this gate
                        emitRealLib(castName, cellName,
                                    Strengths>1 ? prefix + baseName : null, null,
                                    Task.equals("mld") && prefix.equals("V_") ||
                                    Task.equals("qdi"),false,null);
                    }
                }
            }
        }
    }

    /** describe an image input pin **/
    void emitImageInput(CellTiming timing, double fanout_load, boolean setupCheck,
                        String IA, String A0, String A1) {

        // start pin description
        System.out.println("  pin(" + IA + ") {");
        System.out.println("    direction : input;");
        System.out.println("    proteus_channel : \"e1of2\";");

        // report input capacitance
        double cap0 = timing.getInputCap(A0);
        double cap1 = timing.getInputCap(A1);
        double cap = cap0 > cap1 ? cap0 : cap1;
        System.out.println("    capacitance : " + cap + ";");

        // set fanout_load
        if (Task.equals("qdi"))
            System.out.println("    fanout_load : " + fanout_load + ";");
        
        // print dummy setup constraints for TOK_BUF's
        if (setupCheck) System.out.print(timing.setupCheck("CK",null));

        // finish pin description
        System.out.println("  }");
    }

    /** shortcut **/
    void emitImageInput(CellTiming timing, String A) {
        emitImageInput(timing,1.0,false,A,A + ".0",A + ".1");
    }

    /** describe a real input pin **/
    void emitRealInput(CellTiming timing, String A) {
        System.out.println("  pin(" + A + ") {");
        System.out.println("    direction : input;");
        System.out.println("    capacitance : " + timing.getInputCap(A) + ";");
        System.out.println("    fanout_load : 1.0;");
        System.out.println("  }");
    }

    /** describe a timing arc from an image input pin to an image output pin **/
    void emitImageToImageTiming(CellTiming timing, String IA,
                                String A0, String A1, String X0, 
                                String X1, String tau) {
        for (int from_dir=0; from_dir<2; from_dir++) {
            for (int to_dir=0; to_dir<2; to_dir++) {
                String sense = (from_dir==to_dir) ?
                    "positive_unate" : "negative_unate";
                String type = (to_dir==1) ?
                    "combinational_rise" : "combinational_fall";
                String cell_dir = (to_dir==1) ?
                    "cell_rise" : "cell_fall";
                String dir_transition = (to_dir==1) ?
                    "rise_transition" : "fall_transition";
                String from  = (from_dir==1) ? A1 : A0;
                String to    = (to_dir==1)   ? X1 : X0;
                String delay,slew;
                delay = timing.getDelayMatrix(from,"+",to,"+",tau);
                slew  = timing.getSlewMatrix(from,"+",to,"+",tau);
                if (delay!=null && slew!=null) {
                    System.out.println("    timing() {");
                    System.out.println("      related_pin : \"" + IA + "\";");
                    System.out.println("      timing_sense : " + sense + ";");
                    System.out.println("      timing_type : " + type + ";");
                    System.out.print(timing.getDelaySlew(cell_dir,dir_transition,
                                                         delay,slew));
                    System.out.println("    }");
                }
            }
        }
    }

    /** describe a timing arc from an real input pin to an real output pin **/
    void emitRealToRealTiming(CellTiming timing, String A, String X, String tau) {
        String from  = A;
        String to = X;
        for (int to_dir=0; to_dir<2; to_dir++) {
            String sense = "positive_unate";
            String type = (to_dir==1) ?
                "combinational_rise" : "combinational_fall";
            String cell_dir = (to_dir==1) ?
                "cell_rise" : "cell_fall";
            String dir_transition = (to_dir==1) ?
                "rise_transition" : "fall_transition";
            String delay,slew;
            delay = timing.getDelayMatrix(from,"+",to,"+",tau);
            slew  = timing.getSlewMatrix(from,"+",to,"+",tau);
            if (delay!=null && slew!=null) {
                System.out.println("    timing() {");
                System.out.println("      related_pin : \"" + A + "\";");
                System.out.println("      timing_sense : " + sense + ";");
                System.out.println("      timing_type : " + type + ";");
                System.out.print(timing.getDelaySlew(cell_dir,dir_transition,
                                                     delay,slew));
                System.out.println("    }");
            }
        }
    }

    /** describe a timing arc from an image input pin to an image output pin **/
    void emitClockToImageTiming(CellTiming timing,
                                String A0, String A1, String X0,
                                String X1, String tau) {
        int from_dir=1; // don't duplicate timing arcs
        System.out.println("    timing() {");
        System.out.println("      related_pin : \"CK\";");
        System.out.println("      timing_sense : non_unate;");
        System.out.println("      timing_type : rising_edge;");
        for (int to_dir=0; to_dir<2; to_dir++) {
            String cell_dir = (to_dir==1) ?
                "cell_rise" : "cell_fall";
            String dir_transition = (to_dir==1) ?
                "rise_transition" : "fall_transition";
            String from  = (from_dir==1) ? A1 : A0;
            String to    = (to_dir==1)   ? X1 : X0;
            String delay,slew;
            delay = timing.getDelayMatrix(from,"+",to,"+",tau);
            slew  = timing.getSlewMatrix(from,"+",to,"+",tau);
            System.out.print(timing.getDelaySlew(cell_dir,dir_transition,
                                                 delay,slew));
        }
    }

    /** describe a timing arc from a real input pin to an image output pin **/
    void emitRealToImageTiming(CellTiming timing, String A, String X,
                               String tau) {
        int from_dir = 1;
        String from  = A;
        String X0 = X + ".0";
        String X1 = X + ".1";
        for (int to_dir=0; to_dir<2; to_dir++) {
            String sense = (from_dir==to_dir) ?
                "positive_unate" : "negative_unate";
            String type = (to_dir==1) ?
                "combinational_rise" : "combinational_fall";
            String cell_dir = (to_dir==1) ?
                "cell_rise" : "cell_fall";
            String dir_transition = (to_dir==1) ?
                "rise_transition" : "fall_transition";
            String to    = (to_dir==1) ? X1 : X0;
            String delay,slew;
            delay = timing.getDelayMatrix(from,"+",to,"+",tau);
            slew  = timing.getSlewMatrix(from,"+",to,"+",tau);
            if (delay!=null && slew!=null) {
                System.out.println("    timing() {");
                System.out.println("      related_pin : \"" + A + "\";");
                System.out.println("      timing_sense : " + sense + ";");
                System.out.println("      timing_type : " + type + ";");
                System.out.print(timing.getDelaySlew(cell_dir,dir_transition,
                                                     delay,slew));
                System.out.println("    }");
            }
        }
    }

    /** describe a timing arc from an image input pin to a real output pin **/
    void emitImageToRealTiming(CellTiming timing, String A, String X,
                               String tau) {

        int to_dir=1;
        String to = X;
        String A0 = A + ".0";
        String A1 = A + ".1";
        for (int from_dir=0; from_dir<2; from_dir++) {
            String sense = (from_dir==to_dir) ?
                "positive_unate" : "negative_unate";
            String type = (to_dir==1) ?
                "combinational_rise" : "combinational_fall";
            String cell_dir = (to_dir==1) ?
                "cell_rise" : "cell_fall";
            String dir_transition = (to_dir==1) ?
                "rise_transition" : "fall_transition";
            String from  = (from_dir==1) ? A1 : A0;
            String delay,slew;
            delay = timing.getDelayMatrix(from,"+",to,"+",tau);
            slew  = timing.getSlewMatrix(from,"+",to,"+",tau);
            if (delay!=null && slew!=null) {
                System.out.println("    timing() {");
                System.out.println("      related_pin : \"" + A + "\";");
                System.out.println("      timing_sense : " + sense + ";");
                System.out.println("      timing_type : " + type + ";");
                System.out.print(timing.getDelaySlew(cell_dir,dir_transition,
                                                     delay,slew));
                System.out.println("    }");
            }
        }
    }
    
    /** emit synchronous image libraries **/
    private void emitImageLib(String canonicalName, int inversionMask,
                              String cellName, String footName, String trueFunc,
                              String comment,String tau) {
        // handle special image cells
        boolean isBUF=false, isTOK=false, isEDFF=false, isRECV=false,
            isSEND=false, isSCAN=false, isFROM=false, isTO=false,
            isCONFIG=false, isCONFIG2=false, isScanOneOf=false;
        int N = Inputs, M = 0;
        if (footName.equals("BUF")) {
            isBUF=true;
            trueFunc="L";
        } else if (footName.equals("INV")) {
            isBUF=true;
            inversionMask=1;
            canonicalName = StringUtil.replaceSubstring(canonicalName,"INV","BUF");
            trueFunc="!L";
        } else if (footName.equals("TOK_BUF")) {
            isBUF=true; isTOK=true;
            trueFunc="IQ";
        } else if (footName.equals("EDFF")) {
            isEDFF=true;
            N=2;
            trueFunc="E & L | !E & IQ";
        } else if (footName.equals("TOK_EDFF")) {
            isTOK=true; isEDFF=true;
            N=2;
            trueFunc="IQ";
        } else if (footName.equals("SCAN_BUF")) {
            isBUF=true; isSCAN=true;
            trueFunc="L";
        } else if (footName.equals("SCAN_BUF_1of2")) {
            isBUF=true; isSCAN=true;
            isScanOneOf=true;
            canonicalName = StringUtil.replaceSubstring(canonicalName,"SCAN_BUF_1of2","SCAN_BUF");
            N=2;
        } else if (footName.equals("SCAN_BUF_1of4")) {
            isBUF=true; isSCAN=true;
            isScanOneOf=true;
            N=4;
        } else if (footName.equals("SCAN_TOK_BUF")) {
            isBUF=true; isTOK=true; isSCAN=true;
            trueFunc=null;
        } else if (footName.equals("SCAN_EDFF")) {
            isEDFF=true; isSCAN=true;
            N=2;
            trueFunc="E & L | !E & IQ";
        } else if (footName.equals("SCAN_TOK_EDFF")) {
            isTOK=true; isEDFF=true; isSCAN=true;
            N=2;
            trueFunc=null;
        } else if (footName.equals("SCAN_CONFIG")) {
            isSCAN=true; isCONFIG=true;
        } else if (footName.equals("SCAN_CONFIG2")) {
            isSCAN=true; isCONFIG2=true;
        } else if (footName.equals("SCAN_CONFIG_FALSE")) {
            isSCAN=true; isCONFIG=true;
        } else if (footName.equals("SCAN_CONFIG_TRUE")) {
            isSCAN=true; isCONFIG=true;
        } else if (footName.equals("RECV_1of2")) {
            isRECV=true;
            N=1; M=2;
        } else if (footName.equals("RECV_1of4")) {
            isRECV=true;
            N=2; M=4;
        } else if (footName.equals("RECV_1of8")) {
            isRECV=true;
            N=3; M=8;
        } else if (footName.equals("SEND_1of2")) {
            isSEND=true;
            N=1; M=2;
        } else if (footName.equals("SEND_1of4")) {
            isSEND=true;
            N=2; M=4;
        } else if (footName.equals("SEND_1of8")) {
            isSEND=true;
            N=3; M=8;
        } else if (footName.equals("FROM_1of2")) {
            isFROM=true;
            N=1; M=2;
            canonicalName = StringUtil.replaceSubstring(canonicalName,"FROM_1of2","BUF");
        } else if (footName.equals("FROM_1of4")) {
            isFROM=true;
            N=2; M=4;
        } else if (footName.equals("FROM_1of8")) {
            isFROM=true;
            N=3; M=8;
        } else if (footName.equals("TO_1of2")) {
            isTO=true;
            N=1; M=2;
            canonicalName = StringUtil.replaceSubstring(canonicalName,"TO_1of2","BUF");
        } else if (footName.equals("TO_1of4")) {
            isTO=true;
            N=2; M=4;
        } else if (footName.equals("TO_1of8")) {
            isTO=true;
            N=3; M=8;
        }

        // parse cell timing
        CellTiming timing = new CellTiming(timingDir,canonicalName,false);
        if (!timing.valid) return;
        double area = estimateArea(timing.getArea(),canonicalName);

        // print out cell properties
        System.out.println(comment);
        System.out.println("cell (" + cellName + ") {");
        if (!footName.equals(cellName))
            System.out.println("  cell_footprint : " + footName + ";");
        System.out.println("  area : " + NumberFormatter.format(area,4) + ";");

        // special handling of SCAN
        if (isSCAN) {
            String celltype = "scan_buf";
            if (isCONFIG || isCONFIG2) celltype = "scan_config";
            else if (isTOK) celltype = "scan_seq";
            else if (isScanOneOf) celltype = "scan_buf_1of" + N;
            System.out.println("  proteus_cell_type : \"" + celltype + "\";");
            if (!isCONFIG && !isCONFIG2) {
                //Add special case for SCAN_TOKs as they don't have C[3] pins
                int control_bits = isTOK ? 3: 4;
                for (int i=0; i<control_bits; i++) {
                    System.out.print
                        (timing.dummyInput
                         ("C[" + i + "]",null,"    proteus_scan_control : true;\n"));
                }
            }
            if (isTOK) {
                System.out.print
                    (timing.dummyInput
                     ("SE","erase",
                      "    nextstate_type : scan_enable;\n" +
                      timing.setupCheck("CK",null)));
                System.out.print
                    (timing.dummyInput
                     ("LS","e1of2",
                      "    proteus_scan_io : true;\n" +
                      "    nextstate_type : scan_in;\n" +
                      timing.setupCheck("CK",null)));
                System.out.print
                    (timing.dummyOutput
                     ("RS","e1of2",
                      "    proteus_scan_io : true;\n" +
                      "    signal_type : test_scan_out;\n" +
                      "    internal_node : \"IQ1\";\n" +
                      "    test_output_only : true;\n"));
                if (isEDFF) System.out.print(SCAN_TOK_EDFF_Image);
                else        System.out.print(SCAN_TOK_BUF_Image);
            } else {
                if (!isEDFF) // add CK for pin consistency only
                    System.out.print(timing.dummyInput("CK"));
                System.out.print
                    (timing.dummyInput
                     ("SE","erase"));
                System.out.print
                    (timing.dummyInput
                     ("LS","e1of2","    proteus_scan_io : true;\n"));
                System.out.print
                    (timing.dummyOutput
                     ("RS","e1of2","    proteus_scan_io : true;\n"));
            }
            if (isCONFIG) {
                System.out.print
                    (timing.dummyInput
                     ("LC",null,
                      "    proteus_scan_control : true;\n"));
                System.out.print
                    ("  pin(RC) {\n" +
                     "    direction : output;\n" +
                     "    proteus_scan_control : true;\n" +
                     "    capacitance : 0.0;\n" +
                     "  }\n");
                System.out.println("}\n");
                return;
            } else if (isCONFIG2) {
                for (int i=0; i<2; i++) {
                    System.out.print
                        (timing.dummyInput
                         ("LC[" + i + "]",null,
                          "    proteus_scan_control : true;\n"));
                    System.out.print
                        ("  pin(RC[" + i + "]) {\n" +
                         "    direction : output;\n" +
                         "    proteus_scan_control : true;\n" +
                         "    capacitance : 0.0;\n" +
                         "  }\n");
                }
                System.out.println("}\n");
                return;
            }
            if (isScanOneOf) {
                //Take care of input side
                for (int i=0; i<N; i++) {
                    emitRealInput(timing,"L."+i);
                }
                System.out.print(timing.dummyOutput("L.e"));
                //Take care of outputs
                for (int i=0; i<N; i++) {
                    String A = "L." + i;
                    String X = "R." + i;
                    System.out.println("  pin(" + X +") {");
                    System.out.println("    direction : output;");
                    if (Task.equals("qdi")) System.out.println("    max_fanout : 3.0;");
                    emitRealToRealTiming(timing,A,X,tau);
                    System.out.println("  }");
                }
                emitRealInput(timing,"R.e");
                System.out.println("}\n");
                return;
            }
        }

        // special handling of SEND/RECV/TO/FROM
        if (isRECV || isFROM) {
            // external pins
            for (int i=0; i<M; i++) emitRealInput(timing,"L." + i);
            System.out.print(timing.dummyOutput("L.e"));

            // internal pins
            if (isRECV) emitImageInput(timing,"E");

            // timing arcs to image data outputs
            for (int i=0; i<N; i++) {
                String X = "R[" + i + "]";
                if (isFROM && N==1) X = "R";
                String X0 = X + ".0";
                String X1 = X + ".1";
                System.out.println("  pin(" + X +") {");
                System.out.println("    direction : output;");
                System.out.println("    proteus_channel : \"e1of2\";");
                if (Task.equals("qdi")) System.out.println("    max_fanout : 3.0;");
                for (int j=0; j<M; j++) emitRealToImageTiming(timing,"L." + j,X,tau);
                if (isRECV) emitImageToImageTiming(timing,"E","E.0","E.1",X0,X1,tau);
                System.out.println("  }");
            }

            // close cell block
            System.out.println("}\n");
            return;
        } else if (isSEND || isTO) {
            // timing arcs to real data outputs
            for (int i=0; i<M; i++) {
                String X = "R." + i;
                System.out.println("  pin(" + X +") {");
                System.out.println("    direction : output;");
                for (int j=0; j<N; j++) {
                    String L = "L[" + j + "]";
                    if (isTO && N==1) L = "L";
                    emitImageToRealTiming(timing,L,X,tau);
                }
                if (isSEND) emitImageToRealTiming(timing,"E",X,tau);
                System.out.println("  }");
            } 
            emitRealInput(timing,"R.e");

            // internal pins
            if (isSEND) emitImageInput(timing,"E");
            for (int i=0; i<N; i++) {
                String L = "L[" + i + "]";
                if (isTO && N==1) L = "L";
                emitImageInput(timing,L);
            }

            // close cell block
            System.out.println("}\n");
            return;
        }

        // handle inverted output by twisting output rails
        int invX = (inversionMask>>N)&1;
        String X = (isBUF || isTOK || isEDFF) ? "R" : "X";
        String X0 = X + "." + invX;
        String X1 = X + "." + (1-invX);

        // print input ports with capacitance
        for (int j=0; j<N; j++) {

            // handle inverted inputs by twisting input rails
            int invA = (inversionMask>>j)&1;
            String A,IA;
            if (isBUF || isTOK || isEDFF) {
                A = IA = (j==0) ? "L" : "E";
            } else {
                A  = "A[" + j + "]";
                IA = "A" + j;
            }
            String A0 = A + "." + invA;
            String A1 = A + "." + (1-invA);
            double fanout_load = isBUF||isTOK ? 0.33 : 1.0;
            emitImageInput(timing,fanout_load,isTOK||isEDFF,IA,A0,A1);
        }

        // print virtual CK/RN/SN input pins for TOK_BUF/EDFF/TOK_EDFF
        if (isTOK || isEDFF) {
            System.out.print(timing.clockInput());
            System.out.print(timing.dummyInput("RN"));
            System.out.print(timing.dummyInput("SN"));
            if (!(isSCAN && isTOK)) {
                System.out.println("  ff(IQ,IQN) {");
                System.out.println("    clocked_on : \"CK\";");
                if (isEDFF) {
                    if (inversionMask%2==0)
                        System.out.println("    next_state : \"E & L | !E & IQ\"");
                    else
                        System.out.println("    next_state : \"E & !L | !E & IQ\"");
                } else {
                    if (inversionMask%2==0) System.out.println("    next_state : \"L\";");
                    else                    System.out.println("    next_state : \"!L\";");
                }
                System.out.println("    clear : \"!RN\";");
                System.out.println("    preset : \"!SN\";");
                System.out.println("    clear_preset_var1 : H;");
                System.out.println("    clear_preset_var2 : L;");
                System.out.println("  }");
            }
        }

        // print output port with timing
        System.out.println("  pin(" + X +") {");
        System.out.println("    direction : output;");
        System.out.println("    proteus_channel : \"e1of2\";");
        if (isSCAN && isTOK)
            System.out.println("    internal_node : \"IQ0\";");
        if (trueFunc!=null) System.out.println("    function : \"" + trueFunc + "\";");
        if (Task.equals("qdi")) {
            System.out.println("    max_fanout : " +
                               (isBUF || isTOK || isEDFF ? 3.0 : 2.0) + ";");
        }

        // check paths from all inputs
        for (int j=0; j<(isTOK ? 1 : N); j++) {

            // handle inverted inputs by twisting input rails
            int invA = (inversionMask>>j)&1;
            String A,IA;
            if (isEDFF) {
                A  = (j==0) ? "L" : "E";
                IA = isTOK ? "CK" : A;
            } else if (isBUF) {
                A  = "L";
                IA = isTOK ? "CK" : A;
            } else {
                A  = "A[" + j + "]";
                IA = "A" + j;
            }
            String A0 = A + "." + invA;
            String A1 = A + "." + (1-invA);

            // special handling of TOK_BUF's and TOK_EDFF's
            if (isTOK) {
                // dummy timing arc from RN
                System.out.println("    timing() {");
                System.out.println("      related_pin : \"RN\";");
                System.out.println("      timing_type : clear;");
                System.out.println("      timing_sense : positive_unate;");
                System.out.print(timing.getDelaySlew("cell_fall", "fall_transition",
                                                     timing.zeroValues,
                                                     timing.zeroValues));
                System.out.println("    }");

                // dummy timing arc from SN
                System.out.println("    timing() {");
                System.out.println("      related_pin : \"SN\";");
                System.out.println("      timing_type : preset;");
                System.out.println("      timing_sense : negative_unate;");
                System.out.print(timing.getDelaySlew("cell_rise", "rise_transition",
                                                     timing.zeroValues,
                                                     timing.zeroValues));
                System.out.println("    }");

                // timing from CK (set to L->R delays)
                emitClockToImageTiming(timing,A0,A1,X0,X1,tau);
                System.out.println("    }");
            }

            // look for paths from rising and falling inputs to output
            else emitImageToImageTiming(timing,IA,A0,A1,X0,X1,tau);
        }
        System.out.println("  }");

        // close cell block
        System.out.println("}\n");
    }

    private String portRename (String castName) {
        String out = castName;
        try {
            out = nameInterface.renameNode(castName);
        }
        catch(Exception e) {
            System.err.println("Illegal port name "+castName);
        }
        return out;
    }

    private HierName makeHierName(final String s) {
        try {
            return HierName.makeHierName(s, '.');
        } catch (InvalidHierNameException e) {
            throw new RuntimeException("Cannot create HierName " + s, e);
        }
    }

    private void addPinProp(final Map<String,Map<String,String>> pinProps,
                            final String pin, final String prop,
                            final String val) {
        Map<String,String> props = pinProps.get(pin);
        if (props == null) {
            props = new TreeMap<String,String>();
            pinProps.put(pin, props);
        }
        props.put(prop, val);
    }
    
    /**
     * For each slacker directive key-value pair, generate an appropriate pipe
     * delimited string suitable for inclusion in the lib file.
     **/
    private Pair<String,TreeMap<String,String>> getChannelDirectiveString(final Map dm, final Map chanMap) {
        return getChannelDirectiveString(dm, chanMap, null);
    }
     
    /**
     * For each slacker directive key-value pair, generate an appropriate pipe
     * delimited string suitable for inclusion in the lib file, taking into
     * account possible cell level directive.
     **/
    private Pair<String,TreeMap<String,String>> getChannelDirectiveString(final Map dm, final Map chanMap,
                                             final Object def) {
        final TreeMap<String,String> directive =
            new TreeMap<String,String>(NaturalStringComparator.getInstance());
         final TreeMap<String,String> slackerMap =
            new TreeMap<String,String>(NaturalStringComparator.getInstance());

        if (dm != null) {
            for (Iterator i = dm.keySet().iterator(); i.hasNext();) {
                Object key = i.next();
                String dir = portRename(key.toString());
                // FIXME: This fails when the directive is specified
                // on the wide channel, unless nameInterface is
                // CadenceNameInterface.  For example, suppose we had:
                // slacker_time(L[0,1,2]) = 10, and we look for the
                // directive on the component narrow channel
                // L[0,1,2,3].  Then the regex "L\[0,1,2\]" will not
                // match L[0,1,2,3], but with Cadence names,
                // "L\[0\]\[1\]\[2\]" will match L[0][1][2][3].
                // Currently this is not a visible problem, because
                // lve always uses --translate=cadence, and slacker
                // always outputs directives on narrow channels.
                String edir =
                    (dir.replaceAll("\\[", "\\\\[")).replaceAll("\\]","\\\\]");
                for (Iterator j = chanMap.keySet().iterator(); j.hasNext();) {
                    String ch = j.next().toString();
                    if (dir.equals(ch) || (directive.get(ch) == null &&
                                           ch.matches("^"+edir+"[\\[\\.].*$")))
                    {
                        directive.put(ch, ch + '=' + dm.get(key));
                        slackerMap.put(ch,dm.get(key).toString());

                    }
                }
            }
        }
        if (def != null) {
            // set all channels without a directive to the default value
            for (Iterator i = chanMap.keySet().iterator(); i.hasNext();) {
                String ch = i.next().toString();
                if (!directive.containsKey(ch)) {
                    directive.put(ch, ch + '=' + def);
                    slackerMap.put(ch,def.toString());

                }
            }
        }

        if (directive.isEmpty()) {
            return new Pair("",slackerMap);
        } else {
            Pair p = new Pair(StringUtil.join(directive.values().toArray(new String[0]),
                                   '|'),slackerMap);

            return p;
        }
    }

    /** emit real asynchronous library, using cast too **/
    private void emitRealLib(String castName, String cellName, String footName,
                             String trueFunc, boolean useVirtualClocks,boolean image, String tau) {


        // parse cast
        String cellstring = "";
        final CellInterface ci;
        try {
            ci = castParser.getFullyQualifiedCell ( castName );
        } catch (CastSemanticException e) {
            System.err.println("Cannot load cell : "+castName);
            ExceptionPrettyPrinter.printException(e, System.err);
            return;
        }

        final Cadencize cad = new Cadencize(true);
        final AliasedMap ports = cad.convert(ci).getPortNodes();

        // e1ofN channels, from renamed channel name to channel description
        // (e.g., RD[2] -> e1of2 +RD[2])
        final Map<String,String> e1ofChan =
            new TreeMap<String,String>(NaturalStringComparator.getInstance());

        // 1ofN channels, from renamed channel name to channel description
        // (e.g., LS.DFT.C -> 1of3 -LS.DFT.C)
        final Map<String,String> x1ofChan =
            new TreeMap<String,String>(NaturalStringComparator.getInstance());

        // Map from renamed canonical name to set of all renamed channels it
        // belongs to (e.g., AR[0].e -> AR[0].e AR[1].e IR.e)
        final MultiMap<String,String> aliasMap =
            new MultiMap<String,String>(
                new HashMap<String,Collection<String>>(),
                new MultiMap.CollectionFactory<String>() {
                    public Collection<String> newCollection() {
                        return new TreeSet<String>(
                            NaturalStringComparator.getInstance());
                    }
                });

        // Pin properties (pin -> (key -> value))
        final Map<String,Map<String,String>> pinProps =
            new HashMap<String,Map<String,String>>();

        // Pin to Slacker Time (pin -> (Slacker time)) Map
        final Map<String,String> pinToSlackerMap =
            new HashMap<String,String>();


        (new CellUtils.MarkPort() {
            // Are we processing the content of a SramSerialChannel?
            private boolean sramSerial = false;

            // Current channel being processed; renamed
            private String chanName = null;

            // Currently processing an e1ofN channel
            private boolean e1ofN = false;

            private String getDirString(final int idir) {
                final String dir;
                if (idir < 0) {
                    dir = PortDefinition.INSTRING;
                } else if (idir == 0) {
                    dir = PortDefinition.INOUTSTRING;
                } else {
                    dir = PortDefinition.OUTSTRING;
                }
                return dir;
            }

            private String rename(final String s) {
                return portRename(s);
            }

            private String canon(final String s) {
                return ports.getCanonicalKey(makeHierName(s)).toString();
            }

            private void dftProp(final String s, final String prop) {
                final String canon = canon(s);
                if (canon != null) {
                    addPinProp(pinProps, rename(canon), prop, "true");
                }
            }

            private int extract1ofN(final String tag) {
                String remainder =
                    tag.substring("standard.channel.1of".length());
                remainder = remainder.replace('(', ' ');
                remainder = remainder.replace(')', ' ');
                remainder = remainder.trim();

                return Integer.parseInt(remainder);
            }

            protected void mark(final StructureType structureType,
                                final String name, final int direction) {
                final String tag = structureType.getTag();
                final boolean isSramChan = CellUtils.isSramSerialChannel(tag);
                final boolean is1of = tag.startsWith("standard.channel.1of");
                if (isSramChan) {
                    sramSerial = true;
                } else if (CellUtils.isDftChannel(tag)) {
                    // identify nodes associated with the control and datapath
                    // of scan
                    if (!sramSerial) {
                        dftProp(name + ".D.e", "proteus_scan_io");
                        dftProp(name + ".D.d[0]", "proteus_scan_io");
                        dftProp(name + ".D.d[1]", "proteus_scan_io");
                    }
                } else if (is1of) {
                    chanName = rename(name);
                    x1ofChan.put(
                        chanName,
                        "1of" + extract1ofN(tag) + " " +
                        getDirString(direction) + chanName);
                }
                super.mark(structureType, name, direction);
                if (isSramChan) sramSerial = false;
                if (is1of) chanName = null;
            }

            protected void mark(final NodeType nodeType, final String name,
                                final int direction) {
                if (chanName != null) {
                    final String suffix = name.endsWith(".e") ? ".e" : ".d";
                    aliasMap.put(rename(canon(name)), chanName + suffix);
                }
                if (macroLib && !e1ofN) dftProp(name, "proteus_ignore_timing");
            }

            protected void mark(final ChannelType channelType,
                                final String name, final int direction) {
                chanName = rename(name);
                e1ofN = true;
                e1ofChan.put(
                    chanName,
                    "e1of" + CellUtils.extractN(channelType.getTypeName()) +
                    " " + getDirString(direction) + chanName);
                super.mark(channelType, name, direction);
                chanName = null;
                e1ofN = false;
            }
        }).mark(ci);

        // set pin level proteus_channel attribute
        for (String canon : aliasMap.keySet()) {
            Collection<String> c = aliasMap.get(canon);
            addPinProp(pinProps, canon, "proteus_channel", '"' +
                       StringUtil.join(c.toArray(new String[0]), ' ') + '"');
        }
        // parse cell timing
        CellTiming timing =
            new CellTiming(timingDir,castName,
                           virtualClocks && useVirtualClocks, macroLib);
        if (!timing.valid) return;
        double area = timing.getArea();
        double leakage = timing.getLeakage();

        // print out cell properties
        emitHeader(timing,cellName);
        cellstring += "cell (" + cellName + ") {\n";
        if (footName!=null) cellstring += "  cell_footprint : " + footName + ";\n";
        cellstring += "  area : " + area + ";\n";
        cellstring +="  cell_leakage_power : " + leakage + ";\n"; 
        //Add the cell_leakage_power here as a simple attribute in the cell
        // determine cell level proteus_channel attribute
        final StringBuilder proteus_channel = new StringBuilder();
        if (!e1ofChan.isEmpty()) {
            proteus_channel.append(
                StringUtil.join(e1ofChan.values().toArray(new String[0]), '|'));
        }
        if (!x1ofChan.isEmpty()) {
            if (!e1ofChan.isEmpty()) proteus_channel.append('|');
            proteus_channel.append(
                StringUtil.join(x1ofChan.values().toArray(new String[0]), '|'));
        }
        if (proteus_channel.length() > 0)
            cellstring += "  proteus_channel : \""+proteus_channel+"\";\n";

        final StringBuilder proteus_budget_tau = new StringBuilder();

        for (Iterator i = new SortingIterator(timing.slacktaus.keySet().iterator(), new FloatComparator()); i.hasNext(); ) {
            if (proteus_budget_tau.length() > 0)
                proteus_budget_tau.append(",");
            proteus_budget_tau.append((String)i.next());
        }
        if (proteus_budget_tau.length() > 0)
            cellstring += "  proteus_budget_tau : \""+proteus_budget_tau+"\";\n";

        // determine cell level slacker attributes
        Map dm = DirectiveUtils.getTopLevelDirective
            (ci, "slacker_time",
             DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE);
        Object def;
        Pair<String,TreeMap<String,String>> p ;
        p  = getChannelDirectiveString(dm, e1ofChan);
        String directive = p.getFirst();
        if (directive.length() > 0)
            cellstring += "  slacker_time : \""+directive+"\";\n";


        TreeMap<String,String> slackerMap = p.getSecond();
        for (String canon : aliasMap.keySet()) {
            Collection<String> c = aliasMap.get(canon);
            String[] ch_arr = c.toArray(new String[0]);
            String chan = ch_arr[0].substring(0,ch_arr[0].length()-2);
            if(!slackerMap.containsKey(chan))
                pinToSlackerMap.put(canon,null);
            else
                pinToSlackerMap.put(canon,slackerMap.get(chan));
        }
        //System.err.println("\nThe pinToSlackerTimeMap are :\n" +pinToSlackerMap);

 
        // if slacker_leaf is true, then slacker_times are relative to a single
        // reference time regardless of alignment group, but export
        // slacker_alignment directives anyway, because they are needed when
        // there is slacker_free_slack
        dm = DirectiveUtils.getTopLevelDirective
            (ci, "slacker_alignment",
             DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE);
        def = DirectiveUtils.getTopLevelDirective(ci, "slacker_alignment");
        p = getChannelDirectiveString(dm, e1ofChan, def);
        directive = p.getFirst();
        if (directive.length() > 0)
            cellstring += "  slacker_alignment : \""+directive+"\";\n";

        dm = DirectiveUtils.getTopLevelDirective (ci,
             "slacker_initial_tokens", DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE);

        p = getChannelDirectiveString(dm, e1ofChan);
        directive = p.getFirst();
        if (directive.length() > 0)
            cellstring += "  slacker_initial_tokens : \""+directive+"\";\n";

        dm = DirectiveUtils.getTopLevelDirective (ci,
             "slacker_handshakes", DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE);
        p = getChannelDirectiveString(dm, e1ofChan);
        directive = p.getFirst();

        if (directive.length() > 0)
            cellstring += "  slacker_handshakes : \""+directive+"\";\n";

        dm = DirectiveUtils.getTopLevelDirective
            (ci, "slacker_free_slack",
             DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE);
        p = getChannelDirectiveString(dm, e1ofChan);
        directive = p.getFirst();
        if (directive.length() > 0)
            cellstring += "  slacker_free_slack : \""+directive+"\";\n";

        final Cast.SlackerDirective slackerDir = new Cast.SlackerDirective(ci);
        cellstring += "  slacker_leaf : " + slackerDir.isLeaf() + ";\n";

        Object dirobj = DirectiveUtils.getTopLevelDirective(ci, "slacker_ntpc");
        if (dirobj != null) {
            final DirectiveEmitter em = CastEmitter.getInstance();
            cellstring += "  slacker_ntpc : " +
                em.emit(BlockInterface.CELL,
                        DirectiveConstants.FLOAT_TYPE, dirobj)+";\n";
        }

        dm = DirectiveUtils.getTopLevelDirective
            (ci, "slacker_ignore", DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE);
        p = getChannelDirectiveString(dm, e1ofChan);
        directive = p.getFirst();
        if (directive.length() > 0)
            cellstring += "  slacker_ignore : \""+directive+"\";\n";
       //Set the earliest arriving input slacker time in the cell 
         timing.setEarliestInput(pinToSlackerMap);

        //list outputs first to preprocess output timing before listing inputs
        String output_timing = timing.getAllOutputs(trueFunc, pinProps, nameInterface,pinToSlackerMap,image,tau);
        String input_timing =  timing.getAllInputs(pinProps, nameInterface,pinToSlackerMap,image,tau);
        
        //.lib file has inputs then outputs
        cellstring += input_timing;
        cellstring += output_timing;
        
        cellstring += "}\n";
        if (this.CellCount == 0) {
            temperature = timing.getTemperature();
            voltage = timing.getVoltage();
            process_name = timing.getProcess();
            /* override if in an alint directory */
            if (timingDir.matches("^.*/alint/[a-z][a-z]/[0-9\\.]+V/[0-9]+C/[^/]+$")) {
                final String[] dirsplit=timingDir.split("/");
                temperature = dirsplit[dirsplit.length-2].replaceAll("C","");
                voltage = dirsplit[dirsplit.length-3].replaceAll("V","");
                process_name = dirsplit[dirsplit.length-4];
            }
        }
        this.CellCount++;
        cellstring = cellstring.replaceAll("\n", "\n  ");
        cellstring = cellstring.replaceAll("\n *\n", "\n\n");
        cellstring = cellstring.replaceAll("\n *$", "\n");
        System.out.print("  "+cellstring);
        emitFooter();
    }

    /** estimate the area overhead of control **/
    double estimateArea(double area, String castName) {
        String [] fields = castName.split("\\.");
        String S = fields[fields.length-1];
        if (castName.startsWith("synthesis.qdi.special.TOK_EDFF") ||
            castName.startsWith("synthesis.qdi.special.EDFF") ||
            castName.startsWith("synthesis.qdi.special.SCAN_TOK_EDFF") ||
            castName.startsWith("synthesis.qdi.special.SCAN_EDFF")) {
            // set area to 1/4 to encourage use of EDFF's
            area *= 0.25;
        }
        else if (module.startsWith("synthesis.qdi.logic")) {
            // adjust QDI LOGIC area by adding CTREE2
            CellTiming ctree =
                new CellTiming(timingDir,
                               "synthesis.qdi.special.CTREE2." + S,
                               false);
            if (ctree.valid)
                area += ctree.getArea();
        }
        return area;
    }

    static public void usage( String m ) {
        System.err.println("Usage: GenerateLib\n" +
            " --cast-path=path      Cast path.\n" +
            " --timing-file=path    Directory containing timing file OR the timing file itself.\n" +
            " --cell=fqcn           Colon separated FQCN's.\n"+
            " [--tau= %f ]  Forward Latency or the Tau parameter in Float \n"+
            " [--format= [both|real|unit-delay]] By default both real macro lib and Unit delay \n"+
            "lib are generated in the current directory. The names of the files are real-macro.lib and \n"+
             "unit-delay-macro.lib.Specifying real or unit-delay lib would generate real and \n"+ 
            "unit-delay lib files respectively\n"
            );
        if (m != null && m.length() > 0)
            System.err.println( m );
        System.exit(1);
    }

    /** Produce a Proteus gate library. **/
    static public void main(String[] args) throws Exception {
        CommandLineArgs parsedArgs = new CommandLineArgsDefImpl(args);
        CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles(parsedArgs);
        CommandLineArgs cachedArgs =
            new CachingCommandLineArgs(argsWithConfigs);
        PedanticCommandLineArgs pedanticArgs =
            new PedanticCommandLineArgs(cachedArgs);
        CommandLineArgs theArgs = pedanticArgs;

        // get arguments
        int Strengths = 1;
        String  Task = "qdi";
        String  module = "";
        String castVersion = theArgs.getArgValue("cast-version", "2");
        // the real args
        String  castPath = theArgs.getArgValue("cast-path", null);
        String  timingFile = theArgs.getArgValue("timing-file",null);
        String  cellList = theArgs.getArgValue("cell", null);
        String  translate = theArgs.getArgValue("translate", "cast");
        String format = theArgs.getArgValue("format","both");
        String tau = theArgs.getArgValue("tau",null);
        if (cellList == null) {
            usage("No cells");
        }
        if (castPath == null) {
            usage("No cast-path");
        }
        if (timingFile == null) {
            usage("No timing-file");
        }
        if(tau == null && !format.equals("real") ){
            usage("No tau value specified");
        }

        // check obvious errors
        if (!pedanticArgs.pedanticOK(false, true)) {
            usage(pedanticArgs.pedanticString());
        }
        final CastFileParser castParser =
            CastCacheManager.getDefault().getCastFileParser(
                new FileSearchPath(castPath), castVersion,
                new StandardParsingOption(theArgs));
        // emit special gates
        if (cellList != null) {
            StringTokenizer t = new StringTokenizer(cellList,":");
            while (t.hasMoreTokens()) {
                String cellName = t.nextToken();
                File file;
                PrintStream ps;
                FileOutputStream fos_real= null,fos_UnitDelay=null;
                if((format.equals("both") || format.equals("real")) && tau != null){
                    try{
                        file = new File(cellName+"-real-macro.lib");
                        fos_real = new FileOutputStream(file);
                    }catch(FileNotFoundException e){
                        System.err.println("\nCould not create a real macro lib file ");
                    }
                }
                if(format.equals("both") || format.equals("unit-delay") ){
                    try{
                        file = new File(cellName+"-unit-delay-macro.lib");
                        fos_UnitDelay = new FileOutputStream(file);
                    }catch(FileNotFoundException e){
                        System.err.println("\nCould not create a unit delay macro lib file ");
                    }
                }
                if(tau == null){
                    new GenerateLib(Strengths, cellName,
                                    Task, false, false,
                                    timingFile, module, false, false, true,
                                    castParser, translate);

                }
                else{
                    if((format.equals("both") || format.equals("real")) && fos_real != null){
                        ps = new PrintStream(fos_real);
                        System.setOut(ps);
                        new GenerateLib(Strengths, cellName,
                                        Task, false, false,
                                        timingFile, module, false, false, true,
                                        castParser, translate, tau);
                    }

                    if(format.equals("both") || format.equals("unit-delay") ){
                        ps = new PrintStream(fos_UnitDelay);
                        System.setOut(ps);
                        new GenerateLib(Strengths, cellName,
                                        Task, false, false,
                                        timingFile, module, true, false, true,
                                        castParser, translate, tau);


                    }
                    else{
                         usage("Incorrect format specified");
                    }
                }
            }
            System.exit(0);
        }
    }

    /*************** Lots of hard-coded strings for Image Lib's *************/

    /** silly hard-coded crap for SCAN_TOK_BUF image **/
    String SCAN_TOK_BUF_Image =
        "  statetable(\"CK  L  RN  SN  LS  SE\",\"IQ0  IQ1\") {\n" +
        "    table : \"R   -  L   H   -   -  :  -    -  : L  -,\\\n" +
        "             R   -  H   L   -   -  :  -    -  : H  -,\\\n" +
        "             R   -  L   L   -   -  :  -    -  : H  -,\\\n" +
        "             R L/H  H   H   -   L  :  -    -  : L/H -,\\\n" +
        "             R   -  H   H  L/H  H  :  -    -  : - L/H,\\\n" +
        "            ~R   -  H   H   -   -  :  -    -  : N  N\";\n" +
        "  }\n" +
        "  test_cell() {\n" +
        "    pin(LS) {\n" +
        "      direction : input;\n" +
        "      signal_type : test_scan_in;\n" +
        "    }\n" +
        "    pin(SE) {\n" +
        "      direction : input;\n" +
        "      signal_type : test_scan_enable;\n" +
        "    }\n" +
        "    pin(RS) {\n" +
        "      direction : output;\n" +
        "      signal_type : test_scan_out;\n" +
        "      test_output_only : true;\n" +
        "    }\n" +
        "    pin(L) {\n" +
        "      direction : input;\n" +
        "    }\n" +
        "    pin(CK) {\n" +
        "      direction : input;\n" +
        "    }\n" +
        "    pin(RN) {\n" +
        "      direction : input;\n" +
        "    }\n" +
        "    pin(SN) {\n" +
        "      direction : input;\n" +
        "    }\n" +
        "    pin(R) {\n" +
        "      direction : output;\n" +
        "      function : \"IQ\";\n" +
        "    }\n" +
        "    ff(IQ,IQN) {\n" +
        "      clocked_on : \"CK\";\n" +
        "      next_state : \"L\";\n" +
        "      clear : \"!RN\";\n" +
        "      preset : \"!SN\";\n" +
        "      clear_preset_var1 : H;\n" +
        "      clear_preset_var2 : L;\n" +
        "    }\n" +
        "  }\n";

    /** silly hard-coded crap for SCAN_TOK_EDFF image **/
    String SCAN_TOK_EDFF_Image =
        "  statetable(\"CK  L  E  RN  SN  LS  SE\",\"IQ0  IQ1\") {\n" +
        "    table : \"R   -  -   L   H   -   -  :  -    -  : L  -,\\\n" +
        "             R   -  -   H   L   -   -  :  -    -  : H  -,\\\n" +
        "             R   -  -   L   L   -   -  :  -    -  : H  -,\\\n" +
        "             R L/H  H   H   H   -   L  :  -    -  : L/H -,\\\n" +
        "             R   -  L   H   H   -   L  : L/H   -  : L/H -,\\\n" +
        "             R   -  -   H   H  L/H  H  :  -    -  : - L/H,\\\n" +
        "            ~R   -  -   H   H   -   -  :  -    -  : N  N\";\n" +
        "  }\n" +
        "  test_cell() {\n" +
        "    pin(LS) {\n" +
        "      direction : input;\n" +
        "      signal_type : test_scan_in;\n" +
        "    }\n" +
        "    pin(SE) {\n" +
        "      direction : input;\n" +
        "      signal_type : test_scan_enable;\n" +
        "    }\n" +
        "    pin(RS) {\n" +
        "      direction : output;\n" +
        "      signal_type : test_scan_out;\n" +
        "      test_output_only : true;\n" +
        "    }\n" +
        "    pin(L) {\n" +
        "      direction : input;\n" +
        "    }\n" +
        "    pin(E) {\n" +
        "      direction : input;\n" +
        "    }\n" +
        "    pin(CK) {\n" +
        "      direction : input;\n" +
        "    }\n" +
        "    pin(RN) {\n" +
        "      direction : input;\n" +
        "    }\n" +
        "    pin(SN) {\n" +
        "      direction : input;\n" +
        "    }\n" +
        "    pin(R) {\n" +
        "      direction : output;\n" +
        "      function : \"IQ\";\n" +
        "    }\n" +
        "    ff(IQ,IQN) {\n" +
        "      clocked_on : \"CK\";\n" +
        "      next_state : \"E & L | !E & IQ\";\n" +
        "      clear : \"!RN\";\n" +
        "      preset : \"!SN\";\n" +
        "      clear_preset_var1 : H;\n" +
        "      clear_preset_var2 : L;\n" +
        "    }\n" +
        "  }\n";
}
