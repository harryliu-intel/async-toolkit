/*
 * Copyright 2009 Fulcrum Microsystems. All rights reserved.
 * 
 * $Id$
 * 
 * Created on Feb 6, 2009
 * Author zloh
 */

package com.fulcrummicro.hw.verification.chip.mgmt.registers;

import java.util.Arrays;
import java.util.HashSet;

public class Util {
    
    public static boolean isPowerOf2(int x) {
        for (int i=0; i<31; i++) {
            if (x == (1<<i)) return true;
        }
        return false;
    }
    
    public static int log2(int x) {
        for (int i=0; i<31; i++) {
            if (x <= (1<<i)) {
                return i;
            }
        }
        System.out.printf("ERROR log2 does not handle input=%d\n",x);
        return 32;
    }
    
    public static int expandToPowerOf2Mask(int x) {
        return (int) (Math.pow(2, log2(x)) - 1);
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
    // TODO Auto-generated method stub

    }
    
    public static String escapeRdlDesc(String d) {
        if (d == null)
            return "";
        return d.replace('"', '\'');
    }

    public static HashSet<String> systemverilogReservedWords = new HashSet<String>(Arrays.asList(
            // Reserved Words in Verilog HDL version 1995:
            "always",
            "and",
            "assign",
            "begin",
            "buf",
            "bufif0",
            "bufif1",
            "case",
            "casex",
            "casez",
            "cmos",
            "deassign",
            "default",
            "defparam",
            "disable",
            "edge",
            "else",
            "end",
            "endcase",
            "endfunction",
            "endmodule",
            "endprimitive",
            "endspecify",
            "endtable",
            "endtask",
            "event",
            "for",
            "force",
            "forever",
            "fork",
            "function",
            "highz0",
            "highz1",
            "if",
            "ifnone",
            "initial",
            "inout",
            "input",
            "integer",
            "join",
            "large",
            "macromodule",
            "medium",
            "module",
            "nand",
            "negedge",
            "nmos",
            "nor",
            "not",
            "notif0",
            "notif1",
            "or",
            "output",
            "parameter",
            "pmos",
            "posedge",
            "primitive",
            "pull0",
            "pull1",
            "pulldown",
            "pullup",
            "rcmos",
            "real",
            "realtime",
            "reg",
            "release",
            "repeat",
            "rnmos",
            "rpmos",
            "rtran",
            "rtranif0",
            "rtranif1",
            "scalared",
            "small",
            "specify",
            "specparam",
            "strong0",
            "strong1",
            "supply0",
            "supply1",
            "table",
            "task",
            "time",
            "tran",
            "tranif0",
            "tranif1",
            "tri",
            "tri0",
            "tri1",
            "triand",
            "trior",
            "trireg",
            "vectored",
            "wait",
            "wand",
            "weak0",
            "weak1",
            "while",
            "wire",
            "wor",
            "xnor",
            "xor",

            // Reserved Words in Verilog HDL version 2001:
            "automatic",
            "cell",
            "config",
            "endconfig",
            "endgenerate",
            "generate",
            "genvar",
            "incdir",
            "include",
            "instance",
            "liblist",
            "library",
            "localparam",
            "noshowcancelled",
            "pulsestyle_ondetect",
            "pulsestyle_onevent",
            "showcancelled",
            "signed",
            "unsigned",
            "use",

            // Reserved Words in SystemVerilog:
            "accept_on",
            "alias",
            "always_comb",
            "always_ff",
            "always_latch",
            "assert",
            "assume",
            "before",
            "bind",
            "bins",
            "binsof",
            "bit",
            "break",
            "byte",
            "chandle",
            "checker",
            "class",
            "clocking",
            "const",
            "constraint",
            "context",
            "continue",
            "cover",
            "covergroup",
            "coverpoint",
            "cross",
            "dist",
            "do",
            "endchecker",
            "endclass",
            "endclocking",
            "endgroup",
            "endinterface",
            "endpackage",
            "endprogram",
            "endproperty",
            "endsequence",
            "enum",
            "eventually",
            "expect",
            "export",
            "extends",
            "extern",
            "final",
            "first_match",
            "foreach",
            "forkjoin",
            "global",
            "iff",
            "ignore_bins",
            "illegal_bins",
            "implies",
            "import",
            "inside",
            "int",
            "interface",
            "intersect",
            "join_any",
            "join_none",
            "let",
            "local",
            "logic",
            "longint",
            "matches",
            "modport",
            "new",
            "nexttime",
            "null",
            "package",
            "packed",
            "priority",
            "program",
            "property",
            "protected",
            "pure",
            "rand",
            "randc",
            "randcase",
            "randsequence",
            "ref",
            "reject_on",
            "restrict",
            "return",
            "s_always",
            "s_eventually",
            "s_nexttime",
            "s_until",
            "s_until_with",
            "sequence",
            "shortint",
            "shortreal",
            "solve",
            "static",
            "string",
            "strong",
            "struct",
            "super",
            "sync_accept_on",
            "sync_reject_on",
            "tagged",
            "this",
            "throughout",
            "timeprecision",
            "timeunit",
            "type",
            "typedef",
            "union",
            "unique",
            "unique0",
            "until",
            "until_with",
            "untypted",
            "var",
            "virtual",
            "void",
            "wait_order",
            "weak",
            "wildcard",
            "with",
            "within",
            
            // additional synopsys reserved words
            "soft"
            ));

}
