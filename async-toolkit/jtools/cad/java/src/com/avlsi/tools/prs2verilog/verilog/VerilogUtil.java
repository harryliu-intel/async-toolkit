package com.avlsi.tools.prs2verilog.verilog;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class VerilogUtil {
    /**
     * A collection of Verilog keywords, as defined by IEEE Std 1364-2001
     * Version C (Annex B, page 787).
     **/
    public final static Set/*<String>*/ RESERVED_WORDS =
        Collections.unmodifiableSet(new HashSet(Arrays.asList(new Object[] {
            "always",
            "and",
            "assign",
            "automatic",
            "begin",
            "buf",
            "bufif0",
            "bufif1",
            "case",
            "casex",
            "casez",
            "cell",
            "cmos",
            "config",
            "deassign",
            "default",
            "defparam",
            "design",
            "disable",
            "edge",
            "else",
            "end",
            "endcase",
            "endconfig",
            "endfunction",
            "endgenerate",
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
            "generate",
            "genvar",
            "highz0",
            "highz1",
            "if",
            "ifnone",
            "incdir",
            "include",
            "initial",
            "inout",
            "input",
            "instance",
            "integer",
            "join",
            "large",
            "liblist",
            "library",
            "localparam",
            "macromodule",
            "medium",
            "module",
            "nand",
            "negedge",
            "nmos",
            "nor",
            "noshowcancelled",
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
            "pulsestyle_ondetect",
            "pulsestyle_onevent",
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
            "showcancelled",
            "signed",
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
            "unsigned",
            "use",
            "vectored",
            "wait",
            "wand",
            "weak0",
            "weak1",
            "while",
            "wire",
            "wor",
            "xnor",
            "xor"
        })));

    /**
     * A collection of Verilog keywords added to IEEE Std 1364-2001
     * for SystemVerilog 3.1a (Annex B, page 488).
     **/
    public final static Set/*<String>*/ SV_RESERVED_WORDS =
        Collections.unmodifiableSet(new HashSet(Arrays.asList(new Object[] {
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
            "endclass",
            "endclocking",
            "endgroup",
            "endinterface",
            "endpackage",
            "endprogram",
            "endproperty",
            "endsequence",
            "enum",
            "expect",
            "export",
            "extends",
            "extern",
            "final",
            "first_match",
            "foreach",
            "forkjoin",
            "iff",
            "ignore_bins",
            "illegal_bins",
            "import",
            "inside",
            "int",
            "interface",
            "intersect",
            "join_any",
            "join_none",
            "local",
            "logic",
            "longint",
            "matches",
            "modport",
            "new",
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
            "return",
            "sequence",
            "shortint",
            "shortreal",
            "solve",
            "static",
            "string",
            "struct",
            "super",
            "tagged",
            "this",
            "throughout",
            "timeprecision",
            "timeunit",
            "type",
            "typedef",
            "union",
            "unique",
            "var",
            "virtual",
            "void",
            "wait_order",
            "wildcard",
            "with",
            "within"
        })));

    /**
     * This class should not be instantiated.
     **/
    private VerilogUtil() { }

    /**
     * Pattern that matches a Verilog simple identifier, as defined by as
     * defined by IEEE Std 1364-2001 Version C (section 2.7, page 12).
     **/
    private static final /*@ non_null @*/ Pattern SIMPLE_IDENTIFIER =
        Pattern.compile("[a-zA-Z_][a-zA-Z0-9_$]*");

    /**
     * Return whether a token is one of the reserved keywords in Verilog 2001
     * or SystemVerilog 3.1a.
     *
     * @param s token
     * @return <code>true</code> if <code>s</code> is a reserved keyword, or
     * <code>false</code> otherwise.
     **/
    public static boolean isKeyword(final /*@ non_null @*/ String s) {
        return RESERVED_WORDS.contains(s) || SV_RESERVED_WORDS.contains(s);
    }

    /**
     * Return whether an identifier would need to be escaped in Verilog.  An
     * identifier needs to be escaped if it contains special characters, or if
     * it is identical to a Verilog keyword.
     *
     * @param s Identifier
     * @return <code>true</code> if <code>s</code> must be escaped, or
     * <code>false</code> otherwise.
     **/
    public static boolean needsEscaping(final /*@ non_null @*/ String s) {
        return isKeyword(s) || !SIMPLE_IDENTIFIER.matcher(s).matches();
    }

    /**
     * Escape an identifier in Verilog.
     *
     * @param s Identifier
     * @return The identifier escaped in Verilog, by adding a leading backslash
     * and a trailing space.
     **/
    public static /*@ non_null @*/ String escape
            (final /*@ non_null @*/ String s) {
        return "\\" + s + " ";
    }

    /**
     * Escape an identifier in Verilog if it contains any characters
     * that need escaping.
     * Equivalent to <code>needsEscaping(s) ? escape(s) : s</code>.
     *
     * @param s Identifier
     * @return The identifier escaped in Verilog escaping is needed,
     *         otherwise <code>s</code>.
     **/
    public static /*@ non_null @*/ String escapeIfNeeded
            (final /*@ non_null @*/ String s) {
        return needsEscaping(s) ? escape(s) : s;
    }
}
