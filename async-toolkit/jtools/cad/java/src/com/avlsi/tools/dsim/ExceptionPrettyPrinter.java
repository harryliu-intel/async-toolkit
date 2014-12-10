/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.dsim;

import java.io.BufferedReader;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cell.NoSuchEnvironmentException;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import com.avlsi.io.Printable;
import antlr.RecognitionException;
import antlr.TokenStreamRecognitionException;
import com.avlsi.cast.impl.SemanticWrapperException;
import com.avlsi.util.text.StringUtil;
import com.avlsi.io.WrapPrintStream;
import com.avlsi.io.WrapPrintWriter;

/**
 * Print DSim exceptions nicely.  See bug 1843.
 * @author Patrick Pelletier
 */
public class ExceptionPrettyPrinter {
    /**
     * This class should not be instantiated.
     **/
    private ExceptionPrettyPrinter() { }

    private static class ExceptionInfo {
	String message;
	String filename;
	int line;
	int column;
        Throwable exception;
        final Printable out;

        ExceptionInfo(Printable out) {
            this.out = out;
        }
    }

    private static void getPositionInfo(Throwable e, ExceptionInfo ei) {
	if (e instanceof CastSemanticException) {
	    CastSemanticException cse = (CastSemanticException) e;
	    ei.filename = cse.getFileName();
	    ei.line = cse.getLineNumber();
	    ei.column = cse.getColumnNumber();
	} else if (e instanceof RecognitionException) {
	    RecognitionException re = (RecognitionException) e;
	    ei.filename = re.getFilename();
	    ei.line = re.getLine();
	    ei.column = re.getColumn();
	}
    }

    private static void extractExceptionInfo(Throwable e, ExceptionInfo ei) {
        if (e instanceof TokenStreamRecognitionException) {
            e = ((TokenStreamRecognitionException) e).recog;
        }
	getPositionInfo(e, ei);
        if (e.getMessage() != null &&
            !(e instanceof ArrayIndexOutOfBoundsException /* bug 3717 */)) {
            if (e instanceof SemanticWrapperException) {
                ei.message = ((SemanticWrapperException)e).getMessageOnly();
                if (ei.message.startsWith("Error instantiating"))
                    printThisException(ei);
            } else {
                if (ei.exception != null &&
                    (ei.exception.getClass().isAssignableFrom(e.getClass()) ||
                     e.getClass().isAssignableFrom(ei.exception.getClass()))) {
                    // glue together exception messages if exceptions are of
                    // the same type or if one extends the other; this is a
                    // somewhat common scenario, where a function throws an
                    // exception, and the function that called it rethrows the
                    // exception, but adding additional information that's not
                    // available to the function throwing the original
                    // exception
                    ei.message = ei.message + ": " + e.getMessage();
                } else {
                    ei.message = e.getMessage();
                }
            }
        }
	Throwable cause = e.getCause();
        ei.exception = e;
	if (cause != null)
	    extractExceptionInfo(cause, ei);
    }

    private static void printThisException(ExceptionInfo ei) {
	File file = null;

        if (ei.filename != null && ei.filename.length() > 0 &&
            ei.filename.charAt(0) == '<') {
            // this fixes bug 6146
            ei.out.println("top-level: " + ei.message);
        } else {
            file = (ei.filename == null ? null : new File(ei.filename));
            String shortname = ei.filename;
            if (file != null && file.canRead())
                shortname = file.getPath();
            else
                file = null;
            ei.out.println(shortname + ": " + ei.line + ": " + ei.message);
        }

	if (file != null) {
	    try {
		BufferedReader r = new BufferedReader(new FileReader(file));
		String line = "";
		int n = ei.line;
		while (n-- > 0)
		    line = r.readLine();
		r.close();
		ei.out.println(line);
                if (ei.column > 0) {
                    ei.out.println(StringUtil.repeatString(" ", ei.column-1)
                                       + "^");
                }
	    } catch (IOException ignore) {
	    }
	}
    }

    /**
     * Similar to printException, except that you can just supply your
     * own message, rather than passing a real Exception.
     * @param message    your error message
     * @param filename   source file that the error occurred in
     * @param line       source line number that the error occurred on
     * @param column     source column number that the error occurred at
     *                       (1-based, 0 means no column)
     * @param out        stream to print the message to
     */
    public static void prettyMessage(String message, String filename,
                                     int line, int column, Printable out) {
        ExceptionInfo ei = new ExceptionInfo(out);
        ei.message = message;
        ei.filename = filename;
        ei.line = line;
        ei.column = column;
        printThisException(ei);
    }

    /** Pretty-print exception to standard output */
    public static void printException(CastSemanticException e) {
        printException(e, System.out);
    }
    
    /** Pretty-print exception to specified Printable */
    public static void printException(CastSemanticException e,
                                      Printable out) {
	ExceptionInfo ei = new ExceptionInfo(out);
	extractExceptionInfo(e, ei);
        printThisException(ei);
    }

    /**
     * Convenience routine to pretty-print to standard out a
     * NoSuchEnvironmentException, which may wrap a CastSemanticException.
     **/
    public static void printException(NoSuchEnvironmentException e) {
        printException(e, System.out);
    }

    /**
     * Convenience routine to pretty-print to the specified Printable a
     * NoSuchEnvironmentException, which may wrap a CastSemanticException.
     **/
    public static void printException(NoSuchEnvironmentException e,
                                      Printable out) {
        final Throwable ce = e.getCause();
        if (ce == null) {
            out.println("Environment " + e.getEnvironmentName() +
                        " does not exist in " + e.getCellName());
        } else if (ce instanceof CastSemanticException) {
            printException((CastSemanticException) ce, out);
        }
    }

    // overloaded versions to take PrintStream or PrintWriter
    public static void prettyMessage(String message, String filename,
                                     int line, int column, PrintStream out) {
        prettyMessage(message, filename, line, column,
                      new WrapPrintStream(out));
    }

    public static void printException(CastSemanticException e,
                                      PrintStream out) {
        printException(e, new WrapPrintStream(out));
    }

    public static void printException(NoSuchEnvironmentException e,
                                      PrintStream out) {
        printException(e, new WrapPrintStream(out));
    }

    public static void prettyMessage(String message, String filename,
                                     int line, int column, PrintWriter out) {
        prettyMessage(message, filename, line, column,
                      new WrapPrintWriter(out));
    }

    public static void printException(CastSemanticException e,
                                      PrintWriter out) {
        printException(e, new WrapPrintWriter(out));
    }

    public static void printException(NoSuchEnvironmentException e,
                                      PrintWriter out) {
        printException(e, new WrapPrintWriter(out));
    }
}
