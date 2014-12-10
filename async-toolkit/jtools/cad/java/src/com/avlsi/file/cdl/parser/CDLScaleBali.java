/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.parser;

import java.lang.Double;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;
import java.util.Map;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import antlr.RecognitionException;
import antlr.TokenStreamException;

import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.CastFile;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cell.CellInterface;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.NetlistBlock;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.io.FileSearchPath;
import com.avlsi.io.IndentWriter;
import com.avlsi.io.NullWriter;
import com.avlsi.util.cmdlineargs.*;
import com.avlsi.util.cmdlineargs.defimpl.*;
import com.avlsi.util.container.StringContainerIterator;
import com.avlsi.util.text.PrintfFormat;

public final class CDLScaleBali extends CDLFactoryFilter {
    private final double mMosWidthScaleFactor;
    private final double mMosLengthScaleFactor;
    private static final Pattern GATE_REGEX =
        Pattern.compile("(.*)\\.\\d(\\(.*\\))?");
    private static final double GRID = 0.005e-6;
    
    private final static class ScalingToken extends CDLLexer.InfoToken {
        private final CDLLexer.InfoToken mWrapped;
        private final double mScale;
        private final boolean isWidth;
        
        public ScalingToken( final CDLLexer.InfoToken wrapped, double scale ) {
            this(wrapped, scale, false);
        }

        public ScalingToken( final CDLLexer.InfoToken wrapped, double scale,
                             boolean width) {
            super( 0, wrapped.getText() );
            mWrapped = wrapped;
            mScale = scale;
            isWidth = width;
        }

        public String getText( Environment env ) {
            final Double value = getValue( env );
           
            if ( value != null ) {
                setText(value.toString());
                return value.toString();
            }
            else {
                //return Double.toString( mScale ) + "( " + mWrapped.getText( env ) + " )";
                setText("'" + Double.toString(mScale) + "* (" + mWrapped.getText() + ")'");
                return "'" + Double.toString(mScale) + "* (" + mWrapped.getText() + ")'";
            }
        }

        public Double getValue( Environment env ) {
            final Double wrappedValue = mWrapped.getValue( env );
            if ( wrappedValue != null ) {
                if (getText().indexOf("W") == -1 &&
                    getText().indexOf("w") == -1 &&
                    getText().indexOf("L") == -1 &&
                    getText().indexOf("l") == -1 ) {
                    if (isWidth) {
                        double x = 0.6 * wrappedValue.doubleValue();
                        if (x < 0.12e-6) x = 0.12e-6;
                        setText((new Double(x)).toString());
                    } else {
                        if (Math.abs(wrappedValue.doubleValue() - 0.13e-6) < 0.1e-8) {
                            setText("0.06u");
                        } else {
                            setText((new Double(0.6 * wrappedValue.doubleValue() - 0.018e-6)).toString());
                        }
                    }
                }
                return new Double( 0.6 * wrappedValue.doubleValue() );
            }
            else {
                return null;
            }
        }

    }

    public CDLScaleBali(final double mosWidthScaleFactor,
                        final double mosLengthScaleFactor,
                        final CDLFactoryInterface chained ) {
        super(chained);
        mMosWidthScaleFactor = mosWidthScaleFactor;
        mMosLengthScaleFactor = mosLengthScaleFactor;
        
    }
    
    public void makeTransistor(HierName name, String type, HierName ns,
                               HierName nd, HierName ng, HierName nb,
                               CDLLexer.InfoToken w, CDLLexer.InfoToken l,
                               Map parameters, Environment env) {
        inner.makeTransistor( name,
                              type,
                              ns,
                              nd,
                              ng,
                              nb,
                              new ScalingToken( w, mMosWidthScaleFactor, true ),
                              new ScalingToken( l, mMosLengthScaleFactor, false ),
                              parameters,
                              env );
    }

    private double scaleParam(String cell, String param, double val) {
        if ((param.equals("NL") || param.equals("PL")) &&
            val == 0.13e-6) {
            return 0.06e-6;
        } else {
            val = val * 0.6;
            if (param.indexOf("W") != -1 || param.indexOf("w") != -1) {
                if (val < 0.12e-6) val = 0.12e-6;
            } else if (param.indexOf("L") != -1 || param.indexOf("l") != -1) {
                val = val - 0.018e-6;
            }
            return val;
        }
    }

    private static final PrintfFormat FORMAT = new PrintfFormat("%.3fu");
    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters, Environment env) {
        final Map resolved =
            CDLInterfaceSimplifier.resolveCallParameter(parameters, env);
        for (Iterator i = resolved.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final String key = (String) entry.getKey();
            final Double val = (Double) entry.getValue();
            final double scaled = scaleParam(subName, key, val.doubleValue());
            entry.setValue(
                new CDLLexer.SimpleToken(
                    new Double(scaled),
                    FORMAT.sprintf(Math.round(scaled/GRID)*GRID*1e6)));
        }
        final Matcher m = GATE_REGEX.matcher(subName);
        if (m.matches()) {
            final String baseName = m.group(1);
            subName = baseName + ".0(" +
                      (baseName.startsWith("stack") ? "1" : "1,1") +
                      ")";
        }
        inner.makeCall( name, subName, args, resolved, env );
    }

    private static void writeSpec(final File outputDir,
                                  final CellInterface cell)
        throws IOException, RecognitionException, TokenStreamException {
        final String type = cell.getFullyQualifiedType();
        final String castFile = type.replace('.', File.separatorChar) + ".cast";
        final File outCast = new File(outputDir, castFile);
        outCast.getParentFile().mkdirs();
        final IndentWriter iw =
            new IndentWriter(new BufferedWriter(new FileWriter(outCast)));
        final String module = cell.getModuleName();
        final String subtype = cell.getType();
        iw.write("module " + module + ";\n");
        iw.write("define \"" + subtype +
                 "\"() <+ standard.process.scaled_layout_cell" +
                 " <: " + module + " {\n");
        iw.nextLevel();
            iw.write("netlist {\n");
            iw.nextLevel();
                final CDLScaleBali factory =
                    new CDLScaleBali(0.6, 0.6, new CDLFactoryEmitter(iw));
                final NetlistBlock block =
                    (NetlistBlock) cell.getBlockInterface()
                                       .iterator(BlockInterface.NETLIST).next();
                block.getCDLTemplate().execute(factory);
            iw.prevLevel();
            iw.write("}\n");
            iw.write("directives {\n");
            iw.nextLevel();
                iw.write("fixed_size = true;\n");
            iw.prevLevel();
            iw.write("}\n");
        iw.prevLevel();
        iw.write("}\n");
        iw.close();
    }

    private static void usage() {
        System.out.print(
            "Usage: scalebali --cast-path=<path> (130 nm CAST path)\n" +
            "                 --output-dir=<path> (65 nm spec directory)\n" +
            "                 [--verbose (print stack traces)\n]" +
            "                 cell [cell ...]\n");
    }

    public static void main(String[] args) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl(args);
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles(parsedArgs); 
        
        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs(argsWithConfigs);
        
        final PedanticCommandLineArgs pedanticArgs = 
            new PedanticCommandLineArgs(cachedArgs);
        
        final CommandLineArgs theArgs = pedanticArgs;

        final String castPath = theArgs.getArgValue("cast-path", null);
        final String outputPath = theArgs.getArgValue("output-dir", null);
        final boolean verbose = theArgs.argExists("verbose");
        int errors = 0;
        if (castPath == null) {
            System.err.println("--cast-path must be specified.");
            ++errors;
        }
        if (outputPath == null) {
            System.err.println("--output-dir must be specified.");
            ++errors;
        }
        if (errors > 0) {
            usage();
            System.exit(1);
        }
        final CastFileParser cfp =
            new CastFileParser(new FileSearchPath(castPath), "2");
        final File outputDir = new File(outputPath);
        for (StringContainerIterator i = theArgs.nonParsedArgumentsIterator();
             i.hasNext(); ) {
            final String name = i.next();
            try {
                final CellInterface cell = cfp.getFullyQualifiedCell(name);
                writeSpec(outputDir, cell);
            } catch (Exception e) {
                System.err.println("Error processing " + name +
                                   ": " + e.getMessage() + "; skipping");
                if (verbose) {
                    e.printStackTrace();
                }
            }
        }
    }
}
