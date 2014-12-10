/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.lang.NumberFormatException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

import com.avlsi.util.container.SortingIterator;

import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.fast.ports.NodeType;
import com.avlsi.fast.ports.ChannelType;
import com.avlsi.fast.ports.ArrayType;
import com.avlsi.fast.ports.StructureType;
import com.avlsi.fast.ports.PortTypeInterface;

import com.avlsi.file.common.HierName;
import com.avlsi.io.FileSearchPath;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.tools.jauto.SubtypeOutput;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.PedanticCommandLineArgs;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.Pair;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast.impl.SemanticWrapperException;


/**
 * A class that generates a new cast description incorporating the cell
 * which was given as the --cell arg
 **/
public final class LeafSpec {
    /**
     * This class should not be instantiated.
     **/
    private LeafSpec() { }

    private static void usage( String m ) {
        System.err.print(
"Usage: leafspec\n" +
"   --cast-path=<path> (defaults to .)\n" +
"   --cast-version=[ 1 | 2 ] (defaults to 2)\n" +
"   --config=<config> (file containing options)\n" +
"   --cellName=<cell> (fully qualified cell name)\n" +
"   [ --wireLoad=<wire> (Load in m)\n" +
"   [ --wireDrive=<wire> (Drive load in m)\n" +
"   [ --translate=(cadence | gdsII) ] (for hierarchical tasks)\n" +
"   [ --output-cast=<file> ] (write result to file instead of standard out)\n" +
"   [ --module-name=<string> ] (the module name)\n" +
//"(see http://internal/tree/sw/cad/doc/specs/misc/LeafSpec.txt)\n");
"");
        if (m != null && m.length() > 0)
            System.err.print( m );
        System.exit(1);
    }

    private static void usage() {
        usage( null );
    }

    /**
     * Returns the string representation of an non-arrayed type.
     **/
    private static String nonArrayTypeString(final PortTypeInterface type) {
        if (type instanceof ChannelType) {
            final ChannelType channel = (ChannelType) type;
            final int width = channel.getWidth();
            if (channel.isArrayed()) {
                return channel.getTypeName() + "[" + width + "]";
            } else {
                assert width == 1;
                return channel.getTypeName();
            }
        } else if (type instanceof NodeType) {
            final NodeType node = (NodeType) type;
            final int width = node.getWidth();
            if (node.isArrayed()) {
                return "node[" + width + "]";
            } else {
                assert width == 1;
                return "node";
            }
        } else if (type instanceof StructureType) {
            final String tag = ((StructureType) type).getTag();
            if (tag == null) return type.toString();
            else return tag;
        } else {
            throw new RuntimeException("Unknown non-arrayed type "+
                    type.getClass()+"!");
        }
    }

    /**
     * Generate a correct port definition string
     * @param port is a port definition
     * @param cell is the cell in the overloaded function
     **/
    private static String portString (final PortDefinition pd,
            final CellInterface cell) {
        if (cell.isImpliedPort(pd.getName())) return null;
        return SubtypeOutput.portString(pd, false);
        /*
        int x;
        while ((x = result.indexOf("standard.channel.")) >= 0) {
            result.delete(x,x+17);
        }
        return result.toString();
        */
    }

    /**
     * Print out the new cast file wirelength directives associated
     * with them.
     **/
    private static void writeWrap(final CastFileParser cfp,
        double wireload, double wiredrive,
        final Cadencize cad, String module, final CellInterface cell,
        final BufferedWriter w ) throws IOException {

        w.write("/*");
        w.newLine();
        w.write("Type "+cell.getType());
        w.newLine();
        w.write("Filename "+cell.getFilename());
        w.newLine();
        String fqt = cell.getFullyQualifiedType().toString();
        w.write("Fully Qualified Type "+fqt);
        w.newLine();
        w.write("*/");
        w.newLine();
        w.write("module "+module+";");
        w.newLine();
        w.write("import "+fqt+";");
        w.newLine();
        w.write("define TEST_"+cell.getType()+" ()() {");
        w.newLine();
        w.write("  subcells {");
        w.newLine();
        final Map mp = CellUtils.markPorts(cell);

        final PartialExtract.LeafCallback cb = null;

        /**
          * write the port declarations from the leaf cell
        **/
        for (Iterator p = cell.getPortDefinitions(); p.hasNext(); ) {
            String s = portString ((PortDefinition) p.next(), cell);
            if (s != null) {
                w.write("    "+s+";");
                w.newLine();
            }
        }
        /**
         * Write the inverters which are the load and drive
        **/
        final CadenceInfo cinfo = cad.convert(cell);
        final AliasedMap ports = cinfo.getPortNodes();
        int   nodenr = 0;
        for (Iterator i = new SortingIterator(ports.getKeys());
             i.hasNext(); ) {
            final HierName canon = (HierName) i.next();
            final String s = canon.getCadenceString();
            if ( mp.containsKey(s) && !cell.isImpliedPort(s)) {
                int s1 = Integer.parseInt((mp.get(s)).toString());
                if (s1 == PortDefinition.IN) {
                    w.write("    INVDRIVE  _inst"+nodenr+" (_, "+s+");");
                    w.newLine();
                }
                else if (s1 == PortDefinition.OUT || s1 == PortDefinition.INOUT) {
                    w.write("    INVLOAD   _inst"+nodenr+" ("+s+", _);");
                    w.newLine();
                }
                else {
                }
                nodenr++;
            }
        }
        // write the leaf cell itself
        w.write("    "+cell.getType()+"   _dut(");
        int cnt=0;
        for (Iterator p = cell.getPortDefinitions(); p.hasNext(); ) {
            final PortDefinition pd = (PortDefinition) p.next();
            String sn = pd.getName();
            if (!cell.isImpliedPort(sn)) {
                if (cnt == 0) {
                    w.write(sn);
                }
                else {
                    w.write(","+sn);
                }
                cnt++;
            }
        }
        w.write(");");
        w.newLine();
        // Subcells Directives which are just the wire lengths
        w.write("    directives {");
        w.newLine();
        nodenr = 0;
        for (Iterator i = new SortingIterator(ports.getKeys());
             i.hasNext(); ) {
            final HierName canon = (HierName) i.next();
            final String s = canon.getCadenceString();
            if ( mp.containsKey(s) && !cell.isImpliedPort(s)) {
                int s1 = Integer.parseInt((mp.get(s)).toString());
                if (s1 == PortDefinition.IN) {
                    w.write("      wirelength("+s+")="+wiredrive+";");
                    w.newLine();
                }
                else if (s1 == PortDefinition.OUT || s1 == PortDefinition.INOUT) {
                    w.write("      wirelength("+s+")="+wireload+";");
                    w.newLine();
                }
                else {
                }
                nodenr++;
            }
        }
        w.write("    }"); // end directives
        w.newLine();
        w.write("  }"); // end subcells
        w.newLine();
        /** XXX
          * Do we want this?
        **/
        w.write("  directives {");
        w.newLine();
        w.write("    unimplementable=true;");
        w.newLine();
        w.write("  }");
        w.newLine();
        w.write("}"); // end cell
        w.newLine();
    }

    /**
      * Write the inverters, this writes both drive and load inverters
    **/
    private static void writeInverter (String name, BufferedWriter w)
        throws IOException {
        w.newLine();
        w.write("define "+name+" ()(node -I, +O) {");
        w.newLine();
        w.write("    prs {");
        w.newLine();
        w.write("        I => O-");
        w.newLine();
        w.write("    }");
        w.newLine();
        w.write("  directives {");
        w.newLine();
        w.write("    unimplementable=true;");
        w.newLine();
        w.write("    cellnonobservable=true;");
        w.newLine();
        w.write("  }");
        w.newLine();
        w.write("}");
        w.newLine();
    }

    public static void main(String[] args) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 
        
        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );
        
        final PedanticCommandLineArgs pedanticArgs = 
            new PedanticCommandLineArgs( cachedArgs );
        
        final CommandLineArgs theArgs = pedanticArgs;
        final String module = theArgs.getArgValue("module-name", "wrap.ttt");
        final String castRoot = theArgs.getArgValue("cast-path", ".");
        final String castVersion = theArgs.getArgValue("cast-version", "2");
        final String cellName = theArgs.getArgValue("cellName", null);
              String output = theArgs.getArgValue("output-cast", null);
        final String wireLoad = theArgs.getArgValue("wireLoad", "0.00001");
        final String wireDrive = theArgs.getArgValue("wireDrive", "0.00001");
        String widthDrivePlus = null;
        String widthDriveMinus = null;
        String widthLoad = null;
        if (theArgs.argExists("widthDrivePlus")) {
            widthDrivePlus = theArgs.getArgValue("widthDrivePlus", "1.0e-6");
        }
        else {
            widthDrivePlus = theArgs.getArgValue("widthDrive", "1.0e-6");
        }
        if (theArgs.argExists("widthDriveMinus")) {
            widthDriveMinus = theArgs.getArgValue("widthDriveMinus", "1.0e-6");
        }
        else {
            widthDriveMinus = theArgs.getArgValue("widthDrive", "1.0e-6");
        }
        widthLoad = theArgs.getArgValue("widthLoad", "1.0e-6");

        if (theArgs.argExists("version")) {
            System.out.println(
                com.avlsi.util.debug.VersionInfo.getVersionString(
                    LeafSpec.class));
        }

        pedanticArgs.argTag( "widthDrivePlus" );
        pedanticArgs.argTag( "widthDriveMinus" );
        pedanticArgs.argTag( "widthDrive" );

        if ( ! pedanticArgs.pedanticOK( false, true ) ) {
            usage( pedanticArgs.pedanticString() );
        }


        if (cellName == null) {
            usage("ERROR: You must specify a cell name.\n");
        }

        final CastFileParser castParser =
            new CastFileParser(new FileSearchPath(castRoot), castVersion);

        CellInterface rootCell = null;
        try {
            rootCell = castParser.getFullyQualifiedCell( cellName );
        } catch (CastSemanticException e) {
            StackTraceElement ste[] = e.getStackTrace();
            System.err.println("Cell "+cellName+" not found at "+
                ste[ste.length-1].toString());
            System.exit(1);
        }

        double wireload = 0.0;
        double wiredrive = 0.0;
        double widthload = 0.0;
        double widthdriveplus = 0.0;
        double widthdriveminus = 0.0;
        int    ecnt = 0;
        try {
            wireload = Double.parseDouble(wireLoad);
        } catch (NumberFormatException e) {
            System.err.println("wireLoad="+wireLoad+" number format exception!");
            ecnt++;
        }
        try {
            wiredrive = Double.parseDouble(wireDrive);
        } catch (NumberFormatException e) {
            System.err.println("wireDrive="+wireDrive+" number format exception!");
            ecnt++;
        }
        try {
            widthdriveplus = Double.parseDouble(widthDrivePlus);
        } catch (NumberFormatException e) {
            System.err.println("widthDrivePlus="+widthDrivePlus+" number format exception!");
            ecnt++;
        }
        try {
            widthdriveminus = Double.parseDouble(widthDriveMinus);
        } catch (NumberFormatException e) {
            System.err.println("widthDriveMinus="+widthDriveMinus+" number format exception!");
            ecnt++;
        }
        try {
            widthload = Double.parseDouble(widthLoad);
        } catch (NumberFormatException e) {
            System.err.println("widthLoad="+widthLoad+" number format exception!");
            ecnt++;
        }
        if (ecnt > 0) {
            System.err.println("Aborting...");
            System.exit (1);
        }
        if (output != null) {
            File fp = new File(output);
            fp = fp.getAbsoluteFile();
            File dir = fp.getParentFile();
            if (! dir.exists() && ! dir.mkdirs()) {
                System.err.println("Cannot create and/or write to "+
                    dir.toString()+"!");
                System.exit(1);
            }
            if (! dir.canWrite()) {
                System.err.println("Cannot create and/or write to "+
                    dir.toString()+"!");
                System.exit(1);
            }
        }
        final Writer w = output == null ?
            new OutputStreamWriter(System.out) :
            new FileWriter(output);

        final BufferedWriter wr = new BufferedWriter( w ) ;
        writeWrap( castParser, wireload, wiredrive, new Cadencize(true),
            module, rootCell, wr );
        writeInverter("INVLOAD", wr );
        writeInverter("INVDRIVE", wr);
        wr.close();
    }
}
